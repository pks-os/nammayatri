module Lib.Yudhishthira.Tools.Utils where

-- TODO export list
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as DBL
import qualified Data.String.Conversions as CS
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Lazy as DTE
import qualified Data.Text.Lazy.Encoding as DTLE
import JsonLogic
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT

mandatoryChakraFields :: [Text]
mandatoryChakraFields = [userIdField]

userIdField :: Text
userIdField = "userId"

getUserIdsQueryName :: Text
getUserIdsQueryName = "getUserIds"

decodeTextToValue :: Text -> Either String Value
decodeTextToValue text =
  let byteString = DTLE.encodeUtf8 $ DTE.fromStrict text
   in A.eitherDecode byteString

runJsonLogic :: (MonadFlow m) => Value -> Text -> m A.Value
runJsonLogic data' ruleText = do
  let eitherRule = decodeTextToValue ruleText
  rule <-
    case eitherRule of
      Right rule -> return rule
      Left err -> throwError $ InternalError ("Unable to decode rule:" <> show err)
  jsonLogic rule data'

runLogics :: (MonadFlow m, ToJSON a) => [A.Value] -> a -> m LYT.RunLogicResp
runLogics logics data_ = do
  let logicData = A.toJSON data_
  logDebug $ "logics- " <> show logics
  logDebug $ "logicData- " <> CS.cs (A.encode logicData)
  let startingPoint = LYT.RunLogicResp logicData []
  foldlM
    ( \acc logic -> do
        let result = jsonLogicEither logic acc.result
        res <-
          case result of
            Left err -> do
              logError $ "Got error: " <> show err <> " while running logic: " <> CS.cs (A.encode logics)
              pure $ LYT.RunLogicResp acc.result (acc.errors <> [show err])
            Right res -> pure $ LYT.RunLogicResp res acc.errors
        logDebug $ "logic- " <> (CS.cs . A.encode $ logic)
        logDebug $ "json logic result - " <> (CS.cs . A.encode $ res)
        return res
    )
    startingPoint
    logics

decodeText :: Text -> Maybe A.Value
decodeText txt = A.decode (DBL.fromStrict . DTE.encodeUtf8 $ txt)

-- Function to convert Text to Maybe Value
textToMaybeValue :: Text -> Maybe A.Value
textToMaybeValue txt =
  case decodeText txt of
    Just value -> Just value
    Nothing -> decodeText (T.concat ["\"", txt, "\""])

-- TODO also check other places in code, where assignedAt required, try to use single function for show and parse
mkTagNameValueExpiry ::
  LYT.TagName ->
  LYT.TagValue ->
  Maybe Hours ->
  UTCTime ->
  LYT.TagNameValueExpiry
mkTagNameValueExpiry (LYT.TagName tagName) tagValue mbValidity now = do
  let mbExpiredAt = mbValidity <&> (\validity -> addUTCTime (3600 * fromIntegral validity) now) -- TODO test
  let showTagValue = case tagValue of
        LYT.TextValue tagValueText -> tagValueText
        LYT.NumberValue tagValueDouble -> show tagValueDouble
        LYT.ArrayValue tagValueArray -> T.intercalate "&" tagValueArray
  LYT.TagNameValueExpiry $ tagName <> "#" <> showTagValue <> maybe "" (\expiredAt -> "#" <> show expiredAt) mbExpiredAt

mkTagNameValue ::
  LYT.TagName ->
  LYT.TagValue ->
  LYT.TagNameValue
mkTagNameValue (LYT.TagName tagName) tagValue = do
  let showTagValue = case tagValue of
        LYT.TextValue tagValueText -> tagValueText
        LYT.NumberValue tagValueDouble -> show tagValueDouble
        LYT.ArrayValue tagValueArray -> T.intercalate "&" tagValueArray
  LYT.TagNameValue $ tagName <> "#" <> showTagValue

-- TODO test this
addTagExpiry ::
  LYT.TagNameValue ->
  Maybe Hours ->
  UTCTime ->
  LYT.TagNameValueExpiry
addTagExpiry (LYT.TagNameValue txt) (Just validity) now = do
  let expiredAt = addUTCTime (3600 * fromIntegral validity) now
  LYT.TagNameValueExpiry $ case T.splitOn "#" txt of
    (tagName : tagValue : _oldExpiredAt : xs) -> T.intercalate "#" (tagName : tagValue : show expiredAt : xs)
    [tagName, tagValue] -> T.intercalate "#" [tagName, tagValue, show expiredAt]
    [tagName] -> T.intercalate "#" [tagName, "", show expiredAt]
    [] -> T.intercalate "#" ["", "", show expiredAt] -- should never happen
addTagExpiry (LYT.TagNameValue txt) Nothing _now = LYT.TagNameValueExpiry txt

removeTagExpiry ::
  LYT.TagNameValueExpiry ->
  LYT.TagNameValue
removeTagExpiry (LYT.TagNameValueExpiry txt) = do
  LYT.TagNameValue $ case T.splitOn "#" txt of
    (tagName : tagValue : _oldExpiredAt : xs) -> T.intercalate "#" (tagName : tagValue : "" : xs)
    _ -> txt

-- helper class for reduce boilerplate
class HasTagNameValue tag where
  convertToTagNameValue :: tag -> LYT.TagNameValue

instance HasTagNameValue LYT.TagNameValue where
  convertToTagNameValue = identity

instance HasTagNameValue LYT.TagNameValueExpiry where
  convertToTagNameValue = removeTagExpiry

compareTagNameValue :: (HasTagNameValue tag1, HasTagNameValue tag2) => tag1 -> tag2 -> Bool
compareTagNameValue tag1 tag2 = convertToTagNameValue tag1 == convertToTagNameValue tag2

elemTagNameValue :: (HasTagNameValue tag1, HasTagNameValue tag2) => tag1 -> [tag2] -> Bool
elemTagNameValue tag tags = convertToTagNameValue tag `elem` (convertToTagNameValue <$> tags)

-- compareTagNameValueExpiry :: LYT.TagNameValueExpiry -> LYT.TagNameValueExpiry -> Bool
-- compareTagNameValueExpiry tag1 tag2 = tag1.getTagNameValueExpiry == tag2.getTagNameValueExpiry
