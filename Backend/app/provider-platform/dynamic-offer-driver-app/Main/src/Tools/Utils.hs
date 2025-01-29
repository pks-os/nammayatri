module Tools.Utils where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.SearchRequest as SR
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Maps.HasCoordinates (getCoordinates)
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Distance (metersToHighPrecMeters)
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types as LYT
import Tools.Constants

-- TODO test (and maybe move to Lib)
convertTags :: [LYT.TagNameValue] -> A.Value
convertTags input = A.object $ mapMaybe toObject pairs
  where
    pairs = map (T.splitOn "#" . LYT.getTagNameValue) input
    toObject (name : value : _xs) = Just $ do
      -- TODO we can revert this change since we use newtype without expiry
      -- expiredAt ignored here, check is it correct?
      let valueArr = T.splitOn "&" value
      case valueArr of
        [element] -> (A.fromText $ T.strip name :: A.Key) A..= fromMaybe A.Null (textToMaybeValue (T.strip element) :: Maybe A.Value)
        elements -> do
          let jsonValues = map A.String elements
          (A.fromText $ T.strip name :: A.Key) A..= A.Array (Vector.fromList jsonValues)
    toObject [name] = Just $ (A.fromText $ T.strip name :: A.Key) A..= A.Null
    toObject [] = Nothing -- should never happen

accessKey :: T.Text -> A.Value -> Maybe A.Value
accessKey keyValue (A.Object obj) = KM.lookup (A.fromText keyValue) obj
accessKey _ _ = Nothing

isDropInsideThreshold :: DB.Booking -> DTConf.TransporterConfig -> LatLong -> Bool
isDropInsideThreshold booking thresholdConfig currLoation = do
  let dropLocThreshold = metersToHighPrecMeters thresholdConfig.dropLocThreshold
      locationDiff = maybe 0 (\toLocation -> abs $ distanceBetweenInMeters (getCoordinates toLocation) currLoation) booking.toLocation
   in locationDiff <= dropLocThreshold

isValidRide :: DR.Ride -> Bool
isValidRide ride = maybe True (elem validRideTag) ride.rideTags -- TODO: How to remove hardcode string

isRiderEligibleForCabUpgrade :: SR.SearchRequest -> Bool
isRiderEligibleForCabUpgrade searchReq = maybe False (elem riderEligibleForCabUpgradeTag) searchReq.searchTags
