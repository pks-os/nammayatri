module Tools.Constants where

import qualified Lib.Yudhishthira.Types as LYT

-- FIXME its safer to use LYT.mkTagNameValue (LYT.TagName "ValidRide") (LYT.TextValue "Yes")
validRideTag :: LYT.TagNameValue
validRideTag = LYT.TagNameValue "ValidRide#Yes"

riderEligibleForCabUpgradeTag :: LYT.TagNameValue
riderEligibleForCabUpgradeTag = LYT.TagNameValue "RiderEligibleForCabUpgrade#Yes"

validDriverCancellation :: LYT.TagNameValue
validDriverCancellation = LYT.TagNameValue "DriverCancellation#Valid"

validCustomerCancellation :: LYT.TagNameValue
validCustomerCancellation = LYT.TagNameValue "CustomerCancellation#Valid"

invalidDriverCancellation :: LYT.TagNameValue
invalidDriverCancellation = LYT.TagNameValue "DriverCancellation#Invalid"

invalidCustomerCancellation :: LYT.TagNameValue
invalidCustomerCancellation = LYT.TagNameValue "CustomerCancellation#Invalid"
