module Domain.Types.DriverOnboardingConfig where

import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data DriverOnboardingConfig = DriverOnboardingConfig
  { merchantId :: Id Merchant,
    onboardingTryLimit :: Int,
    onboardingRetryTimeInHours :: Int,
    onboardSupportSmsTemplate :: Text,
    checkRCInsuranceExpiry :: Bool,
    checkRCExpiry :: Bool,
    checkRCVehicleClass :: Bool,
    checkDLExpiry :: Bool,
    checkDLVehicleClass :: Bool,
    checkImageExtraction :: Bool,
    checkImageExtractionForDashboard :: Bool,
    validDLVehicleClassInfixes :: [Text]
  }
  deriving (Show, FromJSON, ToJSON, Generic)
