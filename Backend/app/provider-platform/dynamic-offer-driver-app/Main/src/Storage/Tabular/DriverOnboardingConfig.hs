{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOnboardingConfig where

import qualified Domain.Types.DriverOnboardingConfig as Domain
import qualified Domain.Types.Merchant as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverOnboardingConfigT sql=driver_onboarding_config
      merchantId MerchantTId
      onboardingTryLimit Int
      onboardingRetryTimeInHours Int
      onboardSupportSmsTemplate Text
      checkRCInsuranceExpiry Bool
      checkRCExpiry Bool
      checkRCVehicleClass Bool
      checkDLExpiry Bool
      checkDLVehicleClass Bool
      checkImageExtraction Bool
      checkImageExtractionForDashboard Bool
      validDLVehicleClassInfixes (PostgresList Text)
      UniqueDriverOnboardingConfigTId merchantId
      Primary merchantId
      deriving Generic
    |]

instance TEntityKey DriverOnboardingConfigT where
  type DomainKey DriverOnboardingConfigT = Id Domain.Merchant
  fromKey (DriverOnboardingConfigTKey _id) = fromKey _id
  toKey _id = DriverOnboardingConfigTKey (toKey _id)

instance TType DriverOnboardingConfigT Domain.DriverOnboardingConfig where
  fromTType DriverOnboardingConfigT {..} = do
    return $
      Domain.DriverOnboardingConfig
        { merchantId = fromKey merchantId,
          validDLVehicleClassInfixes = unPostgresList validDLVehicleClassInfixes,
          ..
        }
  toTType Domain.DriverOnboardingConfig {..} =
    DriverOnboardingConfigT
      { merchantId = toKey merchantId,
        validDLVehicleClassInfixes = PostgresList validDLVehicleClassInfixes,
        ..
      }
