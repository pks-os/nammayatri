{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.CoinsConfig
  ( putCoinsConfigUpdate,
    postCoinsConfigCreate,
  )
where

import qualified API.Types.ProviderPlatform.Management.CoinsConfig as Common
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Coins.CoinsConfig as DTCC
import qualified Domain.Types.Merchant
import Domain.Types.VehicleCategory (VehicleCategory)
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as ID
import qualified Kernel.Utils.Common as UC
import Servant
import qualified Storage.CachedQueries.CoinsConfig as CQConfig
import qualified Storage.Queries.Coins.CoinsConfig as QConfig
import Tools.Auth

putCoinsConfigUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putCoinsConfigUpdate _merchantShortId _opCity Common.UpdateReq {..} = do
  mbCoinsConfig <- QConfig.fetchConfigOnIdBasis $ ID.cast entriesId
  case mbCoinsConfig of
    Nothing -> error "No entries with such Id:" entriesId
    Just coinsConfig -> do
      let updatedCoinsConfig =
            coinsConfig
              { DTCC.active = active,
                DTCC.expirationAt = expirationAt,
                DTCC.coins = coins
              }
          vehicleCategory = getVehicleCategoryFromMaybe coinsConfig.vehicleCategory
      _ <- QConfig.updateCoinEntries updatedCoinsConfig
      _ <- CQConfig.clearCache coinsConfig.eventName coinsConfig.eventFunction (ID.Id coinsConfig.merchantOptCityId) vehicleCategory
      pure Success

postCoinsConfigCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCoinsConfigReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postCoinsConfigCreate _merchantShortId _opCity req = do
  uuid <- UC.generateGUIDText
  newCoinsConfig <- case req of
    Common.NewCoinsConfig (Common.NewCoinsConfigReq {..}) ->
      pure $
        DTCC.CoinsConfig
          { id = ID.Id uuid,
            ..
          }
    Common.DuplicateCoinsConfig (Common.DuplicateCoinsConfigsReq {..}) -> do
      mbCoinsConfig <- QConfig.fetchConfigOnIdBasis $ ID.cast entriesId
      case mbCoinsConfig of
        Nothing -> error "No entries with such Id:" entriesId
        Just coinsConfig ->
          pure $
            coinsConfig
              { DTCC.id = ID.Id uuid,
                DTCC.eventFunction = eventFunction
              }
  _ <- QConfig.updateCoinEntries newCoinsConfig
  _ <-
    CQConfig.clearCache
      newCoinsConfig.eventName
      newCoinsConfig.eventFunction
      (ID.Id newCoinsConfig.merchantOptCityId)
      (getVehicleCategoryFromMaybe newCoinsConfig.vehicleCategory)
  pure Success

getVehicleCategoryFromMaybe :: Maybe VehicleCategory -> VehicleCategory
getVehicleCategoryFromMaybe mbVehicleCategory =
  case mbVehicleCategory of
    Just vehicleCategory -> vehicleCategory
    Nothing -> error "vehicleCategory is Nothing" mbVehicleCategory
