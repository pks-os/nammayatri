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
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as ID
import Servant
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
      _ <- QConfig.updateCoinEntries updatedCoinsConfig
      -- _ <- clearingCacheFunction
      pure Success

--    error "Logic yet to be decided" updatedCoinsConfig

postCoinsConfigCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCoinsConfigReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postCoinsConfigCreate _merchantShortId _opCity req = do error "Logic yet to be decided" req
