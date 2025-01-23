{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.CoinsConfig
  ( putCoinsConfigUpdate,
    postCoinsConfigCreate,
  )
where

import qualified API.Types.ProviderPlatform.Management.CoinsConfig
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

putCoinsConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.CoinsConfig.UpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putCoinsConfigUpdate _merchantShortId _opCity req = do error "Logic yet to be decided" req

postCoinsConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.CoinsConfig.CreateCoinsConfigReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postCoinsConfigCreate _merchantShortId _opCity req = do error "Logic yet to be decided" req
