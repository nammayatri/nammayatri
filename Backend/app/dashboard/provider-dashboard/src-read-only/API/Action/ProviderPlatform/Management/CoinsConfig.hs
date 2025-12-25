{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.CoinsConfig
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.CoinsConfig
import qualified Domain.Action.ProviderPlatform.Management.CoinsConfig
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("coinsConfig" :> (PutCoinsConfigUpdate :<|> PostCoinsConfigCreate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = putCoinsConfigUpdate merchantId city :<|> postCoinsConfigCreate merchantId city

type PutCoinsConfigUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COINS_CONFIG / 'API.Types.ProviderPlatform.Management.CoinsConfig.PUT_COINS_CONFIG_UPDATE)
      :> API.Types.ProviderPlatform.Management.CoinsConfig.PutCoinsConfigUpdate
  )

type PostCoinsConfigCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COINS_CONFIG / 'API.Types.ProviderPlatform.Management.CoinsConfig.POST_COINS_CONFIG_CREATE)
      :> API.Types.ProviderPlatform.Management.CoinsConfig.PostCoinsConfigCreate
  )

putCoinsConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.CoinsConfig.UpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putCoinsConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.CoinsConfig.putCoinsConfigUpdate merchantShortId opCity apiTokenInfo req

postCoinsConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.CoinsConfig.CreateCoinsConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCoinsConfigCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.CoinsConfig.postCoinsConfigCreate merchantShortId opCity apiTokenInfo req
