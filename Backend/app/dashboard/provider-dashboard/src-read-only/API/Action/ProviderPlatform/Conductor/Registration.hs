{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Conductor.Registration
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Conductor
import qualified API.Types.ProviderPlatform.Conductor.Registration
import qualified Domain.Action.ProviderPlatform.Conductor.Registration
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("conductor" :> (PostConductorRegister :<|> PostConductorBulkRegister))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postConductorRegister merchantId city :<|> postConductorBulkRegister merchantId city

type PostConductorRegister =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_CONDUCTOR) / ('API.Types.ProviderPlatform.Conductor.REGISTRATION) / ('API.Types.ProviderPlatform.Conductor.Registration.POST_CONDUCTOR_REGISTER))
      :> API.Types.ProviderPlatform.Conductor.Registration.PostConductorRegister
  )

type PostConductorBulkRegister =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_CONDUCTOR) / ('API.Types.ProviderPlatform.Conductor.REGISTRATION) / ('API.Types.ProviderPlatform.Conductor.Registration.POST_CONDUCTOR_BULK_REGISTER))
      :> API.Types.ProviderPlatform.Conductor.Registration.PostConductorBulkRegister
  )

postConductorRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Conductor.Registration.ConductorRegisterReq -> Environment.FlowHandler API.Types.ProviderPlatform.Conductor.Registration.ConductorRegisterResp)
postConductorRegister merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Conductor.Registration.postConductorRegister merchantShortId opCity apiTokenInfo req

postConductorBulkRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Conductor.Registration.ConductorBulkRegisterReq -> Environment.FlowHandler API.Types.ProviderPlatform.Conductor.Registration.ConductorBulkRegisterResp)
postConductorBulkRegister merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Conductor.Registration.postConductorBulkRegister merchantShortId opCity apiTokenInfo req
