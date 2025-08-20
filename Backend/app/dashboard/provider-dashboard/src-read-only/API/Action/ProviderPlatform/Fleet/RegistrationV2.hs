{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Fleet.RegistrationV2
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet
import qualified API.Types.ProviderPlatform.Fleet.RegistrationV2
import qualified Domain.Action.ProviderPlatform.Fleet.RegistrationV2
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

type API = ("fleet" :> (PostRegistrationV2LoginOtp :<|> PostRegistrationV2VerifyOtp :<|> PostRegistrationV2Register))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRegistrationV2LoginOtp merchantId city :<|> postRegistrationV2VerifyOtp merchantId city :<|> postRegistrationV2Register merchantId city

type PostRegistrationV2LoginOtp = API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2LoginOtp

type PostRegistrationV2VerifyOtp = API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2VerifyOtp

type PostRegistrationV2Register =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.REGISTRATION_V2 / 'API.Types.ProviderPlatform.Fleet.RegistrationV2.POST_REGISTRATION_V2_REGISTER)
      :> API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2Register
  )

postRegistrationV2LoginOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerLoginReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationV2LoginOtp merchantShortId opCity req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.RegistrationV2.postRegistrationV2LoginOtp merchantShortId opCity req

postRegistrationV2VerifyOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerVerifyReqV2 -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerVerifyResV2)
postRegistrationV2VerifyOtp merchantShortId opCity req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.RegistrationV2.postRegistrationV2VerifyOtp merchantShortId opCity req

postRegistrationV2Register :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerRegisterReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationV2Register merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.RegistrationV2.postRegistrationV2Register merchantShortId opCity apiTokenInfo req
