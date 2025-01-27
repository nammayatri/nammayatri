{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.RideBooking.DriverRegistration
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.DriverRegistration
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Domain.Action.ProviderPlatform.RideBooking.DriverRegistration
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driver" :> (PostDriverRegistrationAuth :<|> PostDriverRegistrationVerify))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverRegistrationAuth merchantId city :<|> postDriverRegistrationVerify merchantId city

type PostDriverRegistrationAuth =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER_REGISTRATION / 'API.Types.Dashboard.RideBooking.DriverRegistration.POST_DRIVER_REGISTRATION_AUTH)
      :> API.Types.Dashboard.RideBooking.DriverRegistration.PostDriverRegistrationAuth
  )

type PostDriverRegistrationVerify =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER_REGISTRATION / 'API.Types.Dashboard.RideBooking.DriverRegistration.POST_DRIVER_REGISTRATION_VERIFY)
      :> API.Types.Dashboard.RideBooking.DriverRegistration.PostDriverRegistrationVerify
  )

postDriverRegistrationAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverRegistrationAuth merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.DriverRegistration.postDriverRegistrationAuth merchantShortId opCity apiTokenInfo req

postDriverRegistrationVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthVerifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationVerify merchantShortId opCity apiTokenInfo authId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.DriverRegistration.postDriverRegistrationVerify merchantShortId opCity apiTokenInfo authId req
