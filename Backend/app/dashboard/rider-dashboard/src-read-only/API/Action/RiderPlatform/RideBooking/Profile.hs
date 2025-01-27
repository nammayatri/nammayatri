{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Profile
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Profile
import qualified Domain.Action.RiderPlatform.RideBooking.Profile
import qualified "rider-app" Domain.Action.UI.Profile
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("profile" :> (GetProfileDetail :<|> PostProfileUpdate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getProfileDetail merchantId city :<|> postProfileUpdate merchantId city

type GetProfileDetail =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.PROFILE / 'API.Types.Dashboard.RideBooking.Profile.GET_PROFILE_DETAIL)
      :> API.Types.Dashboard.RideBooking.Profile.GetProfileDetail
  )

type PostProfileUpdate =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.PROFILE / 'API.Types.Dashboard.RideBooking.Profile.POST_PROFILE_UPDATE)
      :> API.Types.Dashboard.RideBooking.Profile.PostProfileUpdate
  )

getProfileDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Domain.Action.UI.Profile.ProfileRes)
getProfileDetail merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Profile.getProfileDetail merchantShortId opCity apiTokenInfo customerId

postProfileUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Profile.UpdateProfileReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postProfileUpdate merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Profile.postProfileUpdate merchantShortId opCity apiTokenInfo customerId req
