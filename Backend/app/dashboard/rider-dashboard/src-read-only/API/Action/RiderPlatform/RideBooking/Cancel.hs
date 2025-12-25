{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Cancel
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Cancel
import qualified Domain.Action.RiderPlatform.RideBooking.Cancel
import qualified "rider-app" Domain.Action.UI.Cancel
import qualified "rider-app" Domain.Types.Booking
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

type API = ("rideBooking" :> PostCancelBooking)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postCancelBooking merchantId city

type PostCancelBooking =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.CANCEL / 'API.Types.Dashboard.RideBooking.Cancel.POST_CANCEL_BOOKING)
      :> API.Types.Dashboard.RideBooking.Cancel.PostCancelBooking
  )

postCancelBooking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Cancel.CancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancelBooking merchantShortId opCity apiTokenInfo rideBookingId customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Cancel.postCancelBooking merchantShortId opCity apiTokenInfo rideBookingId customerId req
