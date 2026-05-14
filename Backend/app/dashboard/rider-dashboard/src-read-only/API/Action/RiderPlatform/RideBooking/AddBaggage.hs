{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.AddBaggage
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.AddBaggage
import qualified Domain.Action.RiderPlatform.RideBooking.AddBaggage
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

type API = ("rideBooking" :> PostAddBaggageConfirm)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postAddBaggageConfirm merchantId city

type PostAddBaggageConfirm =
  ( ApiAuth
      ('APP_BACKEND)
      ('DSL)
      (('RIDER_RIDE_BOOKING) / ('API.Types.Dashboard.RideBooking.ADD_BAGGAGE) / ('API.Types.Dashboard.RideBooking.AddBaggage.POST_ADD_BAGGAGE_CONFIRM))
      :> API.Types.Dashboard.RideBooking.AddBaggage.PostAddBaggageConfirm
  )

postAddBaggageConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.RideBooking.AddBaggage.AddBaggageConfirmReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAddBaggageConfirm merchantShortId opCity apiTokenInfo bookingId customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.AddBaggage.postAddBaggageConfirm merchantShortId opCity apiTokenInfo bookingId customerId req
