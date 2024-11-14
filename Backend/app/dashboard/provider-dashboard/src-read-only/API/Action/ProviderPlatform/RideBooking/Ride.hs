{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.RideBooking.Ride
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Ride
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.RideBooking.Ride
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

type API = ("ride" :> (PostRideStart :<|> PostRideEnd :<|> GetRideCurrentActiveRide :<|> PostRideCancel :<|> PostRideBookingWithVehicleNumberAndPhone))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRideStart merchantId city :<|> postRideEnd merchantId city :<|> getRideCurrentActiveRide merchantId city :<|> postRideCancel merchantId city :<|> postRideBookingWithVehicleNumberAndPhone merchantId city

type PostRideStart =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.RIDE / 'API.Types.Dashboard.RideBooking.Ride.POST_RIDE_START)
      :> API.Types.Dashboard.RideBooking.Ride.PostRideStart
  )

type PostRideEnd =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.RIDE / 'API.Types.Dashboard.RideBooking.Ride.POST_RIDE_END)
      :> API.Types.Dashboard.RideBooking.Ride.PostRideEnd
  )

type GetRideCurrentActiveRide =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.RIDE / 'API.Types.Dashboard.RideBooking.Ride.GET_RIDE_CURRENT_ACTIVE_RIDE)
      :> API.Types.Dashboard.RideBooking.Ride.GetRideCurrentActiveRide
  )

type PostRideCancel =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.RIDE / 'API.Types.Dashboard.RideBooking.Ride.POST_RIDE_CANCEL)
      :> API.Types.Dashboard.RideBooking.Ride.PostRideCancel
  )

type PostRideBookingWithVehicleNumberAndPhone =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.RIDE / 'API.Types.Dashboard.RideBooking.Ride.POST_RIDE_BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE)
      :> API.Types.Dashboard.RideBooking.Ride.PostRideBookingWithVehicleNumberAndPhone
  )

postRideStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.StartRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideStart merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Ride.postRideStart merchantShortId opCity apiTokenInfo rideId req

postRideEnd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.EndRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideEnd merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Ride.postRideEnd merchantShortId opCity apiTokenInfo rideId req

getRideCurrentActiveRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler (Kernel.Types.Id.Id Dashboard.Common.Ride))
getRideCurrentActiveRide merchantShortId opCity apiTokenInfo vehicleNumber = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Ride.getRideCurrentActiveRide merchantShortId opCity apiTokenInfo vehicleNumber

postRideCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.CancelRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideCancel merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Ride.postRideCancel merchantShortId opCity apiTokenInfo rideId req

postRideBookingWithVehicleNumberAndPhone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.RideBooking.Ride.BookingWithVehicleAndPhoneReq -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Ride.BookingWithVehicleAndPhoneRes)
postRideBookingWithVehicleNumberAndPhone merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Ride.postRideBookingWithVehicleNumberAndPhone merchantShortId opCity apiTokenInfo req
