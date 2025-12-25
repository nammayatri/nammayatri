{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Ride
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Dashboard.Common.Ride
import qualified Domain.Action.RiderPlatform.Management.Ride
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("ride" :> (GetRideList :<|> GetRideInfo :<|> CancellationChargesWaiveOff :<|> GetShareRideInfo :<|> GetShareRideInfoByShortId :<|> GetRideTripRoute :<|> GetRidePickupRoute :<|> PostRideSyncMultiple :<|> PostRideCancelMultiple :<|> GetRideKaptureList))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideList merchantId city :<|> getRideInfo merchantId city :<|> cancellationChargesWaiveOff merchantId city :<|> getShareRideInfo merchantId city :<|> getShareRideInfoByShortId merchantId city :<|> getRideTripRoute merchantId city :<|> getRidePickupRoute merchantId city :<|> postRideSyncMultiple merchantId city :<|> postRideCancelMultiple merchantId city :<|> getRideKaptureList merchantId city

type GetRideList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.RIDE / 'API.Types.RiderPlatform.Management.Ride.GET_RIDE_LIST)
      :> API.Types.RiderPlatform.Management.Ride.GetRideList
  )

type GetRideInfo =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.RIDE / 'API.Types.RiderPlatform.Management.Ride.GET_RIDE_INFO)
      :> API.Types.RiderPlatform.Management.Ride.GetRideInfo
  )

type CancellationChargesWaiveOff =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.RIDE / 'API.Types.RiderPlatform.Management.Ride.CANCELLATION_CHARGES_WAIVE_OFF)
      :> API.Types.RiderPlatform.Management.Ride.CancellationChargesWaiveOff
  )

type GetShareRideInfo = API.Types.RiderPlatform.Management.Ride.GetShareRideInfo

type GetShareRideInfoByShortId = API.Types.RiderPlatform.Management.Ride.GetShareRideInfoByShortId

type GetRideTripRoute = API.Types.RiderPlatform.Management.Ride.GetRideTripRoute

type GetRidePickupRoute = API.Types.RiderPlatform.Management.Ride.GetRidePickupRoute

type PostRideSyncMultiple =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.RIDE / 'API.Types.RiderPlatform.Management.Ride.POST_RIDE_SYNC_MULTIPLE)
      :> API.Types.RiderPlatform.Management.Ride.PostRideSyncMultiple
  )

type PostRideCancelMultiple =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.RIDE / 'API.Types.RiderPlatform.Management.Ride.POST_RIDE_CANCEL_MULTIPLE)
      :> API.Types.RiderPlatform.Management.Ride.PostRideCancelMultiple
  )

type GetRideKaptureList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.RIDE / 'API.Types.RiderPlatform.Management.Ride.GET_RIDE_KAPTURE_LIST)
      :> API.Types.RiderPlatform.Management.Ride.GetRideKaptureList
  )

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.RiderPlatform.Management.Ride.BookingStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideListRes)
getRideList merchantShortId opCity apiTokenInfo limit offset bookingStatus rideShortId customerPhoneNo driverPhoneNo from to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRideList merchantShortId opCity apiTokenInfo limit offset bookingStatus rideShortId customerPhoneNo driverPhoneNo from to

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideInfoRes)
getRideInfo merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRideInfo merchantShortId opCity apiTokenInfo rideId

cancellationChargesWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.CancellationChargesWaiveOffRes)
cancellationChargesWaiveOff merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.cancellationChargesWaiveOff merchantShortId opCity apiTokenInfo rideId

getShareRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)
getShareRideInfo merchantShortId opCity rideId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getShareRideInfo merchantShortId opCity rideId

getShareRideInfoByShortId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.ShortId Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)
getShareRideInfoByShortId merchantShortId opCity rideShortId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getShareRideInfoByShortId merchantShortId opCity rideShortId

getRideTripRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.FlowHandler Kernel.External.Maps.GetRoutesResp)
getRideTripRoute merchantShortId opCity rideId lat lon = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRideTripRoute merchantShortId opCity rideId lat lon

getRidePickupRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.FlowHandler Kernel.External.Maps.GetRoutesResp)
getRidePickupRoute merchantShortId opCity rideId lat lon = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRidePickupRoute merchantShortId opCity rideId lat lon

postRideSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Ride.MultipleRideSyncReq -> Environment.FlowHandler Dashboard.Common.Ride.MultipleRideSyncResp)
postRideSyncMultiple merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.postRideSyncMultiple merchantShortId opCity apiTokenInfo req

postRideCancelMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Ride.MultipleRideCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideCancelMultiple merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.postRideCancelMultiple merchantShortId opCity apiTokenInfo req

getRideKaptureList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.TicketRideListRes)
getRideKaptureList merchantShortId opCity apiTokenInfo rideShortId countryCode phoneNumber supportPhoneNumber = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRideKaptureList merchantShortId opCity apiTokenInfo rideShortId countryCode phoneNumber supportPhoneNumber
