{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Ride
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Dashboard.RiderPlatform.Ride
import qualified Domain.Action.RiderPlatform.Management.Ride
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("ride" :> (GetRideList :<|> GetRideRideinfo :<|> GetRideInfo :<|> GetRideRideInfo :<|> GetRideTripRoute))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideList merchantId city :<|> getRideRideinfo merchantId city :<|> getRideInfo merchantId city :<|> getRideRideInfo merchantId city :<|> getRideTripRoute merchantId city

type GetRideList = (ApiAuth 'APP_BACKEND_MANAGEMENT 'RIDES 'RIDE_LIST :> API.Types.RiderPlatform.Management.Ride.GetRideList)

type GetRideRideinfo = (ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'RIDE_INFO_CUSTOMER :> API.Types.RiderPlatform.Management.Ride.GetRideRideinfo)

type GetRideInfo = (ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'RIDE_INFO_CUSTOMER :> API.Types.RiderPlatform.Management.Ride.GetRideInfo)

type GetRideRideInfo = (ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'RIDE_INFO_CUSTOMER :> API.Types.RiderPlatform.Management.Ride.GetRideRideInfo)

type GetRideTripRoute = (ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'RIDE_INFO_CUSTOMER :> API.Types.RiderPlatform.Management.Ride.GetRideTripRoute)

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Dashboard.RiderPlatform.Ride.BookingStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideListRes)
getRideList merchantShortId opCity apiTokenInfo limit offset bookingStatus rideShortId customerPhoneNo driverPhoneNo from to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRideList merchantShortId opCity apiTokenInfo limit offset bookingStatus rideShortId customerPhoneNo driverPhoneNo from to

getRideRideinfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideInfoRes)
getRideRideinfo merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRideRideinfo merchantShortId opCity apiTokenInfo rideId

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)
getRideInfo merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRideInfo merchantShortId opCity apiTokenInfo rideId

getRideRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.ShortId Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)
getRideRideInfo merchantShortId opCity apiTokenInfo rideShortId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRideRideInfo merchantShortId opCity apiTokenInfo rideShortId

getRideTripRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.FlowHandler Kernel.External.Maps.GetRoutesResp)
getRideTripRoute merchantShortId opCity apiTokenInfo rideId lat lon = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRideTripRoute merchantShortId opCity apiTokenInfo rideId lat lon
