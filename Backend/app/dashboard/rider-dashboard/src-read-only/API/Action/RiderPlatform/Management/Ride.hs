{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Ride
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Domain.Action.RiderPlatform.Management.Ride
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("ride" :> GetRideList)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideList merchantId city

type GetRideList = (ApiAuth ('APP_BACKEND_MANAGEMENT) ('RIDES) ('RIDE_LIST) :> API.Types.RiderPlatform.Management.Ride.GetRideList)

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.RiderPlatform.Management.Ride.BookingStatus) -> Kernel.Prelude.Maybe ((Kernel.Types.Id.ShortId Dashboard.Common.Ride)) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideListRes)
getRideList merchantShortId opCity apiTokenInfo limit offset bookingStatus rideShortId customerPhoneNo driverPhoneNo from to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Ride.getRideList merchantShortId opCity apiTokenInfo limit offset bookingStatus rideShortId customerPhoneNo driverPhoneNo from to
