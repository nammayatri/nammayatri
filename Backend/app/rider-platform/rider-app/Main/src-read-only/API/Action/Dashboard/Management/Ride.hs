{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Ride
  ( API.Types.RiderPlatform.Management.Ride.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Dashboard.Common.Ride
import qualified Domain.Action.Dashboard.Ride
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Ride.API)
handler merchantId city = getRideList merchantId city :<|> getRideInfo merchantId city :<|> cancellationChargesWaiveOff merchantId city :<|> getShareRideInfo merchantId city :<|> getShareRideInfoByShortId merchantId city :<|> getRideTripRoute merchantId city :<|> getRidePickupRoute merchantId city :<|> postRideSyncMultiple merchantId city :<|> postRideCancelMultiple merchantId city :<|> getRideKaptureList merchantId city

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.RiderPlatform.Management.Ride.BookingStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideListRes)
getRideList a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideList a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideInfoRes)
getRideInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideInfo a3 a2 a1

cancellationChargesWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.CancellationChargesWaiveOffRes)
cancellationChargesWaiveOff a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.cancellationChargesWaiveOff a3 a2 a1

getShareRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)
getShareRideInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getShareRideInfo a3 a2 a1

getShareRideInfoByShortId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.ShortId Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)
getShareRideInfoByShortId a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getShareRideInfoByShortId a3 a2 a1

getRideTripRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.FlowHandler Kernel.External.Maps.GetRoutesResp)
getRideTripRoute a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideTripRoute a5 a4 a3 a2 a1

getRidePickupRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.FlowHandler Kernel.External.Maps.GetRoutesResp)
getRidePickupRoute a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRidePickupRoute a5 a4 a3 a2 a1

postRideSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.Ride.MultipleRideSyncReq -> Environment.FlowHandler Dashboard.Common.Ride.MultipleRideSyncResp)
postRideSyncMultiple a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.postRideSyncMultiple a3 a2 a1

postRideCancelMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.Ride.MultipleRideCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideCancelMultiple a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.postRideCancelMultiple a3 a2 a1

getRideKaptureList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.TicketRideListRes)
getRideKaptureList a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideKaptureList a6 a5 a4 a3 a2 a1
