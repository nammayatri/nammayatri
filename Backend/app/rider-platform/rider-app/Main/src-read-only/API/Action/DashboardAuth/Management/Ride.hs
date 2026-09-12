{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Ride
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Dashboard.Common.Ride
import qualified Dashboard.Common.RideDebug
import qualified "this" Domain.Action.Dashboard.Ride
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
import Tools.Auth.DashboardUserAuth

type API = ("ride" :> (GetRideList :<|> GetRideInfo :<|> CancellationChargesWaiveOff :<|> GetShareRideInfo :<|> GetShareRideInfoByShortId :<|> GetRideTripRoute :<|> GetRidePickupRoute :<|> PostRideSyncMultiple :<|> PostRidePayoutOfferSync :<|> PostRideCancelMultiple :<|> GetRideKaptureList :<|> GetRideFlowDebugBap))

type GetRideList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/RIDE/GET_RIDE_LIST" :> API.Types.RiderPlatform.Management.Ride.GetRideList)

type GetRideInfo = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/RIDE/GET_RIDE_INFO" :> API.Types.RiderPlatform.Management.Ride.GetRideInfo)

type CancellationChargesWaiveOff =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE/CANCELLATION_CHARGES_WAIVE_OFF"
      :> API.Types.RiderPlatform.Management.Ride.CancellationChargesWaiveOff
  )

type GetShareRideInfo = API.Types.RiderPlatform.Management.Ride.GetShareRideInfo

type GetShareRideInfoByShortId = API.Types.RiderPlatform.Management.Ride.GetShareRideInfoByShortId

type GetRideTripRoute = API.Types.RiderPlatform.Management.Ride.GetRideTripRoute

type GetRidePickupRoute = API.Types.RiderPlatform.Management.Ride.GetRidePickupRoute

type PostRideSyncMultiple = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE" :> API.Types.RiderPlatform.Management.Ride.PostRideSyncMultiple)

type PostRidePayoutOfferSync =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE/POST_RIDE_PAYOUT_OFFER_SYNC"
      :> API.Types.RiderPlatform.Management.Ride.PostRidePayoutOfferSync
  )

type PostRideCancelMultiple = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE" :> API.Types.RiderPlatform.Management.Ride.PostRideCancelMultiple)

type GetRideKaptureList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST" :> API.Types.RiderPlatform.Management.Ride.GetRideKaptureList)

type GetRideFlowDebugBap = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/RIDE/GET_RIDE_FLOW_DEBUG_BAP" :> API.Types.RiderPlatform.Management.Ride.GetRideFlowDebugBap)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideList merchantId city :<|> getRideInfo merchantId city :<|> cancellationChargesWaiveOff merchantId city :<|> getShareRideInfo merchantId city :<|> getShareRideInfoByShortId merchantId city :<|> getRideTripRoute merchantId city :<|> getRidePickupRoute merchantId city :<|> postRideSyncMultiple merchantId city :<|> postRidePayoutOfferSync merchantId city :<|> postRideCancelMultiple merchantId city :<|> getRideKaptureList merchantId city :<|> getRideFlowDebugBap merchantId city

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.RiderPlatform.Management.Ride.BookingStatus) -> Kernel.Prelude.Maybe ((Kernel.Types.Id.ShortId Dashboard.Common.Ride)) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideListRes)
getRideList a11 a10 _a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideList a11 a10 a8 a7 a6 a5 a4 a3 a2 a1

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.RideInfoRes)
getRideInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideInfo a4 a3 a1

cancellationChargesWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.CancellationChargesWaiveOffRes)
cancellationChargesWaiveOff a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.cancellationChargesWaiveOff a4 a3 a1

getShareRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)
getShareRideInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getShareRideInfo a3 a2 a1

getShareRideInfoByShortId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.ShortId Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)
getShareRideInfoByShortId a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getShareRideInfoByShortId a3 a2 a1

getRideTripRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.FlowHandler Kernel.External.Maps.GetRoutesResp)
getRideTripRoute a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideTripRoute a5 a4 a3 a2 a1

getRidePickupRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.FlowHandler Kernel.External.Maps.GetRoutesResp)
getRidePickupRoute a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRidePickupRoute a5 a4 a3 a2 a1

postRideSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Ride.MultipleRideSyncReq -> Environment.FlowHandler Dashboard.Common.Ride.MultipleRideSyncResp)
postRideSyncMultiple a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.postRideSyncMultiple a4 a3 a1

postRidePayoutOfferSync :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Ride.MultipleRideSyncReq -> Environment.FlowHandler Dashboard.Common.Ride.MultipleRideSyncResp)
postRidePayoutOfferSync a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.postRidePayoutOfferSync a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1

postRideCancelMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Ride.MultipleRideCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideCancelMultiple a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.postRideCancelMultiple a4 a3 a1

getRideKaptureList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe ((Kernel.Types.Id.ShortId Dashboard.Common.Ride)) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.RiderPlatform.Management.Ride.TicketRideListRes)
getRideKaptureList a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideKaptureList a7 a6 a4 a3 a2 a1

getRideFlowDebugBap :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Dashboard.Common.RideDebug.BAPSideDebug)
getRideFlowDebugBap a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Ride.getRideFlowDebugBap a9 a8 a6 a5 a4 a3 a2 a1
