{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Ride
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Ride
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("ride" :> (GetRideAgentList :<|> GetRideList :<|> GetRideListV2 :<|> PostRideEndMultiple :<|> PostRideCancelMultiple :<|> GetRideInfo :<|> PostRideSync :<|> PostRideSyncMultiple :<|> PostRideRoute :<|> GetRideKaptureList :<|> GetRideFareBreakUp :<|> PostRideWaiverRideCancellationPenalty :<|> GetRideNearby :<|> GetRideCallCount :<|> GetRideFlowDebug))

type GetRideAgentList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_AGENT_LIST" :> API.Types.ProviderPlatform.Management.Ride.GetRideAgentList)

type GetRideList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_LIST" :> API.Types.ProviderPlatform.Management.Ride.GetRideList)

type GetRideListV2 = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_LIST_V2" :> API.Types.ProviderPlatform.Management.Ride.GetRideListV2)

type PostRideEndMultiple =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE"
      :> API.Types.ProviderPlatform.Management.Ride.PostRideEndMultiple
  )

type PostRideCancelMultiple =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE"
      :> API.Types.ProviderPlatform.Management.Ride.PostRideCancelMultiple
  )

type GetRideInfo = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_INFO" :> API.Types.ProviderPlatform.Management.Ride.GetRideInfo)

type PostRideSync = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC" :> API.Types.ProviderPlatform.Management.Ride.PostRideSync)

type PostRideSyncMultiple = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE" :> API.Types.ProviderPlatform.Management.Ride.PostRideSyncMultiple)

type PostRideRoute = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_ROUTE" :> API.Types.ProviderPlatform.Management.Ride.PostRideRoute)

type GetRideKaptureList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST" :> API.Types.ProviderPlatform.Management.Ride.GetRideKaptureList)

type GetRideFareBreakUp = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_FARE_BREAK_UP" :> API.Types.ProviderPlatform.Management.Ride.GetRideFareBreakUp)

type PostRideWaiverRideCancellationPenalty =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_WAIVER_RIDE_CANCELLATION_PENALTY"
      :> API.Types.ProviderPlatform.Management.Ride.PostRideWaiverRideCancellationPenalty
  )

type GetRideNearby = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_NEARBY" :> API.Types.ProviderPlatform.Management.Ride.GetRideNearby)

type GetRideCallCount = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_CALL_COUNT" :> API.Types.ProviderPlatform.Management.Ride.GetRideCallCount)

type GetRideFlowDebug = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_FLOW_DEBUG" :> API.Types.ProviderPlatform.Management.Ride.GetRideFlowDebug)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideAgentList merchantId city :<|> getRideList merchantId city :<|> getRideListV2 merchantId city :<|> postRideEndMultiple merchantId city :<|> postRideCancelMultiple merchantId city :<|> getRideInfo merchantId city :<|> postRideSync merchantId city :<|> postRideSyncMultiple merchantId city :<|> postRideRoute merchantId city :<|> getRideKaptureList merchantId city :<|> getRideFareBreakUp merchantId city :<|> postRideWaiverRideCancellationPenalty merchantId city :<|> getRideNearby merchantId city :<|> getRideCallCount merchantId city :<|> getRideFlowDebug merchantId city

getRideAgentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Ride.BookingStatus) -> Kernel.Prelude.Maybe (Kernel.Types.Common.Currency) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListRes)
getRideAgentList a13 a12 _a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideAgentList a13 a12 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Ride.BookingStatus) -> Kernel.Prelude.Maybe (Kernel.Types.Common.Currency) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Ride.PaymentMode) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListRes)
getRideList a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideList a20 a19 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a18) a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getRideListV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Common.Currency) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Ride.PaymentMode) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Ride.RideStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListResV2)
getRideListV2 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideListV2 a18 a17 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a16) a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postRideEndMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Ride.MultipleRideEndReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideEndResp)
postRideEndMultiple a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideEndMultiple a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1

postRideCancelMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelResp)
postRideCancelMultiple a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideCancelMultiple a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideInfoRes)
getRideInfo a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideInfo a5 a4 a2 a1

postRideSync :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideSyncRes)
postRideSync a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideSync a4 a3 a1

postRideSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncRes)
postRideSyncMultiple a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideSyncMultiple a4 a3 a1

postRideRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideRouteRes)
postRideRoute a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideRoute a4 a3 a1

getRideKaptureList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.TicketRideListRes)
getRideKaptureList a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideKaptureList a7 a6 a4 a3 a2 a1

getRideFareBreakUp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.FareBreakUpRes)
getRideFareBreakUp a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideFareBreakUp a4 a3 a1

postRideWaiverRideCancellationPenalty :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.ProviderPlatform.Management.Ride.WaiverRideCancellationPenaltyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideWaiverRideCancellationPenalty a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.postRideWaiverRideCancellationPenalty a5 a4 a2 a1

getRideNearby :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.NearbyResp)
getRideNearby a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideNearby a4 a3 a1

getRideCallCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideCallCountRes)
getRideCallCount a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideCallCount a4 a3 a1

getRideFlowDebug :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideFlowDebugRes)
getRideFlowDebug a11 a10 _a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Ride.getRideFlowDebug a11 a10 a8 a7 a6 a5 a4 a3 a2 a1
