{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Ride
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Ride
import qualified Control.Lens
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Ride
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("ride" :> (GetRideAgentList :<|> GetRideList :<|> GetRideListV2 :<|> PostRideEndMultiple :<|> PostRideCancelMultiple :<|> GetRideInfo :<|> GetRideEvidence :<|> GetRideEvidenceFile :<|> PostRideEvidenceVisibility :<|> PostRideSync :<|> PostRideSyncMultiple :<|> PostRideRoute :<|> GetRideEstimatedRoute :<|> GetRideSnappedRoute :<|> GetRideKaptureList :<|> GetRideFareBreakUp :<|> PostRideWaiverRideCancellationPenalty :<|> GetRideNearby :<|> GetRideCallCount :<|> GetRideFlowDebug))

type GetRideAgentList = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_AGENT_LIST" :> API.Types.ProviderPlatform.Management.Ride.GetRideAgentList)

type GetRideList = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_LIST" :> API.Types.ProviderPlatform.Management.Ride.GetRideList)

type GetRideListV2 = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_LIST_V2" :> API.Types.ProviderPlatform.Management.Ride.GetRideListV2)

type PostRideEndMultiple = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE" :> API.Types.ProviderPlatform.Management.Ride.PostRideEndMultiple)

type PostRideCancelMultiple =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE"
      :> API.Types.ProviderPlatform.Management.Ride.PostRideCancelMultiple
  )

type GetRideInfo = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_INFO" :> API.Types.ProviderPlatform.Management.Ride.GetRideInfo)

type GetRideEvidence = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_EVIDENCE" :> API.Types.ProviderPlatform.Management.Ride.GetRideEvidence)

type GetRideEvidenceFile = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_EVIDENCE_FILE" :> API.Types.ProviderPlatform.Management.Ride.GetRideEvidenceFile)

type PostRideEvidenceVisibility =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_EVIDENCE_VISIBILITY"
      :> API.Types.ProviderPlatform.Management.Ride.PostRideEvidenceVisibility
  )

type PostRideSync = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC" :> API.Types.ProviderPlatform.Management.Ride.PostRideSync)

type PostRideSyncMultiple = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE" :> API.Types.ProviderPlatform.Management.Ride.PostRideSyncMultiple)

type PostRideRoute = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_ROUTE" :> API.Types.ProviderPlatform.Management.Ride.PostRideRoute)

type GetRideEstimatedRoute = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_ESTIMATED_ROUTE" :> API.Types.ProviderPlatform.Management.Ride.GetRideEstimatedRoute)

type GetRideSnappedRoute = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_SNAPPED_ROUTE" :> API.Types.ProviderPlatform.Management.Ride.GetRideSnappedRoute)

type GetRideKaptureList = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST" :> API.Types.ProviderPlatform.Management.Ride.GetRideKaptureList)

type GetRideFareBreakUp = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_FARE_BREAK_UP" :> API.Types.ProviderPlatform.Management.Ride.GetRideFareBreakUp)

type PostRideWaiverRideCancellationPenalty =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_WAIVER_RIDE_CANCELLATION_PENALTY"
      :> API.Types.ProviderPlatform.Management.Ride.PostRideWaiverRideCancellationPenalty
  )

type GetRideNearby = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_NEARBY" :> API.Types.ProviderPlatform.Management.Ride.GetRideNearby)

type GetRideCallCount = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_CALL_COUNT" :> API.Types.ProviderPlatform.Management.Ride.GetRideCallCount)

type GetRideFlowDebug = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/GET_RIDE_FLOW_DEBUG" :> API.Types.ProviderPlatform.Management.Ride.GetRideFlowDebug)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideAgentList merchantId city :<|> getRideList merchantId city :<|> getRideListV2 merchantId city :<|> postRideEndMultiple merchantId city :<|> postRideCancelMultiple merchantId city :<|> getRideInfo merchantId city :<|> getRideEvidence merchantId city :<|> getRideEvidenceFile merchantId city :<|> postRideEvidenceVisibility merchantId city :<|> postRideSync merchantId city :<|> postRideSyncMultiple merchantId city :<|> postRideRoute merchantId city :<|> getRideEstimatedRoute merchantId city :<|> getRideSnappedRoute merchantId city :<|> getRideKaptureList merchantId city :<|> getRideFareBreakUp merchantId city :<|> postRideWaiverRideCancellationPenalty merchantId city :<|> getRideNearby merchantId city :<|> getRideCallCount merchantId city :<|> getRideFlowDebug merchantId city

getRideAgentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.BookingStatus -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListRes)
getRideAgentList a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a11 $ Domain.Action.Dashboard.Management.Ride.getRideAgentList a13 a12 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.BookingStatus -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe [API.Types.ProviderPlatform.Management.Ride.RideDetailGroup] -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.PaymentCollector -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.PaymentMode -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListRes)
getRideList a23 a22 a21 a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a21 $ Domain.Action.Dashboard.Management.Ride.getRideList a23 a22 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a21) a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getRideListV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.PaymentMode -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.RideStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListResV2)
getRideListV2 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a16 $ Domain.Action.Dashboard.Management.Ride.getRideListV2 a18 a17 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a16) a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postRideEndMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Ride.MultipleRideEndReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideEndResp)
postRideEndMultiple a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.postRideEndMultiple a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1
    )

postRideCancelMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelResp)
postRideCancelMultiple a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.postRideCancelMultiple a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1
    )

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Maybe [API.Types.ProviderPlatform.Management.Ride.RideDetailGroup] -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideInfoRes)
getRideInfo a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.Management.Ride.getRideInfo a6 a5 a3 a2 a1

getRideEvidence :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideEvidenceListRes)
getRideEvidence a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.getRideEvidence a4 a3 a1

getRideEvidenceFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideEvidenceFileRes)
getRideEvidenceFile a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.Ride.getRideEvidenceFile a5 a4 a2 a1

postRideEvidenceVisibility :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Ride.SetRideEvidenceVisibilityReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideEvidenceVisibility a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_EVIDENCE_VISIBILITY" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.Management.Ride.postRideEvidenceVisibility a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1
    )

postRideSync :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideSyncRes)
postRideSync a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.postRideSync a4 a3 a1
    )

postRideSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncRes)
postRideSyncMultiple a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.postRideSyncMultiple a4 a3 a1
    )

postRideRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideRouteRes)
postRideRoute a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_ROUTE" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.postRideRoute a4 a3 a1
    )

getRideEstimatedRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideEstimatedRouteRes)
getRideEstimatedRoute a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.getRideEstimatedRoute a4 a3 a1

getRideSnappedRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideSnappedRouteRes)
getRideSnappedRoute a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.getRideSnappedRoute a4 a3 a1

getRideKaptureList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.TicketRideListRes)
getRideKaptureList a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.Management.Ride.getRideKaptureList a7 a6 a4 a3 a2 a1

getRideFareBreakUp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.FareBreakUpRes)
getRideFareBreakUp a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.getRideFareBreakUp a4 a3 a1

postRideWaiverRideCancellationPenalty :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.ProviderPlatform.Management.Ride.WaiverRideCancellationPenaltyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideWaiverRideCancellationPenalty a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RIDE/POST_RIDE_WAIVER_RIDE_CANCELLATION_PENALTY" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.Ride.postRideWaiverRideCancellationPenalty a5 a4 a2 a1
    )

getRideNearby :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.NearbyResp)
getRideNearby a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.getRideNearby a4 a3 a1

getRideCallCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideCallCountRes)
getRideCallCount a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Ride.getRideCallCount a4 a3 a1

getRideFlowDebug :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideFlowDebugRes)
getRideFlowDebug a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a9 $ Domain.Action.Dashboard.Management.Ride.getRideFlowDebug a11 a10 a8 a7 a6 a5 a4 a3 a2 a1
