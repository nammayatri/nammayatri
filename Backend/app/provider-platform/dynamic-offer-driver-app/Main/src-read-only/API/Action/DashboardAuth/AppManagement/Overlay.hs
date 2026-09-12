{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.Overlay
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Overlay
import qualified Domain.Action.Dashboard.AppManagement.Overlay
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("overlay" :> (PostOverlayCreate :<|> PostOverlayDelete :<|> GetOverlayList :<|> GetOverlayInfo :<|> PostOverlaySchedule))

type PostOverlayCreate = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_CREATE" :> API.Types.Dashboard.AppManagement.Overlay.PostOverlayCreate)

type PostOverlayDelete = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_DELETE" :> API.Types.Dashboard.AppManagement.Overlay.PostOverlayDelete)

type GetOverlayList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_APP_MANAGEMENT/OVERLAY/GET_OVERLAY_LIST" :> API.Types.Dashboard.AppManagement.Overlay.GetOverlayList)

type GetOverlayInfo = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_APP_MANAGEMENT/OVERLAY/GET_OVERLAY_INFO" :> API.Types.Dashboard.AppManagement.Overlay.GetOverlayInfo)

type PostOverlaySchedule = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_SCHEDULE" :> API.Types.Dashboard.AppManagement.Overlay.PostOverlaySchedule)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postOverlayCreate merchantId city :<|> postOverlayDelete merchantId city :<|> getOverlayList merchantId city :<|> getOverlayInfo merchantId city :<|> postOverlaySchedule merchantId city

postOverlayCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.Overlay.CreateOverlayReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOverlayCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Overlay.postOverlayCreate a4 a3 a1

postOverlayDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.Overlay.DeleteOverlayReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOverlayDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Overlay.postOverlayDelete a4 a3 a1

getOverlayList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Overlay.ListOverlayResp)
getOverlayList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Overlay.getOverlayList a3 a2

getOverlayInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Overlay.OverlayInfoResp)
getOverlayInfo a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Overlay.getOverlayInfo a5 a4 a2 a1

postOverlaySchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.Overlay.ScheduleOverlay -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOverlaySchedule a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Overlay.postOverlaySchedule a4 a3 a1
