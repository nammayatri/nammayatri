{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.Overlay
  ( API.Types.Dashboard.AppManagement.Overlay.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.Overlay.API)
handler merchantId city = postOverlayCreate merchantId city :<|> postOverlayDelete merchantId city :<|> getOverlayList merchantId city :<|> getOverlayInfo merchantId city :<|> postOverlaySchedule merchantId city

postOverlayCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Overlay.CreateOverlayReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOverlayCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Overlay.postOverlayCreate a3 a2 a1

postOverlayDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Overlay.DeleteOverlayReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOverlayDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Overlay.postOverlayDelete a3 a2 a1

getOverlayList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Overlay.ListOverlayResp)
getOverlayList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Overlay.getOverlayList a2 a1

getOverlayInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Overlay.OverlayInfoResp)
getOverlayInfo a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Overlay.getOverlayInfo a4 a3 a2 a1

postOverlaySchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Overlay.ScheduleOverlay -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOverlaySchedule a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Overlay.postOverlaySchedule a3 a2 a1
