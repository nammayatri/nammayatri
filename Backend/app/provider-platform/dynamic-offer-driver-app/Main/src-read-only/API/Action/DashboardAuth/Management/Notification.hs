{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Notification
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Notification
import qualified DashboardAlert.Domain.Action.Dashboard.List
import qualified Domain.Action.Dashboard.Management.Notification
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.Fleet
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("notification" :> (GetNotificationNotificationList :<|> PostNotificationNotificationRespond))

type GetNotificationNotificationList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NOTIFICATION/GET_NOTIFICATION_NOTIFICATION_LIST"
      :> API.Types.ProviderPlatform.Management.Notification.GetNotificationNotificationList
  )

type PostNotificationNotificationRespond =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NOTIFICATION/POST_NOTIFICATION_NOTIFICATION_RESPOND"
      :> API.Types.ProviderPlatform.Management.Notification.PostNotificationNotificationRespond
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getNotificationNotificationList merchantId city :<|> postNotificationNotificationRespond merchantId city

getNotificationNotificationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler DashboardAlert.Domain.Action.Dashboard.List.NotificationListResp)
getNotificationNotificationList a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerIds <- Kernel.Prelude.map Kernel.Prelude.fst Kernel.Prelude.<$> SharedLogic.Fleet.getFleetOwnerIds (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) Kernel.Prelude.Nothing
  topic <- Tools.Auth.DashboardUserAuth.resolveRequestorTopic fleetOwnerIds a3
  Domain.Action.Dashboard.Management.Notification.getNotificationNotificationList a5 a4 topic a2 a1

postNotificationNotificationRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Notification.RespondReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNotificationNotificationRespond a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerIds <- Kernel.Prelude.map Kernel.Prelude.fst Kernel.Prelude.<$> SharedLogic.Fleet.getFleetOwnerIds (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) Kernel.Prelude.Nothing
  topic <- Tools.Auth.DashboardUserAuth.resolveRequestorTopic fleetOwnerIds a2
  Domain.Action.Dashboard.Management.Notification.postNotificationNotificationRespond a4 a3 topic a1
