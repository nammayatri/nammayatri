{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Notification
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Notification
import qualified DashboardAlert.Domain.Action.Dashboard.List
import qualified Domain.Action.Dashboard.Notification
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

type API = ("notification" :> (GetNotificationNotificationList :<|> PostNotificationNotificationRespond))

type GetNotificationNotificationList =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NOTIFICATION/GET_NOTIFICATION_NOTIFICATION_LIST"
      :> API.Types.RiderPlatform.Management.Notification.GetNotificationNotificationList
  )

type PostNotificationNotificationRespond =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NOTIFICATION/POST_NOTIFICATION_NOTIFICATION_RESPOND"
      :> API.Types.RiderPlatform.Management.Notification.PostNotificationNotificationRespond
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getNotificationNotificationList merchantId city :<|> postNotificationNotificationRespond merchantId city

getNotificationNotificationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler DashboardAlert.Domain.Action.Dashboard.List.NotificationListResp)
getNotificationNotificationList a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  let fleetOwnerIds = []
  topic <- Tools.Auth.DashboardUserAuth.resolveRequestorTopic fleetOwnerIds a3
  Domain.Action.Dashboard.Notification.getNotificationNotificationList a5 a4 topic a2 a1

postNotificationNotificationRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Notification.RespondReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNotificationNotificationRespond a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  let fleetOwnerIds = []
  topic <- Tools.Auth.DashboardUserAuth.resolveRequestorTopic fleetOwnerIds a2
  Domain.Action.Dashboard.Notification.postNotificationNotificationRespond a4 a3 topic a1
