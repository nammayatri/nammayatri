{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Notification
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Notification
import qualified Domain.Action.UI.Notification
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import "lib-dashboard" Tools.Auth

type API =
  ( DashboardAuth 'DASHBOARD_USER :> "list" :> "notification" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "readStatus"
           Kernel.Prelude.Bool
      :> Get
           '[JSON]
           API.Types.UI.Notification.NotificationList
      :<|> DashboardAuth 'DASHBOARD_USER
      :> "read"
      :> "notification"
      :> ReqBody
           '[JSON]
           API.Types.UI.Notification.NotificationReadRequest
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> DashboardAuth 'DASHBOARD_USER
      :> "update"
      :> "receive"
      :> "notificationStatus"
      :> ReqBody
           '[JSON]
           API.Types.UI.Notification.UpdateRecieveNotificationStatusRequest
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getListNotification :<|> postReadNotification :<|> postUpdateReceiveNotificationStatus

getListNotification :: (TokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.UI.Notification.NotificationList)
getListNotification a4 a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Notification.getListNotification a4 a3 a2 a1

postReadNotification :: (TokenInfo -> API.Types.UI.Notification.NotificationReadRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postReadNotification a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Notification.postReadNotification a2 a1

postUpdateReceiveNotificationStatus :: (TokenInfo -> API.Types.UI.Notification.UpdateRecieveNotificationStatusRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postUpdateReceiveNotificationStatus a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Notification.postUpdateReceiveNotificationStatus a2 a1
