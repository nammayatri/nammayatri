{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Notification
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Notification
import qualified DashboardAlert.Domain.Action.Dashboard.List
import qualified Domain.Action.RiderPlatform.Management.Notification
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()

type API = ("notification" :> (GetNotificationNotificationList :<|> PostNotificationNotificationRespond))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getNotificationNotificationList merchantId city :<|> postNotificationNotificationRespond merchantId city

type GetNotificationNotificationList =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NOTIFICATION) / ('API.Types.RiderPlatform.Management.Notification.GET_NOTIFICATION_NOTIFICATION_LIST))
      :> API.Types.RiderPlatform.Management.Notification.GetNotificationNotificationList
  )

type PostNotificationNotificationRespond =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NOTIFICATION) / ('API.Types.RiderPlatform.Management.Notification.POST_NOTIFICATION_NOTIFICATION_RESPOND))
      :> API.Types.RiderPlatform.Management.Notification.PostNotificationNotificationRespond
  )

getNotificationNotificationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler DashboardAlert.Domain.Action.Dashboard.List.NotificationListResp)
getNotificationNotificationList merchantShortId opCity apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Notification.getNotificationNotificationList merchantShortId opCity apiTokenInfo mbLimit mbOffset

postNotificationNotificationRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.Management.Notification.RespondReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNotificationNotificationRespond merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Notification.postNotificationNotificationRespond merchantShortId opCity apiTokenInfo req
