{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.Notification
  ( getNotificationNotificationList,
    postNotificationNotificationRespond,
  )
where

import qualified API.Types.ProviderPlatform.Management.Notification
import qualified DashboardAlert.Domain.Action.Dashboard.List
import qualified DashboardAlert.Domain.Types.Audience
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Storage.Beam.DashboardAlert ()
import Tools.Auth

getNotificationNotificationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow DashboardAlert.Domain.Action.Dashboard.List.NotificationListResp)
getNotificationNotificationList _merchantShortId _opCity topic mbLimit mbOffset = DashboardAlert.Domain.Action.Dashboard.List.listNotifications (DashboardAlert.Domain.Types.Audience.Topic topic) mbLimit mbOffset

postNotificationNotificationRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Notification.RespondReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNotificationNotificationRespond _merchantShortId _opCity topic req = do
  DashboardAlert.Domain.Action.Dashboard.List.respondToNotification
    (DashboardAlert.Domain.Types.Audience.Topic topic)
    DashboardAlert.Domain.Action.Dashboard.List.RespondReq
      { notificationId = req.notificationId,
        status = req.status,
        reason = req.reason
      }
  pure Kernel.Types.APISuccess.Success
