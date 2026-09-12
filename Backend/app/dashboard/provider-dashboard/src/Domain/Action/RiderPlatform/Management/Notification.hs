{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.Notification
  ( getNotificationNotificationList,
    postNotificationNotificationRespond,
  )
where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Notification
import qualified DashboardAlert.Domain.Action.Dashboard.List
import DashboardAlert.Domain.Types.Audience (Topic (..))
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "lib-dashboard" SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.DashboardTopic as DTopic

getNotificationNotificationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow DashboardAlert.Domain.Action.Dashboard.List.NotificationListResp)
getNotificationNotificationList merchantShortId opCity apiTokenInfo mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (topic, _) <- DTopic.resolveTopicForPerson (const $ pure []) apiTokenInfo.personId
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.notificationDSL.getNotificationNotificationList) topic.getTopic mbLimit mbOffset

postNotificationNotificationRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.Management.Notification.RespondReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNotificationNotificationRespond merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    ( do
        (topic, _) <- DTopic.resolveTopicForPerson (const $ pure []) apiTokenInfo.personId
        API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.notificationDSL.postNotificationNotificationRespond) topic.getTopic req
    )
