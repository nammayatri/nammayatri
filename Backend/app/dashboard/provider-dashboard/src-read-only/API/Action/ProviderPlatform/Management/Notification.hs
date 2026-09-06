{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Notification
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Notification
import qualified DashboardAlert.Domain.Action.Dashboard.List
import qualified Domain.Action.ProviderPlatform.Management.Notification
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("notification" :> (GetNotificationNotificationList :<|> PostNotificationNotificationRespond))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getNotificationNotificationList merchantId city :<|> postNotificationNotificationRespond merchantId city

type GetNotificationNotificationList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.NOTIFICATION) / ('API.Types.ProviderPlatform.Management.Notification.GET_NOTIFICATION_NOTIFICATION_LIST))
      :> API.Types.ProviderPlatform.Management.Notification.GetNotificationNotificationList
  )

type PostNotificationNotificationRespond =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.NOTIFICATION) / ('API.Types.ProviderPlatform.Management.Notification.POST_NOTIFICATION_NOTIFICATION_RESPOND))
      :> API.Types.ProviderPlatform.Management.Notification.PostNotificationNotificationRespond
  )

getNotificationNotificationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler DashboardAlert.Domain.Action.Dashboard.List.NotificationListResp)
getNotificationNotificationList merchantShortId opCity apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Notification.getNotificationNotificationList merchantShortId opCity apiTokenInfo mbLimit mbOffset

postNotificationNotificationRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Notification.RespondReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNotificationNotificationRespond merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Notification.postNotificationNotificationRespond merchantShortId opCity apiTokenInfo req
