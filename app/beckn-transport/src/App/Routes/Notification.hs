module App.Routes.Notification where

import App.Types
import Product.Notification
import Servant
import Types.API.Notification
import Types.App
import Utils.Common

type NotificationAPI =
  "notification"
    :> ( TokenAuth
           :> Capture "rideId" RideId
           :> Get '[JSON] NotificationDetailResponse
       )

notificationFlow :: FlowServer NotificationAPI
notificationFlow = getNotificationDetail
