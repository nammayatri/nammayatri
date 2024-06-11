{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Notification where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Notification
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import "lib-dashboard" Tools.Auth

data NotificationList = NotificationList {list :: [Domain.Types.Notification.Notification], summary :: API.Types.UI.Notification.Summary} deriving (Generic, ToJSON, FromJSON, ToSchema)

data NotificationReadRequest = NotificationReadRequest {id :: Data.Text.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

data Summary = Summary {count :: Kernel.Prelude.Int, totalCount :: Kernel.Prelude.Int} deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpdateRecieveNotificationStatusRequest = UpdateRecieveNotificationStatusRequest {readStatus :: Kernel.Prelude.Bool} deriving (Generic, ToJSON, FromJSON, ToSchema)

{-
	DSL Source Link: file://./../../../../spec/API/notification.yaml
-}
