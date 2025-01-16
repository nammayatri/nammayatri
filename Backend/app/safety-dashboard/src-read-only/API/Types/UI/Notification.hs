{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Notification where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Notification
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant hiding (Summary)
import "lib-dashboard" Tools.Auth

data NotificationList = NotificationList {list :: [Domain.Types.Notification.Notification], summary :: Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NotificationReadRequest = NotificationReadRequest {id :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Summary = Summary {count :: Kernel.Prelude.Int, totalCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateRecieveNotificationStatusRequest = UpdateRecieveNotificationStatusRequest {readStatus :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
