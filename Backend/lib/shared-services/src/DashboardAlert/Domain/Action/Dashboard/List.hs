{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module DashboardAlert.Domain.Action.Dashboard.List
  ( NotificationItem (..),
    NotificationListResp (..),
    RespondReq (..),
    listNotifications,
    respondToNotification,
  )
where

import DashboardAlert.Domain.Types.Audience
import DashboardAlert.Domain.Types.DashboardAlert
import DashboardAlert.Storage.BeamFlow
import qualified DashboardAlert.Storage.Queries.DashboardAlert as QDA
import Data.Aeson
import Domain.Types.Alert (castAlertRequestTypeToCategory)
import qualified Domain.Types.Alert.AlertCategory as DAlertCategory
import qualified Domain.Types.Alert.AlertEntityType as DAlertEntity
import qualified Domain.Types.Alert.AlertRequestData as DAlertData
import qualified Domain.Types.Alert.AlertRequestStatus as DAlertStatus
import qualified Domain.Types.Alert.AlertRequestType as DAlertType
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common

defaultLimit :: Int
defaultLimit = 20

maxLimit :: Int
maxLimit = 100

data NotificationItem = NotificationItem
  { id :: Text,
    category :: DAlertCategory.AlertCategory,
    title :: Text,
    body :: Text,
    status :: DAlertStatus.AlertRequestStatus,
    requestType :: DAlertType.AlertRequestType,
    requestData :: DAlertData.AlertRequestData,
    entityId :: Maybe Text,
    entityType :: Maybe DAlertEntity.AlertEntityType,
    reason :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data NotificationListResp = NotificationListResp
  { notifications :: [NotificationItem],
    summary :: Summary
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Summary = Summary
  { totalCount :: Int,
    count :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data RespondReq = RespondReq
  { notificationId :: Text,
    status :: DAlertStatus.AlertRequestStatus,
    reason :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

listNotifications ::
  BeamFlow m r =>
  Topic ->
  Maybe Int ->
  Maybe Int ->
  m NotificationListResp
listNotifications topic mbLimit mbOffset = do
  let limit = min maxLimit (fromMaybe defaultLimit mbLimit)
      offset = fromMaybe 0 mbOffset
  alerts <- QDA.findAllByRequesteeId (Just limit) (Just offset) (Id topic.getTopic)
  pure
    NotificationListResp
      { notifications = map toItem alerts,
        summary = Summary {totalCount = length alerts, count = length alerts}
      }

respondToNotification :: BeamFlow m r => Topic -> RespondReq -> m ()
respondToNotification topic req = do
  let alertId = Id req.notificationId
  alert <- QDA.findByPrimaryKey alertId >>= fromMaybeM (InvalidRequest "Notification not found")
  unless (alert.requesteeId.getId == topic.getTopic) $
    throwError (InvalidRequest "Notification does not belong to this channel")
  when (alert.status /= DAlertStatus.AWAITING_APPROVAL) $
    throwError (InvalidRequest "Notification already processed")
  QDA.updateStatusWithReason req.status req.reason alertId

toItem :: DashboardAlert -> NotificationItem
toItem alert =
  NotificationItem
    { id = alert.id.getId,
      category = castAlertRequestTypeToCategory alert.requestType,
      title = alert.title,
      body = alert.body,
      status = alert.status,
      requestType = alert.requestType,
      requestData = alert.requestData,
      entityId = alert.entityId,
      entityType = alert.entityType,
      reason = alert.reason,
      createdAt = alert.createdAt
    }
