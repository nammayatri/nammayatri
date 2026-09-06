{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module DashboardAlert.Trigger
  ( AlertContent (..),
    triggerRealtime,
    triggerPersist,
  )
where

import DashboardAlert.Domain.Types.Audience
import DashboardAlert.Domain.Types.Common (Merchant, MerchantOperatingCity, Person)
import DashboardAlert.Domain.Types.DashboardAlert
import DashboardAlert.ServiceHandle
import DashboardAlert.Storage.BeamFlow
import qualified DashboardAlert.Storage.Queries.DashboardAlert as QDA
import DashboardAlert.Topic (audienceTopic)
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as AKM
import qualified Domain.Types.Alert.AlertCategory as DAlertCategory
import qualified Domain.Types.Alert.AlertEntityType as DAlertEntity
import qualified Domain.Types.Alert.AlertRequestData as DAlertData
import qualified Domain.Types.Alert.AlertRequestStatus as DAlertStatus
import qualified Domain.Types.Alert.AlertRequestType as DAlertType
import qualified Kernel.External.Notification.GRPC.Flow as GRPC
import qualified Kernel.External.Notification.GRPC.Types as GRPC
import qualified Kernel.External.Notification.Interface.Types as Notification
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common

data AlertContent = AlertContent
  { category :: DAlertCategory.AlertCategory,
    title :: Text,
    body :: Text,
    entityId :: Text,
    entityType :: DAlertEntity.AlertEntityType,
    entityData :: Value,
    requestType :: DAlertType.AlertRequestType,
    requestData :: DAlertData.AlertRequestData,
    requestorId :: Id Person,
    requestorType :: RequestorType,
    requesteeType :: RequesteeType,
    requiresAction :: Bool,
    visibility :: Notification.ShowNotification,
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    ttlSeconds :: Seconds
  }

type TriggerFlow m r =
  ( MonadFlow m,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  )

triggerRealtime ::
  TriggerFlow m r =>
  ServiceHandle m ->
  [Audience] ->
  AlertContent ->
  m ()
triggerRealtime svc audiences content = do
  cfg <- svc.getGRPCConfig content.merchantOperatingCityId
  now <- getCurrentTime
  forM_ audiences $ \audience -> do
    notificationId <- generateGUID
    publishToTopic svc.platform cfg content now notificationId (audienceTopic audience)

triggerPersist ::
  (TriggerFlow m r, BeamFlow m r) =>
  ServiceHandle m ->
  [Audience] ->
  AlertContent ->
  m [Id DashboardAlert]
triggerPersist svc audiences content = do
  cfg <- svc.getGRPCConfig content.merchantOperatingCityId
  now <- getCurrentTime
  forM audiences $ \audience -> do
    alertId <- generateGUID
    let topic = audienceTopic audience
    QDA.create (buildAlert content now alertId topic)
    publishToTopic svc.platform cfg content now alertId.getId topic
    pure alertId

publishToTopic ::
  TriggerFlow m r =>
  AlertPlatform ->
  GRPC.GRPCConfig ->
  AlertContent ->
  UTCTime ->
  Text ->
  Topic ->
  m ()
publishToTopic platform cfg content now notificationId topic = do
  let expiresAt = addUTCTime (fromIntegral content.ttlSeconds.getSeconds) now
  GRPC.notifyPerson cfg (buildNotificationData platform content expiresAt notificationId topic)

buildNotificationData ::
  AlertPlatform ->
  AlertContent ->
  UTCTime ->
  Text ->
  Topic ->
  GRPC.GrpcNotificationData Value
buildNotificationData platform content expiresAt notificationId topic =
  GRPC.GrpcNotificationData
    { entityId = content.entityId,
      entityType = show content.entityType,
      entityData = withPlatform platform content.entityData,
      category = show content.category,
      title = GRPC.GRPCNotificationTitle content.title,
      body = GRPC.GRPCNotificationBody content.body,
      showNotification = show content.visibility,
      ttl = expiresAt,
      streamId = topic.getTopic,
      notificationId = notificationId
    }

buildAlert ::
  AlertContent ->
  UTCTime ->
  Id DashboardAlert ->
  Topic ->
  DashboardAlert
buildAlert content now alertId topic =
  DashboardAlert
    { id = alertId,
      requestorId = content.requestorId,
      requestorType = content.requestorType,
      requesteeId = Id topic.getTopic,
      requesteeType = content.requesteeType,
      requestType = content.requestType,
      requestData = content.requestData,
      entityId = Just content.entityId,
      entityType = Just content.entityType,
      title = content.title,
      body = content.body,
      status =
        if content.requiresAction
          then DAlertStatus.AWAITING_APPROVAL
          else DAlertStatus.TRIGGERED,
      reason = Nothing,
      createdAt = now,
      updatedAt = now,
      merchantId = content.merchantId,
      merchantOperatingCityId = content.merchantOperatingCityId
    }

platformLabel :: AlertPlatform -> Text
platformLabel = \case
  DriverPlatform -> "DRIVER"
  RiderPlatform -> "RIDER"

withPlatform :: AlertPlatform -> Value -> Value
withPlatform p = \case
  Object o -> Object (AKM.insert "platform" (String (platformLabel p)) o)
  other -> other
