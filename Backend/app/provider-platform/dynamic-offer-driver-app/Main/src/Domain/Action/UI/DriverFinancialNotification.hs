module Domain.Action.UI.DriverFinancialNotification
  ( listNotifications,
    markAsRead,
    markAllAsRead,
    getUnreadCount,
    updatePreferences,
    NotificationListResp (..),
    UnreadCountResp (..),
    NotificationPreferencesReq (..),
    NotificationPreference (..),
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.DriverFinancialNotification as DFN
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common (logInfo)
import qualified Storage.Queries.DriverFinancialNotification as QDFN

-- | Response type for notification listing
data NotificationListResp = NotificationListResp
  { notifications :: [DFN.DriverFinancialNotification],
    totalCount :: Int,
    unreadCount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Response type for unread count
data UnreadCountResp = UnreadCountResp
  { count :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Notification preference for a single category
data NotificationPreference = NotificationPreference
  { category :: DFN.DriverNotificationCategory,
    enabled :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Request type for updating notification preferences
data NotificationPreferencesReq = NotificationPreferencesReq
  { preferences :: [NotificationPreference]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | List notifications for a driver
listNotifications ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Maybe Int ->
  Maybe Int ->
  Flow NotificationListResp
listNotifications (mbDriverId, _merchantId, _merchantOpCityId) _mbLimit _mbOffset = do
  driverId <- mbDriverId & maybe (throwError $ InvalidRequest "Driver ID required") pure
  logInfo $ "Listing financial notifications for driver: " <> driverId.getId
  allNotifications <- QDFN.findAllByDriverId driverId
  let unread = filter (\n -> not n.isRead) allNotifications
  pure $
    NotificationListResp
      { notifications = allNotifications,
        totalCount = length allNotifications,
        unreadCount = length unread
      }

-- | Mark a single notification as read
markAsRead ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Id DFN.DriverFinancialNotification ->
  Flow APISuccess
markAsRead (mbDriverId, _merchantId, _merchantOpCityId) notificationId = do
  driverId <- mbDriverId & maybe (throwError $ InvalidRequest "Driver ID required") pure
  logInfo $ "Marking notification as read: " <> notificationId.getId <> " for driver: " <> driverId.getId
  QDFN.updateIsReadById True notificationId
  pure Success

-- | Mark all notifications as read for a driver
markAllAsRead ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Flow APISuccess
markAllAsRead (mbDriverId, _merchantId, _merchantOpCityId) = do
  driverId <- mbDriverId & maybe (throwError $ InvalidRequest "Driver ID required") pure
  logInfo $ "Marking all notifications as read for driver: " <> driverId.getId
  QDFN.updateIsReadByDriverId True driverId
  pure Success

-- | Get unread notification count for a driver
getUnreadCount ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Flow UnreadCountResp
getUnreadCount (mbDriverId, _merchantId, _merchantOpCityId) = do
  driverId <- mbDriverId & maybe (throwError $ InvalidRequest "Driver ID required") pure
  logInfo $ "Getting unread notification count for driver: " <> driverId.getId
  unreadNotifications <- QDFN.findAllByDriverIdAndIsRead driverId False
  pure $ UnreadCountResp {count = length unreadNotifications}

-- | Update notification preferences for a driver
-- Note: This is a placeholder that logs the preferences.
-- Full implementation requires a DriverNotificationPreference storage table.
updatePreferences ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  NotificationPreferencesReq ->
  Flow APISuccess
updatePreferences (mbDriverId, _merchantId, _merchantOpCityId) req = do
  driverId <- mbDriverId & maybe (throwError $ InvalidRequest "Driver ID required") pure
  logInfo $ "Updating notification preferences for driver: " <> driverId.getId <> " with " <> show (length req.preferences) <> " preferences"
  -- TODO: Persist preferences once DriverNotificationPreference table is created
  pure Success
