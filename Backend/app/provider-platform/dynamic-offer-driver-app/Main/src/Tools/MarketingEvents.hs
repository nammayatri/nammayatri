module Tools.MarketingEvents where

import qualified Data.Text as T
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person as Person
import Domain.Types.VehicleCategory
import qualified EulerHS.Prelude hiding (null)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Notifications

data MarketingEventsType
  = NEW_SIGNUP
  | FIRST_RIDE_COMPLETED
  deriving (Show, ToJSON, FromJSON, Generic)

data EventDestination = CLEVERTAP | FIREBASE deriving (Show, ToJSON, FromJSON, Generic)

data MerchantOperatingCityData = MerchantOperatingCityId (Id DMOC.MerchantOperatingCity) | MerchantOperatingCityEntity DMOC.MerchantOperatingCity

notifyMarketingEvents :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["maxNotificationShards" ::: Int], HasKafkaProducer r) => Id Person -> Maybe FCMRecipientToken -> MarketingEventsType -> Maybe VehicleCategory -> MerchantOperatingCityData -> [EventDestination] -> m ()
notifyMarketingEvents driverId deviceToken marketingEventsType vehicleCategory cityData eventDestination = fork "marketing-events" $ do
  (city, merchantOpCityId, shortId) <- case cityData of
    MerchantOperatingCityEntity city ->
      return (city.city, city.id, city.merchantShortId.getShortId)
    MerchantOperatingCityId id -> do
      mcity <- CQMOC.findById id >>= fromMaybeM (MerchantOperatingCityDoesNotExist id.getId)
      return (mcity.city, id, mcity.merchantShortId.getShortId)

  let events = case vehicleCategory of
        Just vehicleCat -> T.toLower $ (splitFirstChars shortId) <> "_" <> show marketingEventsType <> "_" <> show vehicleCat <> "_" <> T.take 3 (T.pack (show city))
        _ -> T.toLower $ (splitFirstChars shortId) <> "_" <> show marketingEventsType <> "_" <> T.take 3 (T.pack (show city))
  logDebug $ "the event to be triggered is :" <> events

  notification <- setNotificationData events merchantOpCityId driverId eventDestination deviceToken
  driver <- QPerson.findById driverId
  runWithServiceConfigForProviders merchantOpCityId (driver >>= (.clientId)) (driver >>= (.clientDevice)) notification EulerHS.Prelude.id (clearDeviceToken driverId)
  where
    splitFirstChars :: T.Text -> T.Text
    splitFirstChars shortId = T.concat $ map (T.take 1) $ T.splitOn "_" shortId

setNotificationData ::
  (Monad m, Log m) =>
  Text ->
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  [EventDestination] ->
  Maybe FCM.FCMRecipientToken ->
  m (Notification.NotificationReq [EventDestination] EmptyDynamicParam)
setNotificationData events merchantOpCityId driverId eventDestination deviceToken = do
  let notificationData =
        Notification.NotificationReq
          { category = Notification.MARKETING_EVENTS,
            subCategory = Nothing,
            showNotification = Notification.DO_NOT_SHOW,
            messagePriority = Just Notification.HIGH,
            entity = Notification.Entity Notification.Merchant merchantOpCityId.getId eventDestination,
            dynamicParams = EmptyDynamicParam,
            body = "",
            title = events,
            auth = Notification.Auth driverId.getId ((.getFCMRecipientToken) <$> deviceToken) Nothing,
            ttl = Nothing,
            sound = Nothing
          }
  logDebug $ "the notification is :" <> show notificationData
  return notificationData
