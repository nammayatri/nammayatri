module Tools.MarketingEvents where

import qualified Data.Text as T
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person as Person
import Domain.Types.VehicleCategory
import qualified EulerHS.Prelude hiding (null)
import qualified Kernel.External.Notification.FCM.Flow as FCM
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Cac.TransporterConfig
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.Person as SQP
import Tools.Error
import Tools.Notifications
import Utils.Common.Cac.KeyNameConstants

data MarketingEventsType
  = NEW_SIGNUP
  | FIRST_RIDE_COMPLETED
  deriving (Show, ToJSON, FromJSON, Generic)

data EventDestination = CLEVERTAP | FIREBASE deriving (Show, ToJSON, FromJSON, Generic)

data PersonData = PersonId (Id Person) | PersonEntity Person

data MerchantOperatingCityData = MerchantOperatingCityId (Id DMOC.MerchantOperatingCity) | MerchantOperatingCityEntity DMOC.MerchantOperatingCity

notifyMarketingEvents :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => PersonData -> MarketingEventsType -> Maybe VehicleCategory -> MerchantOperatingCityData -> [EventDestination] -> m ()
notifyMarketingEvents driverData marketingEventsType vehicleCategory cityData eventDestination = fork "marketing-events" $ do
  (city, merchantOpCityId, shortId) <- case cityData of
    MerchantOperatingCityEntity city ->
      return (city.city, city.id, city.merchantShortId.getShortId)
    MerchantOperatingCityId id -> do
      mcity <- CQMOC.findById id >>= fromMaybeM (MerchantOperatingCityDoesNotExist id.getId)
      return (mcity.city, id, mcity.merchantShortId.getShortId)

  let events = case vehicleCategory of
        Just vehicleCat -> T.toLower $ shortId <> "driver" <> show marketingEventsType <> show vehicleCat <> show city
        _ -> T.toLower $ shortId <> "driver" <> show marketingEventsType <> show city
  logDebug $ "the event to be triggered is :" <> events
  case driverData of
    PersonId driverId -> do
      driver <- SQP.findById driverId
      whenJust driver $ \driver' -> do
        pingEvent driver' events merchantOpCityId
    PersonEntity driver -> pingEvent driver events merchantOpCityId
  where
    pingEvent :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Person.Person -> Text -> Id DMOC.MerchantOperatingCity -> m ()
    pingEvent driver events merchantOpCityId = do
      case driver.deviceToken of
        Just token -> do
          let newCityId = cityFallback driver.clientBundleVersion merchantOpCityId
          transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast driver.id))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
          FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) (clearDeviceToken driver.id) notificationData (FCMNotificationRecipient driver.id.getId (Just token)) EulerHS.Prelude.id
          where
            notificationData =
              FCM.FCMData
                { fcmNotificationType = FCM.MARKETING_EVENTS,
                  fcmShowNotification = FCM.DO_NOT_SHOW,
                  fcmEntityType = FCM.Person,
                  fcmEntityIds = getId driver.id,
                  fcmEntityData = eventDestination,
                  fcmNotificationJSON = FCM.createAndroidNotification title body FCM.MARKETING_EVENTS Nothing,
                  fcmOverlayNotificationJSON = Nothing,
                  fcmNotificationId = Nothing
                }
            title = FCM.FCMNotificationTitle events
            body =
              FCMNotificationBody ""
        Nothing -> log INFO $ "Active drivers with no token" <> show driver.id
