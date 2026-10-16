module Tools.EventTracking
  ( TrackingEvent (..),
    trackEvent,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Map.Strict as Map
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Kernel.External.EventTracking as EventTracking
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))

-- | Driver-side events sent to external analytics providers (Clevertap, ...).
--
-- Add a constructor here and a case in 'eventToAction'; routing, credentials
-- and delivery are shared by every event.
data TrackingEvent
  = DriverSignedUp Text Text -- driverId, city
  | DriverFirstRideCompleted Text (Maybe Text) -- driverId, vehicleCategory
  | DriverRideCompleted Text Text (Maybe Text) -- driverId, rideId, vehicleCategory
  | DriverRideCancelled Text Text Text -- driverId, rideId, cancelledBy

-- | Sends the event to every provider enabled for the city in
-- @merchant_service_usage_config.event_tracking_providers@.
--
-- Delivery failures are logged, never thrown, so tracking cannot break the
-- calling flow. Does not fork; call it from within a fork.
trackEvent ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  TrackingEvent ->
  m ()
trackEvent merchantOpCityId event = do
  mbUsageConfig <- getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
  case mbUsageConfig of
    Nothing -> logDebug "EventTracking: MerchantServiceUsageConfig not found, skipping event"
    Just usageConfig -> do
      let providers = usageConfig.eventTrackingProviders
      if null providers
        then logDebug "EventTracking: no providers configured for merchantOperatingCity, skipping event"
        else do
          now <- getCurrentTime
          let (customerId, actionName, attrs) = eventToAction event
              -- An event absent from the overrides map goes to every provider
              -- enabled in this city. When present, the override can only narrow
              -- that list, never activate a provider the city has not enabled.
              targetProviders =
                case Map.lookup actionName =<< usageConfig.eventTrackingOverrides of
                  Nothing -> providers
                  Just allowed -> filter (`elem` allowed) providers
              req =
                EventTracking.EventTrackingReq
                  { EventTracking.customerId = customerId,
                    EventTracking.eventName = actionName,
                    EventTracking.attributes = attrs,
                    EventTracking.timestamp = Just now,
                    EventTracking.appInstanceId = Nothing,
                    EventTracking.platform = Nothing
                  }
          forM_ targetProviders $ \provider ->
            sendToProvider merchantOpCityId provider actionName req

sendToProvider ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  EventTracking.EventTrackingService ->
  Text ->
  EventTracking.EventTrackingReq ->
  m ()
sendToProvider merchantOpCityId provider actionName req = do
  mbConfig <-
    getOneConfig
      ( MerchantServiceConfigDimensions
          { merchantOperatingCityId = merchantOpCityId.getId,
            merchantId = Nothing,
            serviceName = Just (DMSC.EventTrackingService provider)
          }
      )
      Nothing
  case mbConfig of
    Nothing ->
      logWarning $ "EventTracking: provider " <> show provider <> " listed in usage config but no service config row found, skipping"
    Just config -> case config.serviceConfig of
      DMSC.EventTrackingServiceConfig msc -> do
        result <- try @_ @SomeException $ EventTracking.pushEvent msc req
        case result of
          Left err -> logError $ "EventTracking event " <> actionName <> " (" <> show provider <> ") failed: " <> show err
          Right _ -> logDebug $ "EventTracking event " <> actionName <> " (" <> show provider <> ") sent"
      _ -> logError "Unexpected service config type for EventTracking"

eventToAction :: TrackingEvent -> (Text, Text, Value)
eventToAction = \case
  DriverSignedUp driverId city ->
    (driverId, "ny_driver_signed_up", object ["city" .= city, "identifiers" .= driverIdentifiers driverId])
  DriverFirstRideCompleted driverId vehicleCategory ->
    (driverId, "ny_driver_first_ride_completed", object (["identifiers" .= driverIdentifiers driverId] <> maybe [] (\cat -> ["vehicle_category" .= cat]) vehicleCategory))
  DriverRideCompleted driverId rideId vehicleCategory ->
    (driverId, "ny_driver_ride_completed", object (["identifiers" .= driverIdentifiers driverId, "ride_id" .= rideId] <> maybe [] (\cat -> ["vehicle_category" .= cat]) vehicleCategory))
  DriverRideCancelled driverId rideId cancelledBy ->
    (driverId, "ny_driver_ride_cancelled", object ["identifiers" .= driverIdentifiers driverId, "ride_id" .= rideId, "cancelled_by" .= cancelledBy])

-- | Providers that link an event to a profile (Breeze) read this object from
-- @properties.identifiers@. Only types the provider's tenant has declared count,
-- so add a key here only after the tenant declares it (today: driver_id, phone,
-- email; phone and email are not sent).
driverIdentifiers :: Text -> Value
driverIdentifiers driverId = object ["driver_id" .= driverId]
