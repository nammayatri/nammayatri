{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Tools.EventTracking
  ( TrackingEvent (..),
    trackEvent,
  )
where

import Data.Aeson (object, (.=))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Kernel.External.EventTracking as EventTracking
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))

data TrackingEvent
  = FirstRideCompleted Text
  | RideCompleted Text Int
  | DriverAssigned Text
  | UserSearched Text Double Double (Maybe Double) (Maybe Double)
  | UserRequestedQuotes Text Int

trackEvent ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TrackingEvent ->
  m ()
trackEvent merchantId merchantOperatingCityId event = do
  mbMerchantConfig <- getConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) (Just (CQMSUC.findByMerchantOperatingCityId merchantOperatingCityId))
  case mbMerchantConfig of
    Nothing -> logDebug "EventTracking: MerchantServiceUsageConfig not found, skipping event"
    Just merchantConfig -> do
      let providers = merchantConfig.eventTrackingProviders
      if null providers
        then logDebug "EventTracking: no providers configured for merchantOperatingCity, skipping event"
        else do
          now <- getCurrentTime
          let (customerId, actionName, attrs) = eventToAction event
              req =
                EventTracking.EventTrackingReq
                  { EventTracking.customerId = customerId,
                    EventTracking.eventName = actionName,
                    EventTracking.attributes = attrs,
                    EventTracking.timestamp = Just now
                  }
          forM_ providers $ \provider ->
            sendToProvider merchantId merchantOperatingCityId provider actionName req

sendToProvider ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  EventTracking.EventTrackingService ->
  Text ->
  EventTracking.EventTrackingReq ->
  m ()
sendToProvider merchantId merchantOperatingCityId provider actionName req = do
  mbConfig <-
    getOneConfig
      ( MerchantServiceConfigDimensions
          { merchantOperatingCityId = merchantOperatingCityId.getId,
            merchantId = merchantId.getId,
            serviceName = Just (DMSC.EventTrackingService provider)
          }
      )
      (Just (maybeToList <$> CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.EventTrackingService provider)))
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
  FirstRideCompleted riderId ->
    (riderId, "ny_user_first_ride_completed", object [])
  RideCompleted riderId rideCount ->
    (riderId, "ny_rider_ride_completed", object ["ride_count" .= rideCount])
  DriverAssigned riderId ->
    (riderId, "driver_assigned", object [])
  UserSearched riderId fromLat fromLon toLat toLon ->
    ( riderId,
      "ny_user_source_and_destination",
      object $
        ["from_lat" .= fromLat, "from_lon" .= fromLon]
          <> maybe [] (\lat -> ["to_lat" .= lat]) toLat
          <> maybe [] (\lon -> ["to_lon" .= lon]) toLon
    )
  UserRequestedQuotes riderId quoteCount ->
    (riderId, "ny_user_request_quotes", object ["quote_count" .= quoteCount])
