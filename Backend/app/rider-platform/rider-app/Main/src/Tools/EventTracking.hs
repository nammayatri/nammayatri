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
import qualified Kernel.External.EventTracking.Moengage.Types as MT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
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
  let (customerId, actionName, attrs) = eventToAction event
      logCtx = " event=" <> actionName <> " merchantId=" <> merchantId.getId <> " merchantOpCityId=" <> merchantOperatingCityId.getId
  mbMerchantConfig <- getConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) (Just (CQMSUC.findByMerchantOperatingCityId merchantOperatingCityId))
  case mbMerchantConfig of
    Nothing -> logInfo $ "EventTracking skipped:" <> logCtx <> " reason=MerchantServiceUsageConfig not found for this operating city"
    Just merchantConfig -> do
      let providers = merchantConfig.eventTrackingProviders
      if null providers
        then logInfo $ "EventTracking skipped:" <> logCtx <> " reason=no providers in usage config (eventTrackingProviders is empty)"
        else do
          let req =
                MT.MoengageEventReq
                  { MT._type = "event",
                    MT.customer_id = customerId,
                    MT.actions = [MT.MoengageAction {MT.action = actionName, MT.attributes = attrs}]
                  }
          forM_ providers $ \provider ->
            sendToProvider merchantId merchantOperatingCityId provider actionName req

sendToProvider ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  EventTracking.EventTrackingService ->
  Text ->
  MT.MoengageEventReq ->
  m ()
sendToProvider merchantId merchantOperatingCityId provider actionName req = do
  let logCtx = " event=" <> actionName <> " provider=" <> show provider <> " merchantId=" <> merchantId.getId <> " merchantOpCityId=" <> merchantOperatingCityId.getId
  mbConfig <-
    getOneConfig
      ( MerchantServiceConfigDimensions
          { merchantOperatingCityId = merchantOperatingCityId.getId,
            merchantId = merchantId.getId,
            serviceName = Just (DMSC.EventTrackingService provider)
          }
      )
      Nothing
  case mbConfig of
    Nothing ->
      logInfo $ "EventTracking skipped:" <> logCtx <> " reason=provider listed in usage config but no service config row found (not configured for this merchant/city)"
    Just config -> case config.serviceConfig of
      DMSC.EventTrackingServiceConfig msc -> do
        result <- try @_ @SomeException $ EventTracking.pushEvent msc req
        case result of
          Left err -> logError $ "EventTracking failed:" <> logCtx <> " error=" <> show err
          Right (Just _) -> logInfo $ "EventTracking sent:" <> logCtx
          Right Nothing -> logInfo $ "EventTracking skipped:" <> logCtx <> " reason=provider disabled (enabled=false in service config)"
      _ -> logError $ "EventTracking failed:" <> logCtx <> " reason=unexpected service config type (not EventTrackingServiceConfig)"

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
