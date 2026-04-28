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
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getOneConfig)

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
  mbConfig <-
    getOneConfig
      ( MerchantServiceConfigDimensions
          { merchantOperatingCityId = merchantOperatingCityId.getId,
            merchantId = merchantId.getId,
            serviceName = Just (DMSC.EventTrackingService EventTracking.Moengage)
          }
      )
  case mbConfig of
    Nothing -> logDebug "EventTracking: not configured, skipping event"
    Just config -> do
      case config.serviceConfig of
        DMSC.EventTrackingServiceConfig msc -> do
          let (customerId, actionName, attrs) = eventToAction event
              req =
                MT.MoengageEventReq
                  { MT._type = "event",
                    MT.customer_id = customerId,
                    MT.actions = [MT.MoengageAction {MT.action = actionName, MT.attributes = attrs}]
                  }
          result <- try @_ @SomeException $ EventTracking.pushEvent msc req
          case result of
            Left err -> logError $ "EventTracking event " <> actionName <> " failed: " <> show err
            Right _ -> logDebug $ "EventTracking event " <> actionName <> " sent"
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
