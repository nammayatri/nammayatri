module Tools.EventTracking
  ( TrackingEvent (..),
    trackEvent,
  )
where

import Data.Aeson (Value, object, (.=))
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Kernel.External.EventTracking as EventTracking
import Kernel.External.EventTracking.Moengage.Types
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)

data TrackingEvent
  = FirstRideCompleted {riderId :: Text}
  | RideCompleted {riderId :: Text, rideCount :: Int}
  | DriverAssigned {riderId :: Text}
  | UserSearched
      { riderId :: Text,
        fromLat :: Double,
        fromLon :: Double,
        toLat :: Maybe Double,
        toLon :: Maybe Double
      }
  | UserRequestedQuotes {riderId :: Text, quoteCount :: Int}

-- | Send event to all configured event tracking providers for this merchant.
-- Looks up all MerchantServiceConfigs, filters for EventTrackingServiceConfig,
-- and calls each provider in parallel. Failures are logged but never propagated.
trackEvent ::
  ServiceFlow m r =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TrackingEvent ->
  m ()
trackEvent merchantId merchantOperatingCityId event = do
  allConfigs <-
    getConfig
      ( MerchantServiceConfigDimensions
          { merchantOperatingCityId = merchantOperatingCityId.getId,
            merchantId = merchantId.getId,
            serviceName = Nothing
          }
      )
  let eventTrackingConfigs = mapMaybe extractEventTrackingConfig allConfigs
  if null eventTrackingConfigs
    then logDebug "EventTracking: no providers configured, skipping event"
    else forM_ eventTrackingConfigs $ \msc -> do
      let (customerId, actionName, attrs) = eventToAction event
          req =
            MoengageEventReq
              { _type = "event",
                customer_id = customerId,
                actions = [MoengageAction {action = actionName, attributes = attrs}]
              }
      result <- try @_ @SomeException $ EventTracking.pushEvent msc req
      case result of
        Left err -> logError $ "EventTracking event " <> actionName <> " failed: " <> show err
        Right _ -> logDebug $ "EventTracking event " <> actionName <> " sent for customer " <> customerId

extractEventTrackingConfig :: DMSC.MerchantServiceConfig -> Maybe EventTracking.EventTrackingServiceConfig
extractEventTrackingConfig cfg = case cfg.serviceConfig of
  DMSC.EventTrackingServiceConfig msc -> Just msc
  _ -> Nothing

eventToAction :: TrackingEvent -> (Text, Text, Value)
eventToAction = \case
  FirstRideCompleted {..} ->
    (riderId, "ny_user_first_ride_completed", object [])
  RideCompleted {..} ->
    (riderId, "ny_rider_ride_completed", object ["ride_count" .= rideCount])
  DriverAssigned {..} ->
    (riderId, "driver_assigned", object [])
  UserSearched {..} ->
    ( riderId,
      "ny_user_source_and_destination",
      object $
        ["from_lat" .= fromLat, "from_lon" .= fromLon]
          <> maybe [] (\lat -> ["to_lat" .= lat]) toLat
          <> maybe [] (\lon -> ["to_lon" .= lon]) toLon
    )
  UserRequestedQuotes {..} ->
    (riderId, "ny_user_request_quotes", object ["quote_count" .= quoteCount])
