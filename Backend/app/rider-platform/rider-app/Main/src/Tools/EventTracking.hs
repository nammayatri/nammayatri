{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Tools.EventTracking
  ( TrackingEvent (..),
    trackEvent,
    trackVehicleRideCompletedEvents,
    trackPTBookingUsedEvents,
    trackPTBookingConfirmedEvents,
  )
where

import qualified BecknV2.OnDemand.Enums as BecknEnums
import Data.Aeson (object, (.=))
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Kernel.External.EventTracking as EventTracking
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))

data TrackingEvent
  = FirstRideCompleted Text
  | RideCompleted Text Int HighPrecMoney -- riderId, overallRideCount, fare
  | DriverAssigned Text
  | UserSearched Text Double Double (Maybe Double) (Maybe Double)
  | UserRequestedQuotes Text Int
  | UserOnboarded Text Text Text -- riderId, signupMethod, city
  | VehicleRideCompleted Text Text Int HighPrecMoney -- riderId, categorySlug (cab|auto|bike), catRideCount, fare
  | VehicleFirstRideCompleted Text Text HighPrecMoney -- riderId, categorySlug, fare
  | VehicleFifthRideCompleted Text Text HighPrecMoney -- riderId, categorySlug, fare
  | PTBookingConfirmed Text Text (Maybe Text) Int HighPrecMoney -- riderId, vehicleType, serviceTierType, purchaseCount, fare
  | PTFirstBookingConfirmed Text Text (Maybe Text) HighPrecMoney -- riderId, vehicleType, serviceTierType, fare
  | PTBookingUsed Text Text (Maybe Text) Int HighPrecMoney -- riderId, vehicleType, serviceTierType, purchaseCount, fare
  | PTFirstBookingUsed Text Text (Maybe Text) HighPrecMoney -- riderId, vehicleType, serviceTierType, fare
  | OfferCashbackCredited Text Text HighPrecMoney -- riderId, payoutRequestId, amount

trackEvent ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TrackingEvent ->
  m ()
trackEvent merchantId merchantOperatingCityId event = do
  mbMerchantConfig <- getConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing
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
              -- An event absent from the overrides map goes to every provider
              -- live in this city. When present, the override can only narrow
              -- that list -- never activate a provider that has no service
              -- config row -- so the city-level list stays the source of truth.
              targetProviders =
                case Map.lookup actionName =<< merchantConfig.eventTrackingOverrides of
                  Nothing -> providers
                  Just allowed -> filter (`elem` allowed) providers
              skipped = filter (`notElem` targetProviders) providers
              -- Providers the build supports but this city has not enabled.
              -- Without this, a provider that is simply absent from
              -- eventTrackingProviders produces no log line at all, and its
              -- silence is indistinguishable from a delivery failure.
              notEnabled = filter (`notElem` providers) EventTracking.availableEventTrackingServices
          unless (null notEnabled) $
            logDebug $ "EventTracking: providers not enabled for this merchantOperatingCity: " <> show notEnabled
          forM_ skipped $ \provider ->
            logDebug $ "EventTracking: event " <> actionName <> " not routed to " <> show provider <> " by override config"
          forM_ targetProviders $ \provider ->
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
  FirstRideCompleted riderId ->
    (riderId, "ny_user_first_ride_completed", object [])
  RideCompleted riderId rideCount fare ->
    (riderId, "ny_rider_ride_completed", object ["ride_count" .= rideCount, "fare" .= fare])
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
  UserOnboarded riderId signupMethod city ->
    (riderId, "ny_user_onboarded", object ["signup_method" .= signupMethod, "city" .= city])
  VehicleRideCompleted riderId slug catRideCount fare ->
    (riderId, "ny_" <> slug <> "_ride_completed", object ["ride_count" .= catRideCount, "fare" .= fare])
  VehicleFirstRideCompleted riderId slug fare ->
    (riderId, "ny_" <> slug <> "_first_ride_completed", object ["fare" .= fare])
  VehicleFifthRideCompleted riderId slug fare ->
    (riderId, "ny_" <> slug <> "_user_5_ride_completed", object ["fare" .= fare])
  OfferCashbackCredited riderId payoutRequestId amount ->
    (riderId, "ny_user_offer_cashback_credited", object ["payout_request_id" .= payoutRequestId, "amount" .= amount])
  PTBookingConfirmed riderId vehicleType tier purchaseCount fare ->
    (riderId, "pt_booking_confirmed", object (["vehicleType" .= vehicleType, "purchaseCount" .= purchaseCount, "fare" .= fare] <> serviceTier tier))
  PTFirstBookingConfirmed riderId vehicleType tier fare ->
    (riderId, "pt_first_booking_confirmed", object (["vehicleType" .= vehicleType, "fare" .= fare] <> serviceTier tier))
  PTBookingUsed riderId vehicleType tier purchaseCount fare ->
    (riderId, "pt_booking_used", object (["vehicleType" .= vehicleType, "purchaseCount" .= purchaseCount, "fare" .= fare] <> serviceTier tier))
  PTFirstBookingUsed riderId vehicleType tier fare ->
    (riderId, "pt_first_booking_used", object (["vehicleType" .= vehicleType, "fare" .= fare] <> serviceTier tier))
  where
    serviceTier = maybe [] (\tier -> ["serviceTierType" .= tier])

-- | Per-vehicle ride-completion events (+ first/fifth milestones). No-op for
-- unsupported categories. Does not fork; call it from within a fork.
trackVehicleRideCompletedEvents ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  BecknEnums.VehicleCategory ->
  Int ->
  HighPrecMoney ->
  m ()
trackVehicleRideCompletedEvents merchantId merchantOperatingCityId riderId vehicleCat catRideCount fare =
  forM_ (vehicleCategorySlug vehicleCat) $ \vehicleSlug -> do
    let eventRiderId = getId riderId
    trackEvent merchantId merchantOperatingCityId (VehicleRideCompleted eventRiderId vehicleSlug catRideCount fare)
    when (catRideCount == 1) $
      trackEvent merchantId merchantOperatingCityId (VehicleFirstRideCompleted eventRiderId vehicleSlug fare)
    when (catRideCount == 5) $
      trackEvent merchantId merchantOperatingCityId (VehicleFifthRideCompleted eventRiderId vehicleSlug fare)

-- | Slug for segmented categories; 'Nothing' for the rest (generic event only).
vehicleCategorySlug :: BecknEnums.VehicleCategory -> Maybe Text
vehicleCategorySlug = \case
  BecknEnums.CAB -> Just "cab"
  BecknEnums.AUTO_RICKSHAW -> Just "auto"
  BecknEnums.MOTORCYCLE -> Just "bike"
  _ -> Nothing

trackPTBookingUsedEvents ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Text ->
  Maybe Text ->
  HighPrecMoney ->
  Int ->
  m ()
trackPTBookingUsedEvents merchantId merchantOperatingCityId riderId vehicleType serviceTierType fare purchaseCount = do
  let eventRiderId = getId riderId
  trackEvent merchantId merchantOperatingCityId (PTBookingUsed eventRiderId vehicleType serviceTierType purchaseCount fare)
  when (purchaseCount == 1) $
    trackEvent merchantId merchantOperatingCityId (PTFirstBookingUsed eventRiderId vehicleType serviceTierType fare)

trackPTBookingConfirmedEvents ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Text ->
  Maybe Text ->
  HighPrecMoney ->
  Int ->
  Bool ->
  m ()
trackPTBookingConfirmedEvents merchantId merchantOperatingCityId riderId vehicleType serviceTierType fare purchaseCount isFirstPurchase = do
  let eventRiderId = getId riderId
  trackEvent merchantId merchantOperatingCityId (PTBookingConfirmed eventRiderId vehicleType serviceTierType purchaseCount fare)
  when isFirstPurchase $
    trackEvent merchantId merchantOperatingCityId (PTFirstBookingConfirmed eventRiderId vehicleType serviceTierType fare)
