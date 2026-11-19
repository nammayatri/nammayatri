-- | Publisher for RideEndedEvent. Each configured sink (Redis Stream, Kafka) is published
-- independently; a failure on one never affects the other or ride completion.
-- See Domain.Types.Event.RideEndedEvent for the schema.
module SharedLogic.RideEvents.Publisher
  ( publishRideEnded,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (hash)
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Booking as SRB
import Domain.Types.Event.RideEndedEvent
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RiderDetails as RD
import Environment (RideEventsKafkaCfg, RideEventsPublisherCfg, RideEventsRedisCfg)
import qualified Environment as Fields
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Storage.Hedis.Config (HedisFlow)
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common (HighPrecMoney, MonadTime (getCurrentTime))
import Kernel.Types.Id
import Kernel.Utils.Common (logError, logInfo)
import qualified Lib.Finance.Core.Types as Finance

publishRideEnded ::
  ( HedisFlow m r,
    HasKafkaProducer r,
    HasField "rideEventsPublisherCfg" r (Maybe RideEventsPublisherCfg),
    Metrics.CoreMetrics m,
    Finance.HasActorInfo m r
  ) =>
  SRB.Booking ->
  Ride.Ride ->
  Maybe (Id RD.RiderDetails) ->
  Bool ->
  HighPrecMoney ->
  m ()
publishRideEnded booking ride mbRiderDetailsId isValid fareBeforePaymentCharge = do
  actorInfo <- asks (.actorInfo)
  mbCfg <- asks (.rideEventsPublisherCfg)
  whenJust mbCfg $ \cfg -> do
    now <- getCurrentTime
    let event = buildEvent now booking ride mbRiderDetailsId isValid actorInfo fareBeforePaymentCharge
    whenJust cfg.redisConfig $ publishToRedis ride.id.getId event
    whenJust cfg.kafkaConfig $ publishToKafka ride.id.getId event

publishToRedis ::
  (HedisFlow m r, Metrics.CoreMetrics m) =>
  Text ->
  RideEndedEvent ->
  RideEventsRedisCfg ->
  m ()
publishToRedis rideIdT event cfg = do
  result <- try @_ @SomeException $ do
    let shardId = hash rideIdT `mod` cfg.shardCount
    let streamName = cfg.streamPrefix <> show shardId
    void $ Hedis.xAdd streamName "*" [("payload", BSL.toStrict (A.encode event))]
    logInfo $ "ride-events.published rideId=" <> rideIdT <> " stream=" <> streamName
  case result of
    Right () -> Metrics.incrementGenericMetrics "ride_events_published"
    Left e -> do
      logError $ "ride-events.publish-failed rideId=" <> rideIdT <> " err=" <> show e
      Metrics.incrementGenericMetrics "ride_events_publish_failed"

publishToKafka ::
  (HedisFlow m r, HasKafkaProducer r, Metrics.CoreMetrics m) =>
  Text ->
  RideEndedEvent ->
  RideEventsKafkaCfg ->
  m ()
publishToKafka rideIdT event cfg = do
  result <- try @_ @SomeException $ do
    produceMessage (cfg.kafkaTopic, Just (TE.encodeUtf8 rideIdT)) event
    logInfo $ "ride-events.kafka-published rideId=" <> rideIdT <> " topic=" <> cfg.kafkaTopic
  case result of
    Right () -> Metrics.incrementGenericMetrics "ride_events_kafka_published"
    Left e -> do
      logError $ "ride-events.kafka-publish-failed rideId=" <> rideIdT <> " err=" <> show e
      Metrics.incrementGenericMetrics "ride_events_kafka_publish_failed"

buildEvent ::
  UTCTime ->
  SRB.Booking ->
  Ride.Ride ->
  Maybe (Id RD.RiderDetails) ->
  Bool ->
  Finance.ActorInfo ->
  HighPrecMoney ->
  RideEndedEvent
buildEvent now booking ride mbRiderDetailsId isValid actorInfo fareBeforePaymentCharge =
  RideEndedEvent
    { schemaVer = schemaVersion,
      eventTimestamp = now,
      rideId = ride.id.getId,
      bookingId = booking.id.getId,
      driverId = ride.driverId.getId,
      riderDetailsId = getId <$> mbRiderDetailsId,
      merchantId = booking.providerId.getId,
      merchantOperatingCityId = booking.merchantOperatingCityId.getId,
      vehicleVariant = show booking.vehicleServiceTier,
      tripCategory = show booking.tripCategory,
      rideStartTime = ride.tripStartTime,
      rideEndTime = ride.tripEndTime,
      chargeableDistance = realToFrac <$> ride.chargeableDistance,
      traveledDistance = realToFrac ride.traveledDistance,
      distanceUnit = show booking.distanceUnit,
      estimatedFare = realToFrac booking.estimatedFare,
      finalFare = maybe (realToFrac booking.estimatedFare) realToFrac ride.fare,
      discount = Nothing,
      customerExtraFee = Nothing,
      currency = show booking.currency,
      fleetOwnerId = getId <$> ride.fleetOwnerId,
      isValidRide = isValid,
      actorInfo = Just actorInfo,
      fareBeforePaymentCharge = Just fareBeforePaymentCharge
    }
