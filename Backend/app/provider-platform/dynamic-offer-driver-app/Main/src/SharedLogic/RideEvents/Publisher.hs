-- | Publisher for RideEndedEvent on Redis Streams.
-- See Domain.Types.Event.RideEndedEvent for the schema.
--
-- Direct xAdd from the handler (not outbox). If the pod crashes between the EndRide DB
-- commit and the xAdd here, the event is lost — acceptable for best-effort side-effects.
-- For ledger / money-adjacent work that must not be lost, do NOT route through this publisher;
-- keep those synchronous on the main service.
module SharedLogic.RideEvents.Publisher
  ( publishRideEnded,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (hash)
import qualified Domain.Types.Booking as SRB
import Domain.Types.Event.RideEndedEvent
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RiderDetails as RD
import Environment (RideEventsPublisherCfg)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Storage.Hedis.Config (HedisFlow)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Kernel.Utils.Common (logError, logInfo)
import qualified Lib.Finance.Core.Types as Finance

-- | Best-effort publish: never throws. On failure, logs and returns.
publishRideEnded ::
  ( HedisFlow m r,
    HasField "rideEventsPublisherCfg" r (Maybe RideEventsPublisherCfg),
    Metrics.CoreMetrics m,
    Finance.HasActorInfo m r
  ) =>
  SRB.Booking ->
  Ride.Ride ->
  Maybe (Id RD.RiderDetails) ->
  Bool ->
  m ()
publishRideEnded booking ride mbRiderDetailsId isValid = do
  actorInfo <- asks (.actorInfo)
  mbCfg <- asks (.rideEventsPublisherCfg)
  case mbCfg of
    Nothing -> pure ()
    Just cfg -> do
      result <- try @_ @SomeException $ do
        now <- getCurrentTime
        let event = buildEvent now booking ride mbRiderDetailsId isValid actorInfo
        let rideIdT = ride.id.getId
        let shardId = hash rideIdT `mod` cfg.shardCount
        let streamName = cfg.streamPrefix <> show shardId
        let payload = BSL.toStrict (A.encode event)
        void $ Hedis.xAdd streamName "*" [("payload", payload)]
        logInfo $ "ride-events.published rideId=" <> rideIdT <> " stream=" <> streamName
      case result of
        Right () -> Metrics.incrementGenericMetrics "ride_events_published"
        Left e -> do
          logError $ "ride-events.publish-failed rideId=" <> ride.id.getId <> " err=" <> show e
          Metrics.incrementGenericMetrics "ride_events_publish_failed"

buildEvent ::
  UTCTime ->
  SRB.Booking ->
  Ride.Ride ->
  Maybe (Id RD.RiderDetails) ->
  Bool ->
  Finance.ActorInfo ->
  RideEndedEvent
buildEvent now booking ride mbRiderDetailsId isValid actorInfo =
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
      actorInfo = Just actorInfo
    }
