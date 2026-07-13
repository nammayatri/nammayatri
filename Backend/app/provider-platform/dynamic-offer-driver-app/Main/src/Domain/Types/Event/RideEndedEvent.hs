-- | Domain event emitted from EndRide after the critical ride-completion block.
-- Published as a JSON payload on Redis Stream "ride.events.shard<N>" where
-- shardId = hash(rideId) mod shardCount.
--
-- Consumer side-effect handlers (kafka-consumers / Consumer.RideEvents.Processor)
-- deserialize this and fan out to Kafka pushes, FCM notifications, ML tag computation,
-- leaderboard updates, etc.
--
-- Schema versioning: v1. Add new optional fields only; never remove or rename — in-flight
-- consumers may still be reading the old shape.
module Domain.Types.Event.RideEndedEvent
  ( RideEndedEvent (..),
    schemaVersion,
  )
where

import Kernel.Prelude
import qualified Lib.Finance.Core.Types as Finance

-- | IDs and enums are carried as Text to keep the cross-package JSON contract simple.
-- Handlers convert back to typed Ids (`Id Ride`, `Id Person`, …) when needed.
data RideEndedEvent = RideEndedEvent
  { schemaVer :: Int,
    eventTimestamp :: UTCTime,
    rideId :: Text,
    bookingId :: Text,
    driverId :: Text,
    riderDetailsId :: Maybe Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    vehicleVariant :: Text,
    tripCategory :: Text,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    chargeableDistance :: Maybe Double,
    traveledDistance :: Double,
    distanceUnit :: Text,
    estimatedFare :: Double,
    finalFare :: Double,
    discount :: Maybe Double,
    customerExtraFee :: Maybe Double,
    currency :: Text,
    fleetOwnerId :: Maybe Text,
    isValidRide :: Bool,
    actorInfo :: Maybe Finance.ActorInfo
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

schemaVersion :: Int
schemaVersion = 1
