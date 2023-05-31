{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use join" #-}

module Domain.Action.UI.LocationUpdates
  ( UpdateLocationReq,
    updateLocationHandler,
  )
where

import qualified Data.List.NonEmpty as NE
import Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Kernel.Utils.GenericPretty (PrettyShow)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import Storage.Queries.Booking as QRB
import Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide

type UpdateLocationReq = NonEmpty Waypoint

data Waypoint = Waypoint
  { pt :: LatLong, -- point
    ts :: UTCTime, -- timestamp
    acc :: Maybe Double -- accuracy, optional for now
  }
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema, PrettyShow)

data DriverLocationUpdateStreamData = DriverLocationUpdateStreamData
  { rId :: Text, -- riderId
    mId :: Text, -- merchantId
    ts :: UTCTime, -- timestamp
    pt :: LatLong, -- lat log
    rs :: Maybe DRide.RideStatus, -- rideStatus
    rdId :: Maybe (Id DRide.Ride) --rideId
  }
  deriving (Generic, FromJSON, ToJSON)

updateLocationHandler ::
  ( Redis.HedisFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["riderLocationUpdateTopic" ::: Text],
    CacheFlow m r,
    HasField "minAccuracy" r Double
  ) =>
  Id Person.Person ->
  UpdateLocationReq ->
  m APISuccess
updateLocationHandler riderId waypoints = withLogTag ("riderId-" <> riderId.getId) $ do
  logInfo $ "got location updates: " <> getId riderId <> " " <> encodeToText waypoints
  minAcc <- asks (.minAccuracy)
  rider <- runInReplica $ QP.findById riderId >>= fromMaybeM (PersonNotFound $ getId riderId)
  ride <- runMaybeT $ do
    booking <- MaybeT . runInReplica $ QRB.findAssignedByRiderId riderId
    MaybeT . runInReplica $ QRide.findActiveByRBId booking.id
  case filterNewWaypoints minAcc of
    [] -> logWarning "Incoming points are older than current one, ignoring"
    (a : ax) -> do
      fork "updating in kafka" $
        mapM_
          ( \point -> do
              streamLocationUpdates riderId point.pt point.ts rider.merchantId (ride <&> (.status)) (ride <&> (.id))
          )
          (a : ax)
      let newWaypoints = a :| ax
      addPointsImplementation riderId (NE.map (.pt) newWaypoints)
  pure Success
  where
    filterNewWaypoints minAcc = do
      let sortedWaypoint = toList $ NE.sortWith (.ts) waypoints
      filter (\val -> fromMaybe 0.0 val.acc <= minAcc) sortedWaypoint

makeWaypointsRedisKey :: Id Person.Person -> Text
makeWaypointsRedisKey riderId = mconcat ["waypoints", ":", riderId.getId]

addPointsImplementation :: (Redis.HedisFlow m env, MonadTime m) => Id Person.Person -> NonEmpty LatLong -> m ()
addPointsImplementation riderId waypoints = do
  let key = makeWaypointsRedisKey riderId
      numPoints = length waypoints
  Redis.rPush key waypoints
  logInfo $ mconcat ["added ", show numPoints, " points for driverId = ", riderId.getId]

streamLocationUpdates ::
  ( MonadFlow m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["riderLocationUpdateTopic" ::: Text]
  ) =>
  Id Person.Person ->
  LatLong ->
  UTCTime ->
  Id DM.Merchant ->
  Maybe DRide.RideStatus ->
  Maybe (Id DRide.Ride) ->
  m ()
streamLocationUpdates riderId point timestamp merchantId rideStatus rideId = do
  topicName <- asks (.riderLocationUpdateTopic)
  produceMessage
    (topicName, Just (encodeUtf8 $ getId riderId))
    (DriverLocationUpdateStreamData (getId riderId) (getId merchantId) timestamp point rideStatus rideId)
