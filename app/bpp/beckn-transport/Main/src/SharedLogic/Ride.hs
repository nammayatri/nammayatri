module SharedLogic.Ride where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Beckn.Storage.Esqueleto.Transactionable as Esq
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.Ride
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Ride as RQueries

makeAssignedRideKey :: Id Person -> Text
makeAssignedRideKey id = "RideAssignToDriver:DriverId-" <> id.getId

cacheAssignedRide :: (CacheFlow m r) => Id Person -> Id Ride -> m ()
cacheAssignedRide driverId rideId = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeAssignedRideKey driverId) rideId expTime

clearCache :: (CacheFlow m r) => Id Person -> m ()
clearCache = Hedis.del . makeAssignedRideKey

getInProgressRideIdByDriverId :: (CacheFlow m r, EsqDBReplicaFlow m r) => Id Person -> m (Maybe (Id Ride))
getInProgressRideIdByDriverId driverId =
  Hedis.get (makeAssignedRideKey driverId) >>= \case
    Just a ->
      return $ Just a
    Nothing -> flip whenJust (cacheAssignedRide driverId) /=<< Esq.runInReplica (RQueries.getInProgressRideIdByDriverId driverId)
