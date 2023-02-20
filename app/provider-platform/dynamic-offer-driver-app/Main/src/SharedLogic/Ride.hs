module SharedLogic.Ride where

import Domain.Types.Person (Person)
import Domain.Types.Ride
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Ride as RQueries

makeAssignedRideIdAndStatusKey :: Id Person -> Text
makeAssignedRideIdAndStatusKey id = "RideAssignToDriver:IdAndStatus:DriverId-" <> id.getId

cacheAssignedRideIdAndStatus :: (CacheFlow m r) => Id Person -> (Id Ride, RideStatus) -> m ()
cacheAssignedRideIdAndStatus driverId rideIdAndStatus = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeAssignedRideIdAndStatusKey driverId) rideIdAndStatus expTime

clearCache :: (CacheFlow m r) => Id Person -> m ()
clearCache = Hedis.del . makeAssignedRideIdAndStatusKey 

getInProgressOrNewRideIdAndStatusByDriverId :: (CacheFlow m r, EsqDBReplicaFlow m r) => Id Person -> m (Maybe (Id Ride, RideStatus))
getInProgressOrNewRideIdAndStatusByDriverId driverId =
  Hedis.get (makeAssignedRideIdAndStatusKey driverId) >>= \case
    Just a ->
      return $ Just a
    Nothing -> flip whenJust (cacheAssignedRideIdAndStatus driverId) /=<< Esq.runInReplica (RQueries.getInProgressOrNewRideIdAndStatusByDriverId driverId)
