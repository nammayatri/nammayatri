{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

getInProgressOrNewRideIdAndStatusByDriverId :: forall m r. (CacheFlow m r, EsqDBReplicaFlow m r) => Id Person -> m (Maybe (Id Ride, RideStatus))
getInProgressOrNewRideIdAndStatusByDriverId driverId =
  Hedis.get (makeAssignedRideIdAndStatusKey driverId) >>= \case
    Just a ->
      return $ Just a
    Nothing -> flip whenJust (cacheAssignedRideIdAndStatus driverId) /=<< Esq.runInReplica (RQueries.getInProgressOrNewRideIdAndStatusByDriverId driverId (Proxy @m))
