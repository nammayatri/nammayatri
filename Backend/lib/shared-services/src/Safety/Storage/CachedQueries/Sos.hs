{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Safety.Storage.CachedQueries.Sos
  ( findByRideId,
    clearCache,
    clearAllCacheKeys,
    cacheSosIdByRideId,
    updateStatusToNotResolvedIfPendingByRideId,
  )
where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.Domain.Types.Common as Common
import qualified Safety.Domain.Types.Sos as DSos
import qualified Safety.Storage.Beam.Sos as Beam
import qualified Safety.Storage.BeamFlow as BeamFlow
import qualified Safety.Storage.Queries.Sos as QSos
import qualified Sequelize as Se

findByRideId :: (BeamFlow.BeamFlow m r) => Id Common.Ride -> m (Maybe DSos.Sos)
findByRideId rideId = do
  Hedis.safeGet (makeIdKey rideId) >>= \case
    Just sos -> pure (Just sos)
    Nothing -> do
      mbSos <- QSos.findByRideId (Just rideId)
      whenJust mbSos $ \sos -> cacheSosIdByRideId rideId sos
      pure mbSos

cacheSosIdByRideId :: (CacheFlow m r) => Id Common.Ride -> DSos.Sos -> m ()
cacheSosIdByRideId rideId sos = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey rideId
  Hedis.setExp idKey sos expTime

makeIdKey :: Id Common.Ride -> Text
makeIdKey rideId = "CachedQueries:Sos:RideId-" <> rideId.getId

clearCache :: (CacheFlow m r) => Id Common.Ride -> m ()
clearCache rideId = Hedis.runInMultiCloudRedisWrite $ Hedis.del $ makeIdKey rideId

clearAllCacheKeys :: (CacheFlow m r) => DSos.Sos -> m ()
clearAllCacheKeys sos = whenJust sos.rideId clearCache

updateStatusToNotResolvedIfPendingByRideId :: (BeamFlow.BeamFlow m r) => Id Common.Ride -> m ()
updateStatusToNotResolvedIfPendingByRideId rideId = do
  mbSos <-
    findOneWithKV
      [ Se.And
          [ Se.Is Beam.rideId $ Se.Eq (Just rideId.getId),
            Se.Is Beam.status $ Se.Eq DSos.Pending
          ]
      ]
  whenJust mbSos $ \sos -> do
    _now <- getCurrentTime
    updateWithKV
      [Se.Set Beam.status DSos.NotResolved, Se.Set Beam.updatedAt _now]
      [ Se.And
          [ Se.Is Beam.rideId $ Se.Eq (Just rideId.getId),
            Se.Is Beam.status $ Se.Eq DSos.Pending
          ]
      ]
    clearAllCacheKeys sos
