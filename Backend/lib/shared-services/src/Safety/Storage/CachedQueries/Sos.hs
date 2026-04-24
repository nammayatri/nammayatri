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
    mockSosKey,
    clearMockDrill,
    setMockDrill,
    sosTrackingHitsCountKey,
    erssStatusUpdateHitsCountKey,
    mkExternalSOSTraceKey,
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

-- | Redis key for the mock safety drill entity owned by a person.
-- Value stored at this key is 'Safety.Domain.Types.Sos.SosMockDrill'.
mockSosKey :: Id Common.Person -> Text
mockSosKey personId = "mock-sos-" <> personId.getId

-- | Delete the mock drill Redis entry (used on real SOS create, etc.).
clearMockDrill :: (CacheFlow m r) => Id Common.Person -> m ()
clearMockDrill personId = Hedis.del (mockSosKey personId)

-- | Set / refresh the mock drill Redis entry. TTL matches rider's existing value (13400s ~ 3h 43m).
setMockDrill :: (CacheFlow m r) => Id Common.Person -> DSos.SosMockDrill -> m ()
setMockDrill personId drill = Hedis.setExp (mockSosKey personId) drill 13400

-- | Rate-limit key for the /sos/{sosId}/tracking GET endpoint.
sosTrackingHitsCountKey :: Id DSos.Sos -> Text
sosTrackingHitsCountKey sosId = "SosTrackingHits:" <> sosId.getId <> ":hitsCount"

-- | Rate-limit key for the /sos/erss/statusUpdate webhook (global, not per-SOS).
erssStatusUpdateHitsCountKey :: Text
erssStatusUpdateHitsCountKey = "ErssStatusUpdateHits:hitsCount"

-- | Cache key for dedup/polling of external SOS trace pings.
mkExternalSOSTraceKey :: Id DSos.Sos -> Text
mkExternalSOSTraceKey sosId = "SOS:ExternalTrace:" <> sosId.getId
