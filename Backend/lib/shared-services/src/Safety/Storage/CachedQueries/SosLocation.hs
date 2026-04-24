{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Redis-backed location tracking for active SOS sessions. Shared between
-- rider and driver platforms — previously lived at rider's
-- @SharedLogic.SosLocationTracking@; moved here so the unified SOS handlers can
-- call it directly without an app-specific callback.
module Safety.Storage.CachedQueries.SosLocation
  ( SosLocationData (..),
    makeSosLocationRedisKey,
    updateSosLocation,
    getSosLocation,
    clearSosLocation,
  )
where

import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.Domain.Types.Sos as SafetyDSos

data SosLocationData = SosLocationData
  { lat :: Double,
    lon :: Double,
    timestamp :: UTCTime,
    accuracy :: Maybe Double
  }
  deriving (Generic, Show, ToJSON, FromJSON)

makeSosLocationRedisKey :: Id SafetyDSos.Sos -> Text
makeSosLocationRedisKey sosId = "sos-rider-location:" <> sosId.getId

updateSosLocation ::
  (CacheFlow m r) =>
  Id SafetyDSos.Sos ->
  LatLong ->
  Maybe Double ->
  Maybe UTCTime ->
  m ()
updateSosLocation sosId LatLong {..} mbAccuracy mbExpiryTimeStamp = do
  now <- getCurrentTime
  let locationData =
        SosLocationData
          { lat,
            lon,
            timestamp = now,
            accuracy = mbAccuracy
          }
  let key = makeSosLocationRedisKey sosId
  let expirySeconds = maybe 3600 (\expiry -> max 1 $ floor $ diffUTCTime expiry now) mbExpiryTimeStamp
  Redis.setExp key locationData expirySeconds

getSosLocation ::
  (CacheFlow m r) =>
  Id SafetyDSos.Sos ->
  m (Maybe SosLocationData)
getSosLocation sosId = Redis.safeGet (makeSosLocationRedisKey sosId)

clearSosLocation ::
  (CacheFlow m r) =>
  Id SafetyDSos.Sos ->
  m ()
clearSosLocation sosId = Redis.del (makeSosLocationRedisKey sosId)
