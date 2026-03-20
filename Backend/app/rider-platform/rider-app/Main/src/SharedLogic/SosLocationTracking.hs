{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SosLocationTracking where

import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
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

updateSosRiderLocation ::
  (HedisFlow m env) =>
  Id SafetyDSos.Sos ->
  LatLong ->
  Maybe Double ->
  Maybe UTCTime ->
  m ()
updateSosRiderLocation sosId LatLong {..} mbAccuracy mbExpiryTimeStamp = do
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

getSosRiderLocation ::
  (HedisFlow m env) =>
  Id SafetyDSos.Sos ->
  m (Maybe SosLocationData)
getSosRiderLocation sosId = do
  let key = makeSosLocationRedisKey sosId
  Redis.safeGet key

clearSosRiderLocation ::
  (HedisFlow m env) =>
  Id SafetyDSos.Sos ->
  m ()
clearSosRiderLocation sosId = do
  let key = makeSosLocationRedisKey sosId
  Redis.del key
