{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.DriverOnlineSessionEvent
  ( DriverOnlineSessionEvent (..),
    emitClosedSessionEvents,
    splitSessionAtMidnights,
  )
where

import Data.Time (Day, UTCTime)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnlineHoursCache as DriverOnlineHoursCache

-- | One closed driver online session, bounded to a single merchant-local day so
-- per-day queries stay a plain WHERE clause.
-- Field names are snake_case on purpose: they map 1:1 onto the ClickHouse columns of
-- atlas_kafka.DriverOnlineSession{,_queue} (JSONEachRow).
data DriverOnlineSessionEvent = DriverOnlineSessionEvent
  { driver_id :: Text,
    merchant_id :: Text,
    merchant_op_city_id :: Text,
    online_at :: UTCTime,
    offline_at :: UTCTime,
    duration_seconds :: Int,
    merchant_local_date :: Day,
    event_type :: Text
  }
  deriving (Generic, Show, ToJSON)

driverOnlineSessionTopic :: Text
driverOnlineSessionTopic = "driver-online-sessions"

-- | Emit a *closed* online session (onlineAt -> offlineAt) to Kafka for analytics,
-- split at merchant-local midnight(s) into per-day events. Each push happens in a
-- fork so a Kafka hiccup can never break the driver mode-change flow (mirrors the
-- SearchTryBatchData push in SharedLogic.DriverPool).
emitClosedSessionEvents ::
  MonadFlow m =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Seconds ->
  Maybe (UTCTime, UTCTime) ->
  m ()
emitClosedSessionEvents driverId merchantId merchantOpCityId timeDiffFromUtc mbSession =
  whenJust mbSession $ \(onlineAt, offlineAt) -> do
    let chunks = splitSessionAtMidnights timeDiffFromUtc onlineAt offlineAt
    forM_ chunks $ \(chunkDay, chunkOnlineAt, chunkOfflineAt, chunkDuration) ->
      fork "emit driver online session event" $ do
        pushToKafka
          DriverOnlineSessionEvent
            { driver_id = driverId.getId,
              merchant_id = merchantId.getId,
              merchant_op_city_id = merchantOpCityId.getId,
              online_at = chunkOnlineAt,
              offline_at = chunkOfflineAt,
              duration_seconds = chunkDuration,
              merchant_local_date = chunkDay,
              event_type = "SESSION_END"
            }
          driverOnlineSessionTopic
          driverId.getId

-- | Split [onlineAt, offlineAt) at merchant-local midnights into
-- (merchantLocalDay, chunkStart, chunkEnd, durationSeconds) chunks, one per local day.
splitSessionAtMidnights ::
  Seconds ->
  UTCTime ->
  UTCTime ->
  [(Day, UTCTime, UTCTime, Int)]
splitSessionAtMidnights timeDiffFromUtc onlineAt offlineAt = go onlineAt
  where
    go chunkStart
      | chunkStart >= offlineAt = []
      | otherwise =
        let currentDay = DriverOnlineHoursCache.localDay timeDiffFromUtc chunkStart
            dayEnd = DriverOnlineHoursCache.localMidnightUtc timeDiffFromUtc (succ currentDay)
            chunkEnd = min dayEnd offlineAt
            Seconds secs = DriverOnlineHoursCache.diffUTCTimeInSeconds chunkEnd chunkStart
         in (currentDay, chunkStart, chunkEnd, max 0 secs) : go chunkEnd
