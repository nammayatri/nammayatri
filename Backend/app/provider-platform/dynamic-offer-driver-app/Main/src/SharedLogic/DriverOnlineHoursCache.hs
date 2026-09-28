{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverOnlineHoursCache
  ( markOnlineToday,
    markOfflineToday,
    getTodayOnlineDuration,
  )
where

import Data.Time (Day, DiffTime, UTCTime (..))
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common

data DriverOnlineHoursCache = DriverOnlineHoursCache
  { day :: Day,
    totalOnline :: Seconds,
    lastOnlineAt :: Maybe UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

mkOnlineHoursKey :: Text -> Text
mkOnlineHoursKey driverId = "driver-offer:OnlineHours:{" <> driverId <> "}"

onlineHoursKeyExpiry :: Redis.ExpirationTime
onlineHoursKeyExpiry = 345600

localDay :: Seconds -> UTCTime -> Day
localDay timeDiffFromUtc now = utctDay (addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now)

localMidnightUtc :: Seconds -> Day -> UTCTime
localMidnightUtc timeDiffFromUtc d = addUTCTime (negate (secondsToNominalDiffTime timeDiffFromUtc)) (UTCTime d 0)

diffUTCTimeInSeconds :: UTCTime -> UTCTime -> Seconds
diffUTCTimeInSeconds to from = Seconds (round $ diffUTCTime (roundUTCTimeToSecond to) (roundUTCTimeToSecond from))

roundUTCTimeToSecond :: UTCTime -> UTCTime
roundUTCTimeToSecond (UTCTime utcDay dt) = UTCTime utcDay (fromIntegral $ floor @DiffTime @Integer dt)

markOnlineToday :: (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Seconds -> m ()
markOnlineToday driverId timeDiffFromUtc = Redis.withCrossAppRedis $ do
  now <- getCurrentTime
  let today = localDay timeDiffFromUtc now
      key = mkOnlineHoursKey driverId.getId
  (mbCache :: Maybe DriverOnlineHoursCache) <- Redis.safeGet key
  let carriedOverTotal = case mbCache of
        Just cache | cache.day == today -> cache.totalOnline
        _ -> Seconds 0
  Redis.setExp key (DriverOnlineHoursCache {day = today, totalOnline = carriedOverTotal, lastOnlineAt = Just now}) onlineHoursKeyExpiry

markOfflineToday :: (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Seconds -> m ()
markOfflineToday driverId timeDiffFromUtc = Redis.withCrossAppRedis $ do
  now <- getCurrentTime
  let today = localDay timeDiffFromUtc now
      todayStart = localMidnightUtc timeDiffFromUtc today
      key = mkOnlineHoursKey driverId.getId
  (mbCache :: Maybe DriverOnlineHoursCache) <- Redis.safeGet key
  whenJust mbCache $ \cache ->
    whenJust cache.lastOnlineAt $ \lastOnlineAt -> do
      let effectiveStart = max lastOnlineAt todayStart
          priorTotal = if cache.day == today then cache.totalOnline else Seconds 0
          newTotalOnline = priorTotal + max (Seconds 0) (diffUTCTimeInSeconds now effectiveStart)
      Redis.setExp key (cache {day = today, totalOnline = newTotalOnline, lastOnlineAt = Nothing}) onlineHoursKeyExpiry

getTodayOnlineDuration :: (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Seconds -> m Seconds
getTodayOnlineDuration driverId timeDiffFromUtc = Redis.withCrossAppRedis $ do
  now <- getCurrentTime
  let today = localDay timeDiffFromUtc now
      todayStart = localMidnightUtc timeDiffFromUtc today
  (mbCache :: Maybe DriverOnlineHoursCache) <- Redis.safeGet (mkOnlineHoursKey driverId.getId)
  pure $ case mbCache of
    Nothing -> Seconds 0
    Just cache
      | cache.day == today ->
        cache.totalOnline + maybe (Seconds 0) (max (Seconds 0) . diffUTCTimeInSeconds now) cache.lastOnlineAt
      | otherwise ->
        case cache.lastOnlineAt of
          Just lastOnlineAt | lastOnlineAt < todayStart -> max (Seconds 0) (diffUTCTimeInSeconds now todayStart)
          _ -> Seconds 0
