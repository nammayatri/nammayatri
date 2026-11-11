{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

-- | Valid rides per rider and city, as a sorted set of ride ids scored by ride
-- end time. Feeds the campaign-block counts in the reward eligibility context
-- ('validRidesThisCampaignDay' / 'validRidesThisCampaignWeek'), which need
-- exact timestamps (blocks start at the campaign's start, not at midnight) and
-- valid rides only, unlike the per-UTC-day completed counters in
-- 'SharedLogic.BehaviourManagement.CustomerCancellationRate'.
--
-- Members are ride ids, so recording the same ride twice (a repeated
-- ride-completed callback) does not double-count. Rides completed before this
-- was deployed are not in the set.
module Tools.Rewards.ValidRideLog
  ( recordValidRide,
    countValidRidesBetween,
  )
where

import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common

-- | Longest block is 7 days; keep a margin so a block that started a few days
-- before the oldest ride still counts correctly.
retentionDays :: Int
retentionDays = 30

mkValidRidesKey :: Id DMOC.MerchantOperatingCity -> Id DP.Person -> Text
mkValidRidesKey moCityId personId =
  "rewards:valid-rides:" <> moCityId.getId <> ":" <> personId.getId

toScore :: UTCTime -> Double
toScore = realToFrac . utcTimeToPOSIXSeconds

recordValidRide ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  Id DRide.Ride ->
  UTCTime ->
  m ()
recordValidRide personId moCityId rideId rideEndTime = Redis.withCrossAppRedis $ do
  now <- getCurrentTime
  let key = mkValidRidesKey moCityId personId
      cutoff = addUTCTime (negate (fromIntegral retentionDays * 86400)) now
  Redis.zAdd key [(toScore rideEndTime, rideId.getId)]
  void $ Redis.zRemRangeByScore key 0 (toScore cutoff)
  Redis.expire key (retentionDays * 86400)

-- | Valid rides in the rider's city with end time in [from, to], both inclusive.
countValidRidesBetween ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m Int
countValidRidesBetween personId moCityId from to = Redis.withCrossAppRedis $ do
  count <- Redis.zCount (mkValidRidesKey moCityId personId) (toScore from) (toScore to)
  pure $ fromIntegral count
