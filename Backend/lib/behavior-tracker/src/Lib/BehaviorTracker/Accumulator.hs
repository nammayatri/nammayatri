{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.BehaviorTracker.Accumulator
  ( mkCounterKey,
    incrementCounter,
    getCountForPeriod,
    buildCounterValues,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Lib.BehaviorTracker.Types

-- | Unified Redis key scheme
-- Format: "bt:{entityType}:{actionType}:{counterType}:{entityId}"
-- Examples:
--   "bt:DRIVER:DRIVER_RIDE_CANCELLATION:ACTION_COUNT:driver-abc-123"
--   "bt:RIDER:RIDER_BOOKING_CANCELLATION:ELIGIBLE_COUNT:rider-xyz-456"
mkCounterKey :: EntityType -> ActionType -> CounterType -> Text -> Text
mkCounterKey entityType actionType counterType entityId =
  "bt:" <> show entityType <> ":" <> show actionType <> ":" <> show counterType <> ":" <> entityId

-- | Increment a sliding window counter
-- Uses CrossAppRedis to ensure counters are accessible across services
incrementCounter ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  EntityType ->
  ActionType ->
  CounterType ->
  Text -> -- entityId
  Integer -> -- windowSizeDays
  m ()
incrementCounter entityType actionType counterType entityId windowSizeDays =
  Redis.withCrossAppRedis $
    SWC.incrementWindowCount
      (mkCounterKey entityType actionType counterType entityId)
      (SWC.SlidingWindowOptions windowSizeDays SWC.Days)

-- | Get the count for a specific period within the window
-- periodDays: how many days to look back (e.g. 1 for daily, 7 for weekly)
-- windowSizeDays: total window size for the SWC storage
getCountForPeriod ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  EntityType ->
  ActionType ->
  CounterType ->
  Text -> -- entityId
  Integer -> -- periodDays: how many days to look back
  Integer -> -- windowSizeDays: SWC storage window
  m Integer
getCountForPeriod entityType actionType counterType entityId periodDays windowSizeDays =
  Redis.withCrossAppRedis $ do
    values <-
      SWC.getCurrentWindowValuesUptoLast
        periodDays
        (mkCounterKey entityType actionType counterType entityId)
        (SWC.SlidingWindowOptions windowSizeDays SWC.Days)
    return $ sum $ map (fromMaybe 0) values

-- | Build CounterValues for a given period
-- Fetches both ACTION_COUNT and ELIGIBLE_COUNT for the period and computes the rate
buildCounterValues ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  EntityType ->
  ActionType ->
  Text -> -- entityId
  Integer -> -- periodDays
  Integer -> -- windowSizeDays
  m CounterValues
buildCounterValues entityType actionType entityId periodDays windowSizeDays = do
  actionCnt <- getCountForPeriod entityType actionType ACTION_COUNT entityId periodDays windowSizeDays
  eligibleCnt <- getCountForPeriod entityType actionType ELIGIBLE_COUNT entityId periodDays windowSizeDays
  let computedRate =
        if eligibleCnt > 0
          then (actionCnt * 100) `div` eligibleCnt
          else 0
  return $
    CounterValues
      { actionCount = actionCnt,
        eligibleCount = eligibleCnt,
        rate = computedRate
      }
