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
    decrementCounterInTimeBucket,
    getCountForPeriod,
    buildCounterValues,
    buildCounterValuesWithEligible,
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
--   "bt:DRIVER:RIDE_CANCELLATION:ACTION_COUNT:driver-abc-123"
--   "bt:RIDER:BOOKING_CANCELLATION:ELIGIBLE_COUNT:rider-xyz-456"
mkCounterKey :: EntityType -> Text -> CounterType -> Text -> Text
mkCounterKey entityType actionType counterType entityId =
  "bt:" <> show entityType <> ":" <> actionType <> ":" <> show counterType <> ":" <> entityId

-- | Increment a sliding window counter
-- Uses CrossAppRedis to ensure counters are accessible across services
incrementCounter ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  EntityType ->
  Text -> -- actionType
  CounterType ->
  Text -> -- entityId
  Integer -> -- windowSizeDays
  m ()
incrementCounter entityType actionType counterType entityId windowSizeDays =
  Redis.runInMultiCloudRedisWrite $
    Redis.withCrossAppRedis $
      SWC.incrementWindowCount
        (mkCounterKey entityType actionType counterType entityId)
        (SWC.SlidingWindowOptions windowSizeDays SWC.Days)

-- | Decrement a sliding window counter in the bucket of the original event time.
-- Mirror of incrementCounter, for retroactive corrections: an event voided later
-- (e.g. a sent request cancelled by the customer) is removed from the bucket it
-- was originally counted in, keeping daily slices accurate.
decrementCounterInTimeBucket ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  EntityType ->
  Text -> -- actionType
  CounterType ->
  Text -> -- entityId
  UTCTime -> -- when the event being reversed was originally counted
  Integer -> -- windowSizeDays
  m ()
decrementCounterInTimeBucket entityType actionType counterType entityId eventTime windowSizeDays =
  Redis.runInMultiCloudRedisWrite $
    Redis.withCrossAppRedis $
      SWC.decrementByValueInTimeBucket
        eventTime
        1
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
  Text -> -- actionType
  CounterType ->
  Text -> -- entityId
  Integer -> -- periodDays: how many days to look back
  Integer -> -- windowSizeDays: SWC storage window
  m Integer
getCountForPeriod entityType actionType counterType entityId periodDays windowSizeDays =
  Redis.runInMultiCloudRedisWrite $
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
  Text -> -- actionType
  Text -> -- entityId
  Integer -> -- periodDays
  Integer -> -- windowSizeDays
  m CounterValues
buildCounterValues entityType actionType entityId periodDays windowSizeDays =
  buildCounterValuesWithEligible entityType actionType Nothing entityId periodDays windowSizeDays

-- | Like buildCounterValues, but the ELIGIBLE_COUNT (rate denominator) may be read
-- from a different actionType. Use when several outcome-specific action types share
-- one total-eligibility counter (e.g. QUOTE_RESPONSE_ACCEPT / REJECT / PULL all
-- rated against a shared QUOTE_RESPONSE eligibility).
buildCounterValuesWithEligible ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  EntityType ->
  Text -> -- actionType for ACTION_COUNT
  Maybe Text -> -- actionType for ELIGIBLE_COUNT (Nothing = same as actionType)
  Text -> -- entityId
  Integer -> -- periodDays
  Integer -> -- windowSizeDays
  m CounterValues
buildCounterValuesWithEligible entityType actionType mbEligibleActionType entityId periodDays windowSizeDays = do
  actionCnt <- getCountForPeriod entityType actionType ACTION_COUNT entityId periodDays windowSizeDays
  eligibleCnt <- getCountForPeriod entityType (fromMaybe actionType mbEligibleActionType) ELIGIBLE_COUNT entityId periodDays windowSizeDays
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
