-- | When to ask HDFC about a batch, and what to do with its answer. The schedule is a pure function
-- of the plan and the submit time, so it can be tested without a database or network.
module Lib.Payment.Payout.Bulk.Schedule
  ( checkTimes,
    firstCheckAt,
    nextCheck,
    CallOutcome (..),
    NextStep (..),
    nextStepAfterCall,
    ensureCityJob,
    batchLockKey,
  )
where

import qualified Data.Set as Set
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import Lib.Payment.Payout.Bulk.Types (Handle (..))

--------------------------------------------------------------------------------
-- The schedule
--------------------------------------------------------------------------------

-- | Every check time for a batch submitted at @t@: the first day's burst, then one per later day.
--   All are offsets from @t@, so there are no time zones and a test can shrink them to seconds.
--   Sorted and deduplicated, because 'nextCheck' walks the list by position: a custom plan with a
--   tail gap under 12 h would otherwise place tail checks after later burst checks and skip them.
checkTimes :: Payout.BulkStatusCheckPlan -> UTCTime -> [UTCTime]
checkTimes plan t = Set.toAscList (Set.fromList (burst <> tailChecks))
  where
    burst =
      [ addUTCTime (minutes (plan.firstCheckDelayMinutes + i * plan.burstGapMinutes)) t
        | i <- [0 .. plan.burstChecks - 1]
      ]
    tailChecks = [addUTCTime (minutes (plan.tailGapMinutes * (k + 1))) t | k <- [1 .. plan.tailChecks]]

-- | When the first check of a batch submitted at @t@ is due.
firstCheckAt :: Payout.BulkStatusCheckPlan -> UTCTime -> UTCTime
firstCheckAt plan t = addUTCTime (minutes plan.firstCheckDelayMinutes) t

-- | The check after check number @done@: the first planned time still far enough away. Missed
--   times are skipped, not fired back to back, so an outage does not burn the day's call budget.
--   'Nothing' means the plan is finished and the batch needs a human.
nextCheck :: Payout.BulkStatusCheckPlan -> UTCTime -> Int -> UTCTime -> Maybe (Int, UTCTime)
nextCheck plan submittedAt done now =
  find farEnough . drop done . zip [1 ..] $ checkTimes plan submittedAt
  where
    farEnough (_, plannedAt) = plannedAt >= addUTCTime (minutes (plan.burstGapMinutes `div` 2)) now

--------------------------------------------------------------------------------
-- One call
--------------------------------------------------------------------------------

-- | What HDFC said, as far as the schedule cares.
data CallOutcome
  = -- | An answer (rows, "no data" or a refusal); this check is done.
    Answered
  | -- | "We have accepted your request. Please enquire again after sometime."
    NotReady
  | -- | No usable answer: a timeout, a connection failure, an unreadable body.
    CallFailed
  deriving (Eq, Show)

data NextStep
  = -- | Call again this many seconds from now, still inside this check.
    CallAgainIn NominalDiffTime
  | -- | This check is over; move to the next one in the plan.
    EndCheck
  deriving (Eq, Show)

-- | What to do after one call; @calls@ includes this one. Every call counts, answered or not, so a
--   batch stays under HDFC's daily ceiling ('Payout.sanitizeBulkStatusCheckPlan' caps the product).
--   A failed call is retried in the same check only while calls are left.
nextStepAfterCall :: Payout.BulkStatusCheckPlan -> Int -> CallOutcome -> NextStep
nextStepAfterCall plan calls outcome
  | calls >= plan.maxCallsPerCheck = EndCheck
  | otherwise = case outcome of
    Answered -> EndCheck
    NotReady -> CallAgainIn (seconds plan.readCallDelaySeconds)
    CallFailed -> CallAgainIn (seconds plan.errorRetryDelaySeconds)

--------------------------------------------------------------------------------
-- The city's job
--------------------------------------------------------------------------------

-- | The lock held by a batch's status check and by the admin release-hold and re-check APIs, so no
--   two of them work on the same batch at the same time.
batchLockKey :: Handle m -> Id DPayoutBatch.PayoutBatch -> Text
batchLockKey h batchId = h.keyPrefix <> "BulkPayoutStatusCheck:batch:" <> batchId.getId

-- | Make sure the city has its status-check job; called whenever a batch is created there. The lock
--   stops two batches created at the same moment from both creating one.
ensureCityJob :: (Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, MonadMask m) => Handle m -> Text -> Text -> m ()
ensureCityJob h merchantId cityId =
  Redis.whenWithLockRedis (h.keyPrefix <> "BulkPayoutStatusCheck:create:" <> cityId) 60 $
    Redis.runInMasterCloudRedisCell (h.createCityStatusCheckJob merchantId cityId)

minutes :: Int -> NominalDiffTime
minutes m = fromIntegral m * 60

seconds :: Int -> NominalDiffTime
seconds = fromIntegral
