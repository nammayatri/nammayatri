-- NOTE (reviewer, remove before merge): NEW module, not on main (bulk lifecycle: the timing of the
--   "status check" step). Holds the status-check plan maths (when to ask HDFC about a batch), what to do
--   after one call, the per-city job create and the per-batch lock name. All pure except ensureCityJob.
--   Juspay/Stripe impact: bulk-only. Callers: Bulk.Batch.openBulkBatch (ensureCityJob, firstCheckAt),
--   Bulk.Submit (firstCheckAt), Bulk.Resolve and Bulk.StatusCheck -- all bulk-only. Juspay/Stripe orders are
--   checked by main's per-order job in Lib.Payment.Payout.StatusCheck (job data {payoutOrderId, attempt}),
--   which this module does not touch.

-- | When to ask HDFC about a batch, and what to do with what it says.
--
-- The schedule is a plain function of the plan and the moment the batch was submitted, so it can
-- be read, tested and reasoned about without a database or a network. The rest of this module
-- makes sure a city has its status-check job, and names the per-batch lock.
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

-- NOTE (reviewer, remove before merge): the status-check plan. Its numbers come from the city's HDFC
--   config (bulkStatusCheckPlan), clamped by shared-kernel's sanitizeBulkStatusCheckPlan. The default
--   (defaultBulkStatusCheckPlan): first check at T+2h, then every 2h, 6 checks on day 1; then one check at
--   T+48h and one at T+72h; up to 2 calls per check, 15 s apart; at most 12 calls a day.
--   How many daily checks follow day 1 is `tailChecks` in the city's HDFC config. A batch is checked many
--   times, but nothing is ever re-sent to HDFC.

-- | Every check time for a batch submitted at @t@: the first day's burst, then one check at the
--   end of each following day -- T+48h and T+72h on the production plan, where @tailGapMinutes@
--   is a day. Everything is an offset from @t@, so there is no time zone and no calendar
--   arithmetic anywhere in the plan, and a test can shrink every number to seconds.
checkTimes :: Payout.BulkStatusCheckPlan -> UTCTime -> [UTCTime]
checkTimes plan t = burst <> tailChecks
  where
    burst =
      [ addUTCTime (minutes (plan.firstCheckDelayMinutes + i * plan.burstGapMinutes)) t
        | i <- [0 .. plan.burstChecks - 1]
      ]
    tailChecks = [addUTCTime (minutes (plan.tailGapMinutes * (k + 1))) t | k <- [1 .. plan.tailChecks]]

-- | When the first check of a batch submitted at @t@ is due.
firstCheckAt :: Payout.BulkStatusCheckPlan -> UTCTime -> UTCTime
firstCheckAt plan t = addUTCTime (minutes plan.firstCheckDelayMinutes) t

-- NOTE (reviewer, remove before merge): picks the next planned check. Times that were missed (e.g. the job
--   was down) are skipped, not fired back to back. `Nothing` = the plan is finished; Bulk.Resolve then moves
--   a batch that still has items in flight to MANUAL_REVIEW_REQUIRED.

-- | The check to run after check number @done@ has ended: the first planned time still far
--   enough away.
--
--   Planned times that have passed, or are about to, are skipped rather than fired back to back.
--   A check is only worth making for the status it reads /now/, so repeating ones missed during
--   an outage gains nothing -- and skipping is what keeps the day's call budget intact.
--
--   'Nothing' means the plan is finished: the batch needs a human.
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
  = -- | We have an answer: rows, "no data", or a refusal. Either way this check is done asking.
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

-- NOTE (reviewer, remove before merge): one check = up to maxCallsPerCheck calls. HDFC's normal first
--   answer is 202 "enquire again" (NotReady): call again readCallDelaySeconds later, in the same check. A
--   failed call is retried after errorRetryDelaySeconds. A real answer ends the check. Every call counts
--   toward the cap. Known limit: the daily cap is checked against the day-1 burst only, so a tail check that
--   also falls on day 1 is not counted; the default plan has none.

-- | What to do after one call. @calls@ counts the calls already made in this check, this one
--   included.
--
--   Every call counts, answered or not. That is the simple version of HDFC's daily ceiling: a
--   batch can never make more than @burstChecks * maxCallsPerCheck@ calls a day, and
--   'Payout.sanitizeBulkStatusCheckPlan' has already held that product under @maxCallsPerDay@.
--   The cost is that a failed call uses up one of the check's calls: it is retried inside the same
--   check only while a call is left, and a failure on the check's last call waits for the next
--   planned check.
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

-- NOTE (reviewer, remove before merge): the per-batch lock. Bulk.StatusCheck.stepOneBatch takes it (without
--   waiting) before touching a batch, so if the scheduler runs two copies of a city's job, only one of them
--   calls HDFC for a given batch. Only the status check takes this lock. Bulk-only.

-- | The lock a batch's status check holds, so two status-check runs never work on the same batch at
--   the same time.
batchLockKey :: Handle m -> Id DPayoutBatch.PayoutBatch -> Text
batchLockKey h batchId = h.keyPrefix <> "BulkPayoutStatusCheck:batch:" <> batchId.getId

-- NOTE (reviewer, remove before merge): one status-check job PER CITY. The only caller is
--   Bulk.Batch.openBulkBatch, so this runs every time a batch is opened. The app's createCityStatusCheckJob
--   makes no new job if the city already has a pending one. The 60 s per-city lock stops two batches opened
--   at the same moment from both creating one; a caller that misses the lock skips, because the holder is
--   creating it. The create runs in the master cloud's Redis cell. If a city's job is ever lost, it comes
--   back only with that city's next batch. Bulk-only.

-- | Make sure the city has its status-check job. Called whenever a batch is created in that city.
--   The app's create looks for a pending job for the city in the scheduler itself and creates one
--   only when there is none; the lock keeps two batches created at the same moment from both
--   finding none and both creating one.
ensureCityJob :: (Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, MonadMask m) => Handle m -> Text -> Text -> m ()
ensureCityJob h merchantId cityId =
  Redis.whenWithLockRedis (h.keyPrefix <> "BulkPayoutStatusCheck:create:" <> cityId) 60 $
    Redis.runInMasterCloudRedisCell (h.createCityStatusCheckJob merchantId cityId)

minutes :: Int -> NominalDiffTime
minutes m = fromIntegral m * 60

seconds :: Int -> NominalDiffTime
seconds = fromIntegral
