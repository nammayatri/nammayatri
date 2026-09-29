{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | When to ask HDFC about a batch, and what to do with what it says.
--
-- The schedule is a plain function of the plan and the moment the batch was submitted, so it can
-- be read, tested and reasoned about without a database or a network. The rest of this module is
-- the two Redis helpers that keep the always-on job alive.
--
-- Design: @Backend/dev/docs/hdfc-cbx-bulk-status-check-design.md@.
module SharedLogic.Payout.BulkStatusCheck
  ( checkTimes,
    firstCheckAt,
    nextCheck,
    CallOutcome (..),
    NextStep (..),
    nextStepAfterCall,
    statusCheckMaxSleep,
    ensureStatusCheckJob,
    refreshStatusCheckJobAlive,
  )
where

import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator

--------------------------------------------------------------------------------
-- The schedule
--------------------------------------------------------------------------------

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

-- | What to do after one call. @calls@ counts the calls already made in this check, this one
--   included.
--
--   Every call counts, answered or not. That is the simple version of HDFC's daily ceiling: a
--   batch can never make more than @burstChecks * maxCallsPerCheck@ calls a day, and
--   'Payout.sanitizeBulkStatusCheckPlan' has already held that product under @maxCallsPerDay@.
--   The cost is that a check lost to a timeout waits for the next one instead of retrying inside
--   the same check.
nextStepAfterCall :: Payout.BulkStatusCheckPlan -> Int -> CallOutcome -> NextStep
nextStepAfterCall plan calls outcome
  | calls >= plan.maxCallsPerCheck = EndCheck
  | otherwise = case outcome of
    Answered -> EndCheck
    NotReady -> CallAgainIn (seconds plan.readCallDelaySeconds)
    CallFailed -> CallAgainIn (seconds plan.errorRetryDelaySeconds)

--------------------------------------------------------------------------------
-- Keeping the job alive
--------------------------------------------------------------------------------

-- | The longest the job sleeps when nothing is due. Also how quickly a batch submitted while it
--   sleeps gets noticed -- harmless, since a first check is hours away.
statusCheckMaxSleep :: NominalDiffTime
statusCheckMaxSleep = 5 * 60

statusCheckAliveKey :: Text
statusCheckAliveKey = "BulkPayoutStatusCheck:alive"

-- | Two full sleeps and a run: long enough that a working job always refreshes in time, short
--   enough that a dead one is noticed on the next submission.
statusCheckAliveTtl :: Int
statusCheckAliveTtl = 11 * 60

-- | Called at the start of every run of the job.
refreshStatusCheckJobAlive :: (Redis.HedisFlow m r) => m ()
refreshStatusCheckJobAlive = Redis.setExp statusCheckAliveKey True statusCheckAliveTtl

-- | Create the one always-on status-check job unless it is already running.
--
--   Called after every submission, so a job that died -- or was never created, on a fresh
--   deployment -- is back within a cycle. Idempotent twice over: the liveness key short-circuits
--   the common case, and the lock keeps two callers from creating two jobs.
ensureStatusCheckJob :: (JobCreator r m, Redis.HedisLTSFlowEnv r) => m ()
ensureStatusCheckJob = do
  mbAlive :: Maybe Bool <- Redis.get statusCheckAliveKey
  when (isNothing mbAlive) $
    Redis.whenWithLockRedis "BulkPayoutStatusCheck:create" 60 $ do
      Redis.runInMasterCloudRedisCell $
        createJobIn @_ @'BulkPayoutStatusCheck Nothing Nothing 5 BulkPayoutStatusCheckJobData
      refreshStatusCheckJobAlive
      logInfo "Created the bulk payout status-check job"

minutes :: Int -> NominalDiffTime
minutes m = fromIntegral m * 60

seconds :: Int -> NominalDiffTime
seconds = fromIntegral
