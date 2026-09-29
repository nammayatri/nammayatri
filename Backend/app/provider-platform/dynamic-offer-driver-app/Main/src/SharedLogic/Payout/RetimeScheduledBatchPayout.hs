{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- NOTE (reviewer, remove before merge): new file; main never touches the queued sweep job after it is created.
--   This module moves, creates or replaces the queued ScheduledBatchPayout (sweep) job when the scheduled-config upsert
--   edits a config, and gives VIEW_DIFF a read-only preview of the same decision. Callers:
--   Domain/Action/Dashboard/PayoutRequest.hs (upsertScheduledPayoutConfig, diffScheduledPayoutConfig);
--   Domain/Action/Dashboard/Management/Payout.hs only reads the RetimeDecision type to build the VIEW_DIFF answer.
--   Shared with Juspay/Stripe -- intended change: the sweep job and its config are the same for every payout partner,
--   so Juspay/Stripe cities get this too. It only moves, creates or closes scheduler_job rows and their Redis queue
--   entries; it never pays.
--   Ops note: a manual "merchant/scheduler/trigger" for the sweep queues a second job; a later re-time or resume
--   cancels extra waiting copies (cancelJob).

-- | When a scheduled payout config's timing is edited, move the already-queued job to the new
--   schedule.
--
--   gap = queued job's scheduledAt - now
--     gap >  buffer -> move it to computeNextRunTime(new config)
--     gap <= buffer -> leave it; when it finishes it reschedules itself using the new config
module SharedLogic.Payout.RetimeScheduledBatchPayout
  ( scheduleChanged,
    retimeQueuedPayoutJob,
    previewRetime,
    RetimeDecision (..),
    resumeSweepJob,
    previewResume,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Text.Encoding as DT
import qualified Data.Time as T
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Environment
import Kernel.Beam.Functions (findAllWithKVScheduler)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.DB.Queries as DBQ
import qualified Lib.Scheduler.JobStorageType.Redis.Queries as RQ
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Sequelize as Se
import SharedLogic.Allocator (AllocatorJobType (..), ScheduledBatchPayoutJobData (..))
import SharedLogic.Allocator.Jobs.Payout.ScheduledBatchPayout (computeNextRunTime)
import qualified Storage.Beam.SchedulerJob as BeamST

-- | True when a field used by 'computeNextRunTime' changed.
scheduleChanged :: DSPC.ScheduledPayoutConfig -> DSPC.ScheduledPayoutConfig -> Bool
scheduleChanged old new =
  old.frequency /= new.frequency
    || old.timeOfDay /= new.timeOfDay
    || old.dayOfWeek /= new.dayOfWeek
    || old.dayOfMonth /= new.dayOfMonth
    || old.intervalHours /= new.intervalHours
    || old.intervalDays /= new.intervalDays
    || old.timeDiffFromUtc /= new.timeDiffFromUtc

-- NOTE (reviewer, remove before merge): outside the buffer, the earliest queued job is moved (same job id), then the
--   extra copies are cancelled. If that job is not in the Redis queue, moveJob raises "The payout job wasn't found in
--   the queue; nothing was changed. Try again." The caller runs this before saving the config, so an error saves
--   nothing. Shared with Juspay/Stripe -- intended.

-- | Keeps the earliest queued job (moved to the new time) and cancels any others for this
--   (city, category). Runs in the same Redis cell as job creation.
retimeQueuedPayoutJob :: DSPC.ScheduledPayoutConfig -> DSPC.ScheduledPayoutConfig -> Environment.Flow ()
retimeQueuedPayoutJob old new = withLogTag "ScheduledBatchPayout:retime" . Redis.runInMasterCloudRedisCell $ do
  now <- getCurrentTime
  jobs <- findQueuedJobs old now
  case jobs of
    [] -> logInfo "no queued job found; the new schedule applies after the next run"
    (job : extraJobs)
      | insideBuffer new now (scheduledAtOf job) ->
        logInfo $ "queued job at " <> show (scheduledAtOf job) <> " is within the buffer; leaving it"
      | otherwise -> do
        newTime <- computeNextRunTime new
        moveJob job newTime
        mapM_ cancelJob extraJobs

-- | The buffer rule, in one place: a queued job this close to firing is left where it is, because
--   when it finishes it reschedules itself off the new config anyway. Shared with 'previewRetime',
--   so what an operator is shown before committing cannot differ from what the commit does.
insideBuffer :: DSPC.ScheduledPayoutConfig -> UTCTime -> UTCTime -> Bool
insideBuffer new now scheduledAt =
  diffUTCTime scheduledAt now <= (fromIntegral (60 * fromMaybe 0 new.rescheduleBufferMinutes) :: NominalDiffTime)

-- | What 'retimeQueuedPayoutJob' would do to the queued job, decided without touching it.
data RetimeDecision
  = -- | nothing queued for this (city, category): the new schedule applies after the next run
    NoQueuedJob
  | -- | queued at this time, close enough to firing that it is left alone
    LeaveInsideBuffer UTCTime
  | -- | queued at the first time; would be moved to the second
    MoveJobTo UTCTime UTCTime
  | -- | the commit will not re-time at all (buffer check off, config disabled or resuming, or the
    --   schedule unchanged): whatever is queued (if anything, at this time) keeps its time
    NotMoved (Maybe UTCTime)
  deriving (Show, Eq)

-- | The read-only half of 'retimeQueuedPayoutJob': same lookup, same buffer rule, same
--   'computeNextRunTime'. Used by the config preview, which must not be able to promise an outcome
--   the commit would not produce.
previewRetime :: DSPC.ScheduledPayoutConfig -> DSPC.ScheduledPayoutConfig -> Environment.Flow RetimeDecision
previewRetime old new = withLogTag "ScheduledBatchPayout:retime:preview" . Redis.runInMasterCloudRedisCell $ do
  now <- getCurrentTime
  jobs <- findQueuedJobs old now
  case jobs of
    [] -> pure NoQueuedJob
    (job : _)
      | insideBuffer new now (scheduledAtOf job) -> pure $ LeaveInsideBuffer (scheduledAtOf job)
      | otherwise -> MoveJobTo (scheduledAtOf job) <$> computeNextRunTime new

-- | Pending jobs of this (city, category) between now and the old config's next run, earliest first.
--   The upper bound keeps the scan small (scheduler_job has no job_type index): a job placed by the
--   old config cannot be later than that. +1 min because the bound is strict and a fixed-time job
--   sits exactly on it.
findQueuedJobs :: DSPC.ScheduledPayoutConfig -> UTCTime -> Environment.Flow [AnyJob AllocatorJobType]
findQueuedJobs old now = do
  oldNextRun <- computeNextRunTime old
  jobs <-
    findAllWithKVScheduler
      [ Se.And
          [ Se.Is BeamST.status $ Se.Eq Pending,
            Se.Is BeamST.jobType $ Se.Eq "ScheduledBatchPayout",
            Se.Is BeamST.merchantOperatingCityId $ Se.Eq (Just old.merchantOperatingCityId.getId),
            Se.Is BeamST.scheduledAt $ Se.GreaterThan (toLocal now),
            Se.Is BeamST.scheduledAt $ Se.LessThan (toLocal (addUTCTime 60 oldNextRun))
          ]
      ]
  pure $ sortOn scheduledAtOf (filter (isSameCategory old) jobs)
  where
    toLocal = T.utcToLocalTime T.utc

-- | Whether a sweep job is for this config's payout category. The category is not a column, so it is
--   read from the job data.
isSameCategory :: DSPC.ScheduledPayoutConfig -> AnyJob AllocatorJobType -> Bool
isSameCategory config (AnyJob job) =
  case decodeFromText (storeJobInfo job.jobInfo).storedJobContent of
    Just (jobData :: ScheduledBatchPayoutJobData) -> jobData.payoutCategory == config.payoutCategory
    Nothing -> False

-- NOTE (reviewer, remove before merge): tryMoveJob is separate from moveJob so that resumeSweepJob can recover when the
--   queue entry is missing (mark the job done and create a new one), while a plain re-time fails loudly through
--   moveJob. Shared, as above.

-- | Move a queued job to a new time: zRem -> DB update -> zAdd, same job id. Removing the old entry
--   first means a crash can only leave no entry (the reviver recovers the Pending row), never two.
--   False if the job is not in the Redis queue, in which case nothing was changed.
tryMoveJob :: AnyJob AllocatorJobType -> UTCTime -> Environment.Flow Bool
tryMoveJob job newTime = do
  removed <- takeOffQueue job
  when removed $ do
    DBQ.reSchedule (idOf job) newTime
    addToQueue job newTime
    logInfo $ "moved job " <> (idOf job).getId <> " from " <> show (scheduledAtOf job) <> " to " <> show newTime
  pure removed

-- | 'tryMoveJob', but an error when the job is not in the queue, so a config edit that could not move
--   its job fails instead of reporting success.
moveJob :: AnyJob AllocatorJobType -> UTCTime -> Environment.Flow ()
moveJob job newTime = do
  moved <- tryMoveJob job newTime
  unless moved $
    throwError (InvalidRequest "The payout job wasn't found in the queue; nothing was changed. Try again.")

-- | An extra queued job would run a second sweep. Take it off the queue, then mark it Completed so
--   the reviver does not bring it back.
cancelJob :: AnyJob AllocatorJobType -> Environment.Flow ()
cancelJob job = do
  removed <- takeOffQueue job
  when removed $ do
    DBQ.markAsComplete (idOf job)
    logInfo $ "cancelled extra job " <> (idOf job).getId <> " at " <> show (scheduledAtOf job)

-- | RedisBased: remove the job's sorted-set entry; True only if this call removed it, so two
--   concurrent edits cannot both move the same job. DbBased: the DB row is the only copy.
takeOffQueue :: AnyJob AllocatorJobType -> Environment.Flow Bool
takeOffQueue job = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    DbBased -> pure True
    RedisBased -> removeFromSortedSet job

addToQueue :: AnyJob AllocatorJobType -> UTCTime -> Environment.Flow ()
addToQueue job newTime = do
  schedulerType <- asks (.schedulerType)
  case schedulerType of
    DbBased -> pure ()
    RedisBased -> RQ.reSchedule job newTime -- writes the job with the new scheduledAt as the score

-- | The shard is not stored on the entry's key, so look in every shard around the job's score. The
--   window is +-500 ms because the DB copy and the Redis copy of a job are written with separate
--   clock reads, a few ms apart. Safe: an entry is only removed if its job id matches. The entry is
--   removed by its raw bytes, because re-encoding the job may not give the same string.
removeFromSortedSet :: AnyJob AllocatorJobType -> Environment.Flow Bool
removeFromSortedSet job = do
  setName <- asks (.schedulerSetName)
  maxShards <- asks (.maxShards)
  let score = utcToMilliseconds (scheduledAtOf job)
  removeFrom score [setName <> "{" <> show shard <> "}" | shard <- [0 .. maxShards - 1]]
  where
    removeFrom _ [] = pure False
    removeFrom score (key : otherKeys) = do
      entries <- Redis.withNonCriticalCrossAppRedis $ Redis.zRangeByScore key (score - 500) (score + 500)
      case find isThisJob entries of
        Just entry -> do
          removedCount <- Redis.withNonCriticalCrossAppRedis $ Redis.zRem key [DT.decodeUtf8 entry]
          pure (removedCount == 1)
        Nothing -> removeFrom score otherKeys
    isThisJob entry =
      case A.decode (BL.fromStrict entry) :: Maybe (AnyJob AllocatorJobType) of
        Just entryJob -> idOf entryJob == idOf job
        Nothing -> False

idOf :: AnyJob t -> Id AnyJob
idOf (AnyJob job) = job.id

scheduledAtOf :: AnyJob t -> UTCTime
scheduledAtOf (AnyJob job) = job.scheduledAt

-- NOTE (reviewer, remove before merge): resume, run before the config is saved: a sweep running or due within the last
--   hour -> 400 "try again in a few minutes"; a job due more than an hour ago (stuck) -> marked done; a waiting job ->
--   moved, or, if its Redis entry is missing, marked done and replaced by a new job; no waiting job -> one is created.
--   Extra waiting copies are cancelled. PayoutRequest.hs also calls this when a config is created enabled.
--   Shared with Juspay/Stripe -- intended.
--   Known gap: when the waiting job is already at the new time, its queue entry is not checked (if it is missing, the
--   scheduler's reviver runs the job late).

-- | Turning the schedule back on: put the sweep job at the new schedule's next run. Runs before the
--   config is saved, so an error saves nothing.
--
--   * A sweep running or due right now: refuse, and ask to try again in a few minutes.
--   * A waiting job: move it to the new time. If its Redis entry is missing it would never fire, so
--     it is marked done and a new job is created.
--   * No waiting job: create one.
resumeSweepJob :: DSPC.ScheduledPayoutConfig -> Environment.Flow ()
resumeSweepJob config = withLogTag "ScheduledBatchPayout:resume" . Redis.runInMasterCloudRedisCell $ do
  now <- getCurrentTime
  newTime <- computeNextRunTime config
  pendingJobs <- findAllPendingSweepJobs config now
  let oneHourAgo = addUTCTime (-3600) now
      runningOrDueNow = [job | job <- pendingJobs, scheduledAtOf job <= now, scheduledAtOf job > oneHourAgo]
      stuckJobs = [job | job <- pendingJobs, scheduledAtOf job <= oneHourAgo]
      waitingJobs = [job | job <- pendingJobs, scheduledAtOf job > now] -- soonest first
  unless (null runningOrDueNow) $
    throwError (InvalidRequest "A payout sweep is running or due now; try again in a few minutes.")
  mapM_ markJobDone stuckJobs
  case waitingJobs of
    [] -> do
      createSweepJob config newTime
      logInfo $ "no waiting job; created one for " <> show newTime
    (job : extraJobs) -> do
      if scheduledAtOf job == newTime
        then logInfo $ "the waiting job is already at " <> show newTime
        else do
          moved <- tryMoveJob job newTime
          unless moved $ do
            -- Its Redis entry is missing, so it would never fire. Replace it.
            markJobDone job
            createSweepJob config newTime
            logInfo $ "the waiting job was missing from the queue; replaced it with a new one for " <> show newTime
      mapM_ cancelJob extraJobs

-- NOTE (reviewer, remove before merge): the VIEW_DIFF preview of a resume: move the waiting job, or create one.
--   Read-only. It does not predict the refusal when a sweep is running or due now. Shared with Juspay/Stripe --
--   preview only.

-- | What 'resumeSweepJob' would do, for the config preview.
previewResume :: DSPC.ScheduledPayoutConfig -> Environment.Flow RetimeDecision
previewResume config = withLogTag "ScheduledBatchPayout:resume:preview" . Redis.runInMasterCloudRedisCell $ do
  now <- getCurrentTime
  newTime <- computeNextRunTime config
  pendingJobs <- findAllPendingSweepJobs config now
  case [job | job <- pendingJobs, scheduledAtOf job > now] of
    [] -> pure NoQueuedJob -- a new job will be created at the new time
    (job : _) -> pure (MoveJobTo (scheduledAtOf job) newTime)

-- | Every pending sweep job of this (city, category) from the last day onwards, soonest first.
findAllPendingSweepJobs :: DSPC.ScheduledPayoutConfig -> UTCTime -> Environment.Flow [AnyJob AllocatorJobType]
findAllPendingSweepJobs config now = do
  jobs <-
    findAllWithKVScheduler
      [ Se.And
          [ Se.Is BeamST.status $ Se.Eq Pending,
            Se.Is BeamST.jobType $ Se.Eq "ScheduledBatchPayout",
            Se.Is BeamST.merchantOperatingCityId $ Se.Eq (Just config.merchantOperatingCityId.getId),
            Se.Is BeamST.scheduledAt $ Se.GreaterThan (T.utcToLocalTime T.utc (addUTCTime (-86400) now))
          ]
      ]
  pure $ sortOn scheduledAtOf (filter (isSameCategory config) jobs)

-- | A new sweep job for this config, at the given time.
createSweepJob :: DSPC.ScheduledPayoutConfig -> UTCTime -> Environment.Flow ()
createSweepJob config runAt = do
  now <- getCurrentTime
  let jobData =
        ScheduledBatchPayoutJobData
          { merchantId = config.merchantId,
            merchantOperatingCityId = config.merchantOperatingCityId,
            payoutCategory = config.payoutCategory,
            vehicleCategory = config.vehicleCategory
          }
  createJobIn @_ @'ScheduledBatchPayout (Just config.merchantId) (Just config.merchantOperatingCityId) (diffUTCTime runAt now) jobData

-- | Take a job off the queue (if it is there) and mark it done, so it never runs.
markJobDone :: AnyJob AllocatorJobType -> Environment.Flow ()
markJobDone job = do
  void (takeOffQueue job)
  DBQ.markAsComplete (idOf job)
  logInfo $ "marked job " <> (idOf job).getId <> " (at " <> show (scheduledAtOf job) <> ") as done"
