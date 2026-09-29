{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Moves the queued sweep job when a scheduled payout config's timing is edited or the schedule is
--   turned back on.
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

-- | A job this close to firing is left alone; it reschedules itself from the new config when it
--   finishes. Shared with 'previewRetime' so the preview matches the commit.
insideBuffer :: DSPC.ScheduledPayoutConfig -> UTCTime -> UTCTime -> Bool
insideBuffer new now scheduledAt =
  diffUTCTime scheduledAt now <= (fromIntegral (60 * fromMaybe 0 new.rescheduleBufferMinutes) :: NominalDiffTime)

-- | What 'retimeQueuedPayoutJob' would do, decided without touching the job.
data RetimeDecision
  = -- | nothing queued for this (city, category): the new schedule applies after the next run
    NoQueuedJob
  | -- | queued at this time, close enough to firing that it is left alone
    LeaveInsideBuffer UTCTime
  | -- | queued at the first time; would be moved to the second
    MoveJobTo UTCTime UTCTime
  | -- | the commit will not re-time (check off, disabled, resuming or schedule unchanged);
    --   a queued job, if any, keeps this time
    NotMoved (Maybe UTCTime)
  deriving (Show, Eq)

-- | Read-only version of 'retimeQueuedPayoutJob', for the config preview.
previewRetime :: DSPC.ScheduledPayoutConfig -> DSPC.ScheduledPayoutConfig -> Environment.Flow RetimeDecision
previewRetime old new = withLogTag "ScheduledBatchPayout:retime:preview" . Redis.runInMasterCloudRedisCell $ do
  now <- getCurrentTime
  jobs <- findQueuedJobs old now
  case jobs of
    [] -> pure NoQueuedJob
    (job : _)
      | insideBuffer new now (scheduledAtOf job) -> pure $ LeaveInsideBuffer (scheduledAtOf job)
      | otherwise -> MoveJobTo (scheduledAtOf job) <$> computeNextRunTime new

-- | Pending jobs of this (city, category) up to the old config's next run (+1 min, as the bound is
--   strict), earliest first. The bound keeps the scan small: scheduler_job has no job_type index.
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

-- | The payout category is not a column, so it is read from the job data.
isSameCategory :: DSPC.ScheduledPayoutConfig -> AnyJob AllocatorJobType -> Bool
isSameCategory config (AnyJob job) =
  case decodeFromText (storeJobInfo job.jobInfo).storedJobContent of
    Just (jobData :: ScheduledBatchPayoutJobData) -> jobData.payoutCategory == config.payoutCategory
    Nothing -> False

-- | Move a queued job, keeping its id. The Redis entry goes first, so a crash leaves no entry (the
--   reviver recovers it), never two. False, with nothing changed, if the job is not queued.
tryMoveJob :: AnyJob AllocatorJobType -> UTCTime -> Environment.Flow Bool
tryMoveJob job newTime = do
  removed <- takeOffQueue job
  when removed $ do
    DBQ.reSchedule (idOf job) newTime
    addToQueue job newTime
    logInfo $ "moved job " <> (idOf job).getId <> " from " <> show (scheduledAtOf job) <> " to " <> show newTime
  pure removed

-- | 'tryMoveJob' that fails when the job is not queued, so the config edit does not report success.
moveJob :: AnyJob AllocatorJobType -> UTCTime -> Environment.Flow ()
moveJob job newTime = do
  moved <- tryMoveJob job newTime
  unless moved $
    throwError (InvalidRequest "The payout job wasn't found in the queue; nothing was changed. Try again.")

-- | Drop an extra queued job (it would run a second sweep); Completed so the reviver skips it.
cancelJob :: AnyJob AllocatorJobType -> Environment.Flow ()
cancelJob job = do
  removed <- takeOffQueue job
  when removed $ do
    DBQ.markAsComplete (idOf job)
    logInfo $ "cancelled extra job " <> (idOf job).getId <> " at " <> show (scheduledAtOf job)

-- | True only if this call removed the Redis entry, so two concurrent edits cannot both move the
--   job. DbBased has no queue, so always True.
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
    RedisBased -> RQ.reSchedule job newTime

-- | The shard is not on the key, so search every shard within +-500 ms of the score (the DB and Redis
--   copies use separate clock reads). Matches on job id and removes the raw bytes, as re-encoding may differ.
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

-- | On resume, put the sweep job at the new next run. Runs before the config is saved, so an error
--   saves nothing. Refuses while a sweep is running or due; a job missing from Redis is replaced.
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
            markJobDone job
            createSweepJob config newTime
            logInfo $ "the waiting job was missing from the queue; replaced it with a new one for " <> show newTime
      mapM_ cancelJob extraJobs

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
