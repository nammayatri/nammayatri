{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | When a scheduled payout config's timing is edited, move the already-queued job to the new
--   schedule (design: Backend/dev/docs/payout-job-reschedule-DESIGN.md).
--
--   gap = queued job's scheduledAt - now
--     gap >  buffer -> move it to computeNextRunTime(new config)
--     gap <= buffer -> leave it; when it finishes it reschedules itself using the new config
module SharedLogic.Payout.RetimeScheduledBatchPayout
  ( scheduleChanged,
    retimeQueuedPayoutJob,
    previewRetime,
    RetimeDecision (..),
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
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.DB.Queries as DBQ
import qualified Lib.Scheduler.JobStorageType.Redis.Queries as RQ
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
        mapM_ cancelJob extraJobs
        moveJob job newTime

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
  pure $ sortOn scheduledAtOf (filter isSameCategory jobs)
  where
    toLocal = T.utcToLocalTime T.utc
    -- payout category is not a column, so read it from the job data
    isSameCategory :: AnyJob AllocatorJobType -> Bool
    isSameCategory (AnyJob job) =
      case decodeFromText (storeJobInfo job.jobInfo).storedJobContent of
        Just (jobData :: ScheduledBatchPayoutJobData) -> jobData.payoutCategory == old.payoutCategory
        Nothing -> False

-- | zRem -> DB update -> zAdd, same job id. Removing the old entry first means a crash can only
--   leave no entry (the reviver recovers the Pending row), never two.
moveJob :: AnyJob AllocatorJobType -> UTCTime -> Environment.Flow ()
moveJob job newTime = do
  removed <- takeOffQueue job
  if removed
    then do
      DBQ.reSchedule (idOf job) newTime
      addToQueue job newTime
      logInfo $ "moved job " <> (idOf job).getId <> " from " <> show (scheduledAtOf job) <> " to " <> show newTime
    else logWarning $ "job " <> (idOf job).getId <> " was not in the queue (already moved?); left as is"

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

-- | The shard is not stored on the entry's key, so look in every shard at the job's score
--   (+-1 ms: the DB keeps microseconds, the entry full precision). The entry is removed by its raw
--   bytes, because re-encoding the job may not give the same string.
removeFromSortedSet :: AnyJob AllocatorJobType -> Environment.Flow Bool
removeFromSortedSet job = do
  setName <- asks (.schedulerSetName)
  maxShards <- asks (.maxShards)
  let score = utcToMilliseconds (scheduledAtOf job)
  removeFrom score [setName <> "{" <> show shard <> "}" | shard <- [0 .. maxShards - 1]]
  where
    removeFrom _ [] = pure False
    removeFrom score (key : otherKeys) = do
      entries <- Redis.withNonCriticalCrossAppRedis $ Redis.zRangeByScore key (score - 1) (score + 1)
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
