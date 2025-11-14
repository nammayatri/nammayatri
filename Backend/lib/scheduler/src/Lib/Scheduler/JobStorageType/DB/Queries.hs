{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Lib.Scheduler.JobStorageType.DB.Queries where

import qualified Data.ByteString as BS
import Data.Time as T hiding (getCurrentTime)
import Kernel.Beam.Functions (FromTType'' (..), ToTType'' (..), createWithKVScheduler, findAllWithKVScheduler, findAllWithOptionsKVScheduler, findOneWithKVScheduler, updateWithKVScheduler)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Common (Log, MonadTime (getCurrentTime))
import Kernel.Types.Error (GenericError (InternalError, InvalidRequest))
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Kernel.Utils.Error (throwError)
import Lib.Scheduler.Environment
import Lib.Scheduler.JobStorageType.DB.Table hiding (Id)
import qualified Lib.Scheduler.JobStorageType.DB.Table as BeamST hiding (Id)
import qualified Lib.Scheduler.ScheduleJob as ScheduleJob
import Lib.Scheduler.Types as ST
import qualified Sequelize as Se

instance (JobProcessor t) => FromTType'' BeamST.SchedulerJob (AnyJob t) where
  fromTType'' BeamST.SchedulerJobT {..} = do
    (AnyJobInfo anyJobInfo) :: AnyJobInfo t <-
      restoreAnyJobInfoMain @t (StoredJobInfo jobType jobData)
        & fromMaybeM (InternalError ("Unable to restore JobInfo. " <> jobType <> ": " <> jobData))
    pure . Just $
      AnyJob $
        Job
          { id = Id id,
            shardId = shardId,
            scheduledAt = T.localTimeToUTC T.utc scheduledAt,
            createdAt = T.localTimeToUTC T.utc createdAt,
            updatedAt = T.localTimeToUTC T.utc updatedAt,
            maxErrors = maxErrors,
            currErrors = currErrors,
            status = status,
            jobInfo = anyJobInfo,
            parentJobId = Id parentJobId,
            merchantId = Id <$> merchantId,
            merchantOperatingCityId = Id <$> merchantOperatingCityId
          }

instance (JobProcessor t) => ToTType'' BeamST.SchedulerJob (AnyJob t) where
  toTType'' (AnyJob Job {..}) = do
    let storedJobInfo = storeJobInfo jobInfo
    BeamST.SchedulerJobT
      { BeamST.id = getId id,
        BeamST.jobType = storedJobType storedJobInfo,
        BeamST.jobData = storedJobContent storedJobInfo,
        BeamST.shardId = shardId,
        BeamST.scheduledAt = T.utcToLocalTime T.utc scheduledAt,
        BeamST.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamST.updatedAt = T.utcToLocalTime T.utc updatedAt,
        BeamST.maxErrors = maxErrors,
        BeamST.currErrors = currErrors,
        BeamST.status = status,
        BeamST.parentJobId = getId parentJobId,
        BeamST.merchantId = getId <$> merchantId,
        BeamST.merchantOperatingCityId = getId <$> merchantOperatingCityId
      }

createJob ::
  forall t (e :: t) m r.
  (JobFlow t e, JobCreator r m) =>
  Maybe (Id (MerchantType t)) ->
  Maybe (Id (MerchantOperatingCityType t)) ->
  Text ->
  Int ->
  JobContent e ->
  m ()
createJob merchantId merchantOperatingCityId uuid maxShards jobData = do
  void $
    ScheduleJob.createJob @t @e @m merchantId merchantOperatingCityId uuid createWithKVScheduler maxShards $
      JobEntry
        { jobData = jobData,
          maxErrors = 5
        }

createJobIn ::
  forall t (e :: t) m r.
  (Log m, JobFlow t e, JobCreator r m) =>
  Maybe (Id (MerchantType t)) ->
  Maybe (Id (MerchantOperatingCityType t)) ->
  Text ->
  NominalDiffTime ->
  Int ->
  JobContent e ->
  m ()
createJobIn merchantId merchantOperatingCityId uuid inTime maxShards jobData = do
  void $
    ScheduleJob.createJobIn @t @e @m merchantId merchantOperatingCityId uuid createWithKVScheduler inTime maxShards $
      JobEntry
        { jobData = jobData,
          maxErrors = 5
        }

createJobByTime ::
  forall t (e :: t) m r.
  (JobFlow t e, JobCreator r m) =>
  Maybe (Id (MerchantType t)) ->
  Maybe (Id (MerchantOperatingCityType t)) ->
  Text ->
  UTCTime ->
  Int ->
  JobContent e ->
  m ()
createJobByTime merchantId merchantOperatingCityId uuid byTime maxShards jobData = do
  void $
    ScheduleJob.createJobByTime @t @e @m merchantId merchantOperatingCityId uuid createWithKVScheduler byTime maxShards $
      JobEntry
        { jobData = jobData,
          maxErrors = 5
        }

findAll :: forall m r t. (FromTType'' BeamST.SchedulerJob (AnyJob t), JobMonad r m) => m [AnyJob t]
findAll = findAllWithKVScheduler [Se.Is BeamST.id $ Se.Not $ Se.Eq ""]

findById :: forall m r t. (FromTType'' BeamST.SchedulerJob (AnyJob t), JobMonad r m) => Id AnyJob -> m (Maybe (AnyJob t))
findById (Id id) = findOneWithKVScheduler [Se.Is BeamST.id $ Se.Eq id]

getTasksById :: forall m r t. (FromTType'' BeamST.SchedulerJob (AnyJob t), JobMonad r m) => [Id AnyJob] -> m [AnyJob t]
getTasksById ids = findAllWithKVScheduler [Se.Is BeamST.id $ Se.In $ getId <$> ids]

getPendingStuckJobs :: forall m r t. (FromTType'' BeamST.SchedulerJob (AnyJob t), JobMonad r m) => UTCTime -> m [AnyJob t]
getPendingStuckJobs newtime = do
  findAllWithKVScheduler
    [ Se.And
        [ Se.Is BeamST.status $ Se.Eq Pending,
          Se.Is BeamST.scheduledAt $ Se.LessThanOrEq (T.utcToLocalTime T.utc newtime)
        ]
    ]

getJobByTypeAndScheduleTime :: forall m r t. (FromTType'' BeamST.SchedulerJob (AnyJob t), JobMonad r m) => Text -> UTCTime -> UTCTime -> m [AnyJob t]
getJobByTypeAndScheduleTime jobType minScheduleTime maxScheduleTime = do
  findAllWithKVScheduler
    [ Se.And
        [ Se.Is BeamST.status $ Se.Eq Pending,
          Se.Is BeamST.scheduledAt $ Se.GreaterThan (T.utcToLocalTime T.utc minScheduleTime),
          Se.Is BeamST.scheduledAt $ Se.LessThan (T.utcToLocalTime T.utc maxScheduleTime),
          Se.Is BeamST.jobType $ Se.Eq jobType
        ]
    ]

getShardIdKey :: Text
getShardIdKey = "DriverOffer:Jobs:ShardId"

getReadyTasks ::
  forall t r m.
  (FromTType'' BeamST.SchedulerJob (AnyJob t), JobMonad r m) =>
  Maybe Int ->
  m [(AnyJob t, BS.ByteString)]
getReadyTasks mbMaxShards = do
  now <- getCurrentTime
  shardId <-
    case mbMaxShards of
      Just maxShards -> (`mod` maxShards) . fromIntegral <$> Hedis.incr getShardIdKey
      Nothing -> pure 0 -- wouldn't be used to fetch jobs in case of nothing
  res <- findAllWithOptionsKVScheduler [Se.And ([Se.Is BeamST.status $ Se.Eq Pending, Se.Is BeamST.scheduledAt $ Se.LessThanOrEq (T.utcToLocalTime T.utc now)] <> [Se.Is BeamST.shardId $ Se.Eq shardId | isJust mbMaxShards])] (Se.Asc BeamST.scheduledAt) Nothing Nothing
  return $ zip res (map (const "rndm") [1 .. length res])

getReadyTask :: (MonadThrow m, Log m) => m [(AnyJob t, BS.ByteString)]
getReadyTask = throwError (InvalidRequest "Not defined for Db_Based Scheduler") $> []

updateStatus ::
  (JobMonad r m) =>
  JobStatus ->
  Id AnyJob ->
  m ()
updateStatus newStatus (Id jobId) = do
  now <- getCurrentTime
  updateWithKVScheduler
    [ Se.Set BeamST.status newStatus,
      Se.Set BeamST.updatedAt (T.utcToLocalTime T.utc now)
    ]
    [Se.Is BeamST.id (Se.Eq jobId)]

markAsComplete :: forall m r. (JobMonad r m) => Id AnyJob -> m ()
markAsComplete = updateStatus Completed

markAsFailed :: forall m r. (JobMonad r m) => Id AnyJob -> m ()
markAsFailed = updateStatus Failed

updateErrorCountAndFail :: forall m r. (JobMonad r m) => Id AnyJob -> Int -> m ()
updateErrorCountAndFail (Id jobId) fCount = do
  now <- getCurrentTime
  updateWithKVScheduler
    [ Se.Set BeamST.status Failed,
      Se.Set BeamST.currErrors fCount,
      Se.Set BeamST.updatedAt (T.utcToLocalTime T.utc now)
    ]
    [Se.Is BeamST.id (Se.Eq jobId)]

reSchedule :: forall m r. (JobMonad r m) => Id AnyJob -> UTCTime -> m ()
reSchedule (Id jobId) newScheduleTime = do
  now <- getCurrentTime
  updateWithKVScheduler
    [ Se.Set BeamST.updatedAt (T.utcToLocalTime T.utc now),
      Se.Set BeamST.scheduledAt (T.utcToLocalTime T.utc newScheduleTime)
    ]
    [Se.Is BeamST.id (Se.Eq jobId)]

updateFailureCount :: forall m r. (JobMonad r m) => Id AnyJob -> Int -> m ()
updateFailureCount (Id jobId) newCountValue = do
  now <- getCurrentTime
  updateWithKVScheduler
    [ Se.Set BeamST.currErrors newCountValue,
      Se.Set BeamST.updatedAt (T.utcToLocalTime T.utc now)
    ]
    [Se.Is BeamST.id (Se.Eq jobId)]

reScheduleOnError :: forall m r. (JobMonad r m) => Id AnyJob -> Int -> UTCTime -> m ()
reScheduleOnError (Id jobId) newCountValue newScheduleTime = do
  now <- getCurrentTime
  updateWithKVScheduler
    [ Se.Set BeamST.scheduledAt (T.utcToLocalTime T.utc newScheduleTime),
      Se.Set BeamST.currErrors newCountValue,
      Se.Set BeamST.updatedAt (T.utcToLocalTime T.utc now)
    ]
    [Se.Is BeamST.id (Se.Eq jobId)]
