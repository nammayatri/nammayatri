{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.Scheduler.JobStorageType.DB.Queries where

import Data.Singletons (SingI)
import Kernel.Data.HeterogeneousList
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Lib.Scheduler.Environment
import Lib.Scheduler.JobStorageType.DB.Table
import qualified Lib.Scheduler.ScheduleJob as ScheduleJob
import Lib.Scheduler.Types

createJob :: forall t (e :: t). (SingI e, JobInfoProcessor e, JobProcessor t) => JobContent e -> Esq.SqlDB ()
createJob jobData = do
  void $
    ScheduleJob.createJob @t @e @Esq.SqlDB Esq.create $
      JobEntry
        { jobData = jobData,
          maxErrors = 5
        }

createJobIn :: forall t (e :: t). (SingI e, JobInfoProcessor e, JobProcessor t) => NominalDiffTime -> JobContent e -> Esq.SqlDB ()
createJobIn inTime jobData = do
  void $
    ScheduleJob.createJobIn @t @e @Esq.SqlDB Esq.create inTime $
      JobEntry
        { jobData = jobData,
          maxErrors = 5
        }

createJobByTime :: forall t (e :: t). (SingI e, JobInfoProcessor e, JobProcessor t) => UTCTime -> JobContent e -> Esq.SqlDB ()
createJobByTime byTime jobData = do
  void $
    ScheduleJob.createJobByTime @t @e @Esq.SqlDB Esq.create byTime $
      JobEntry
        { jobData = jobData,
          maxErrors = 5
        }

findAll :: FromTType SchedulerJobT (AnyJob t) => SchedulerM [AnyJob t]
findAll = Esq.findAll $ from $ table @SchedulerJobT

findById :: FromTType SchedulerJobT (AnyJob t) => Id AnyJob -> SchedulerM (Maybe (AnyJob t))
findById = Esq.findById

-- WITH ExtraFeeList AS (
-- 	SELECT id, unnest(driver_extra_fee_list) AS extraFee
-- 	FROM   atlas_driver_offer_bpp.fare_policy
-- ), MinMaxExtraFee AS (
-- 	SELECT id, min (extraFee), max (extraFee)
-- 	FROM ExtraFeeList
-- 	GROUP BY id
-- )
-- UPDATE atlas_driver_offer_bpp.fare_policy AS T1 SET driver_min_extra_fee = T2.min, driver_max_extra_fee = T2.max
--   FROM MinMaxExtraFee AS T2
--   WHERE T1.id = T2.id;

takeReadyTasks :: forall t. FromTType SchedulerJobT (AnyJob t) => Int -> SchedulerM [AnyJob t]
takeReadyTasks lim = do
  now <- getCurrentTime
  Esq.runTransaction $ do
    Esq.rawSql @(Entity SchedulerJobT) @(AnyJob t)
      "WITH SelectedJobs AS (  \
      \ SELECT ?  \
      \ FROM ?  \
      \ WHERE ? = ?  \
      \ LIMIT  ? \
      \ ) \
      \ UPDATE ? SET ? = ? \
      \ WHERE ? IN SelectedJobs \
      \ RETURNING ?;"
      ( fieldName SchedulerJobId
          :<+> tableName @SchedulerJobT
          :<+> fieldName SchedulerJobStatus
          :<+> Pending
          :<+> lim
          :<+> tableName @SchedulerJobT
          :<+> (fieldName SchedulerJobStatus, fieldName SchedulerJobScheduledAt)
          :<+> (InProgress, now)
          :<+> fieldName SchedulerJobId
          :<+> fieldName SchedulerJobId
          :<+> mempty
      )

-- Esq.findAll $ do
--   job <- from $ table @SchedulerJobT
--   where_ $
--     job ^. SchedulerJobStatus ==. val Pending
--       &&. job ^. SchedulerJobScheduledAt <=. val now
--   orderBy [asc $ job ^. SchedulerJobScheduledAt]
--   pure $ job ^. SchedulerJobTId

updateStatus :: JobStatus -> Id AnyJob -> SchedulerM ()
updateStatus newStatus jobId = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [SchedulerJobStatus =. val newStatus, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

markAsComplete :: Id AnyJob -> SchedulerM ()
markAsComplete = updateStatus Completed

markAsFailed :: Id AnyJob -> SchedulerM ()
markAsFailed = updateStatus Failed

updateErrorCountAndFail :: Id AnyJob -> Int -> SchedulerM ()
updateErrorCountAndFail jobId fCount = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [SchedulerJobStatus =. val Failed, SchedulerJobCurrErrors =. val fCount, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

reSchedule :: Id AnyJob -> UTCTime -> SchedulerM ()
reSchedule jobId newScheduleTime = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set
      job
      [ SchedulerJobScheduledAt =. val newScheduleTime,
        SchedulerJobUpdatedAt =. val now,
        SchedulerJobStatus =. val Pending
      ]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

updateFailureCount :: Id AnyJob -> Int -> SchedulerM ()
updateFailureCount jobId newCountValue = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set job [SchedulerJobCurrErrors =. val newCountValue, SchedulerJobUpdatedAt =. val now]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId

reScheduleOnError :: Id AnyJob -> Int -> UTCTime -> SchedulerM ()
reScheduleOnError jobId newCountValue newScheduleTime = do
  now <- getCurrentTime
  Esq.runTransaction . Esq.update $ \job -> do
    set
      job
      [ SchedulerJobScheduledAt =. val newScheduleTime,
        SchedulerJobUpdatedAt =. val now,
        SchedulerJobCurrErrors =. val newCountValue
      ]
    where_ $ job ^. SchedulerJobId ==. val jobId.getId
