{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE RankNTypes #-}

module Lib.Scheduler.ScheduleJob
  ( createJob,
    createJobByTime,
    createJobIn,
    partitionScheduledJobs,
  )
where

import Data.Singletons
import Data.Time.Clock.System ()
import qualified Data.UUID as UU
import EulerHS.Prelude hiding (id, map)
import Kernel.Prelude hiding (mask, throwIO)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Lib.Scheduler.Environment
import Lib.Scheduler.Types

createJob ::
  forall t (e :: t) m r.
  (JobFlow t e, JobMonad r m) =>
  Maybe (Id (MerchantType t)) ->
  Maybe (Id (MerchantOperatingCityType t)) ->
  Text ->
  (AnyJob t -> m ()) ->
  Maybe UTCTime ->
  Int ->
  JobEntry e ->
  m (Id AnyJob)
createJob merchantId merchantOperatingCityId uuid createJobFunc jobExpireAt maxShards jobEntry = do
  now <- getCurrentTime
  createJobImpl merchantId merchantOperatingCityId uuid createJobFunc now jobExpireAt maxShards jobEntry

createJobIn ::
  forall t (e :: t) m r.
  (JobFlow t e, JobMonad r m) =>
  Maybe (Id (MerchantType t)) ->
  Maybe (Id (MerchantOperatingCityType t)) ->
  Text ->
  (AnyJob t -> m ()) ->
  NominalDiffTime ->
  Maybe UTCTime ->
  Int ->
  JobEntry e ->
  m (Id AnyJob)
createJobIn merchantId merchantOperatingCityId uuid createJobFunc diff jobExpireAt maxShards jobEntry = do
  now <- getCurrentTime
  when (diff < 0) $ throwError $ InternalError "job can only be scheduled for now or for future"
  let scheduledAt = addUTCTime diff now
  createJobImpl merchantId merchantOperatingCityId uuid createJobFunc scheduledAt jobExpireAt maxShards jobEntry

-- createJobIn' ::
--   forall t (e :: t) m.
--   (MonadTime m, L.MonadFlow m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
--   (AnyJob t -> m ()) ->
--   NominalDiffTime ->
--   Int ->
--   JobEntry e ->
--   m (Id AnyJob)
-- createJobIn' createJobFunc diff maxShards jobEntry = do
--   now <- getCurrentTime
--   when (diff < 0) $ throwError $ InternalError "job can only be scheduled for now or for future"
--   let scheduledAt = addUTCTime diff now
--   createJobImpl' createJobFunc scheduledAt maxShards jobEntry

createJobByTime ::
  forall t (e :: t) m r.
  (JobFlow t e, JobMonad r m) =>
  Maybe (Id (MerchantType t)) ->
  Maybe (Id (MerchantOperatingCityType t)) ->
  Text ->
  (AnyJob t -> m ()) ->
  UTCTime ->
  Maybe UTCTime ->
  Int ->
  JobEntry e ->
  m (Id AnyJob)
createJobByTime merchantId merchantOperatingCityId uuid createJobFunc scheduledAt jobExpireAt maxShards jobEntry = do
  now <- getCurrentTime
  when (scheduledAt <= now) $
    throwError $
      InternalError
        "job can only be scheduled for the future\
        \ using createJobByTime, for scheduling for\
        \ now use createJobIn function instead"
  createJobImpl merchantId merchantOperatingCityId uuid createJobFunc scheduledAt jobExpireAt maxShards jobEntry

createJobImpl ::
  forall t (e :: t) m r.
  (JobFlow t e, JobMonad r m) =>
  Maybe (Id (MerchantType t)) ->
  Maybe (Id (MerchantOperatingCityType t)) ->
  Text ->
  (AnyJob t -> m ()) ->
  UTCTime ->
  Maybe UTCTime ->
  Int ->
  JobEntry e ->
  m (Id AnyJob)
createJobImpl merchantId merchantOperatingCityId uuid createJobFunc scheduledAt jobExpireAt maxShards JobEntry {..} = do
  when (maxErrors <= 0) $ throwError $ InternalError "maximum errors should be positive"
  now <- getCurrentTime
  let id = Id uuid
  let shardId :: Int = idToShardNumber . fromJust $ UU.fromText uuid -- using fromJust because its never going to fail
  let job = makeJob shardId id now
  createJobFunc $ AnyJob job
  pure id
  where
    idToShardNumber uuidTxt = fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords uuidTxt)) `mod` maxShards
    makeJob shardId id currentTime =
      Job
        { id = id,
          jobInfo = JobInfo (sing :: Sing e) jobData,
          shardId = shardId,
          scheduledAt = scheduledAt,
          maxErrors = maxErrors,
          createdAt = currentTime,
          updatedAt = currentTime,
          currErrors = 0,
          status = Pending,
          parentJobId = id,
          merchantId,
          merchantOperatingCityId,
          jobExpireAt
        }

-- createJobImpl' ::
--   forall t (e :: t) m.
--   (MonadTime m, L.MonadFlow m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t), ToJSON t) =>
--   (AnyJob t -> m ()) ->
--   UTCTime ->
--   Int ->
--   JobEntry e ->
--   m (Id AnyJob)
-- createJobImpl' createJobFunc scheduledAt maxShards JobEntry {..} = do
--   when (maxErrors <= 0) $ throwError $ InternalError "maximum errors should be positive"
--   now <- getCurrentTime
--   uuid <- generateGUIDText
--   let id = Id uuid
--   let shardId :: Int = idToShardNumber . fromJust $ UU.fromText uuid -- using fromJust because its never going to fail
--   let job = makeJob shardId id now
--   createJobFunc $ AnyJob job
--   pure id
--   where
--     idToShardNumber uuid = fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords uuid)) `mod` maxShards
--     makeJob shardId id currentTime =
--       Job
--         { id = id,
--           jobInfo = JobInfo (sing :: Sing e) jobData,
--           shardId = shardId,
--           scheduledAt = scheduledAt,
--           maxErrors = maxErrors,
--           createdAt = currentTime,
--           updatedAt = currentTime,
--           currErrors = 0,
--           status = Pending
--         }

validateScheduledJob :: UTCTime -> AnyJob t -> Bool
validateScheduledJob now (AnyJob job) = case job.jobExpireAt of
  Just expireAt -> job.scheduledAt < expireAt && expireAt >= now
  Nothing -> True

partitionScheduledJobs :: UTCTime -> [Either String (AnyJob t)] -> ([AnyJob t], [(Text, Id AnyJob)])
partitionScheduledJobs now jobs =
  let processJob (Right job@(AnyJob j)) =
        if validateScheduledJob now job
          then Left job
          else Right (show (fromSing $ jobType $ jobInfo j), id j)
      processJob (Left _) = Right ("", Id "")
   in partitionEithers $ map processJob jobs
