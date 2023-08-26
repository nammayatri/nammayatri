{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage where

import App.Scheduler.Types
import Control.Concurrent
import qualified Data.Map as Map
import Environment (Flow)
import GHC.IO.Unsafe (unsafePerformIO)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Scheduler

jobsList :: MVar (Map.Map (Id AnyJob) (AnyJob SchedulerJobType))
{-# NOINLINE jobsList #-}
jobsList = unsafePerformIO . newMVar $ Map.empty

createJobFunc :: AnyJob SchedulerJobType -> Flow ()
createJobFunc (AnyJob job@Job {id}) = do
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      return $ Map.insert id (AnyJob job) map_

findAll :: SchedulerM [AnyJob SchedulerJobType]
findAll = liftIO $ Map.elems <$> readMVar jobsList

findById :: Id AnyJob -> SchedulerM (Maybe (AnyJob SchedulerJobType))
findById jobId = liftIO $ Map.lookup jobId <$> readMVar jobsList

getTasksById :: [Id AnyJob] -> SchedulerM [AnyJob SchedulerJobType]
getTasksById ids = liftIO $ filter (\(AnyJob Job {id}) -> id `elem` ids) . Map.elems <$> readMVar jobsList

getReadyTasks :: SchedulerM [AnyJob SchedulerJobType]
getReadyTasks = do
  now <- getCurrentTime
  liftIO $ filter (filterFunc now) . Map.elems <$> readMVar jobsList
  where
    filterFunc :: UTCTime -> AnyJob SchedulerJobType -> Bool
    filterFunc now (AnyJob Job {scheduledAt, status}) =
      status == Pending && scheduledAt <= now

updateStatus :: JobStatus -> Id AnyJob -> SchedulerM ()
updateStatus newStatus jobId = do
  now <- getCurrentTime
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      let mbJob = Map.lookup jobId map_
      return $ maybe map_ (\(AnyJob job) -> Map.insert jobId (AnyJob job{status = newStatus, updatedAt = now}) map_) mbJob

markAsComplete :: Text -> Id AnyJob -> SchedulerM ()
markAsComplete _ = updateStatus Completed

markAsFailed :: Text -> Id AnyJob -> SchedulerM ()
markAsFailed _ = updateStatus Failed

updateErrorCountAndFail :: Text -> Id AnyJob -> Int -> SchedulerM ()
updateErrorCountAndFail _ jobId fCount = do
  now <- getCurrentTime
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      let mbJob = Map.lookup jobId map_
      return $ maybe map_ (\(AnyJob job) -> Map.insert jobId (AnyJob job{status = Failed, currErrors = fCount, updatedAt = now}) map_) mbJob

reSchedule :: Text -> AnyJob t -> UTCTime -> SchedulerM ()
reSchedule _ (AnyJob x) newScheduleTime = do
  now <- getCurrentTime
  let jobId :: Id AnyJob = x.id
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      let mbJob = Map.lookup jobId map_
      return $ maybe map_ (\(AnyJob job) -> Map.insert jobId (AnyJob job{scheduledAt = newScheduleTime, updatedAt = now}) map_) mbJob

updateFailureCount :: Text -> Id AnyJob -> Int -> SchedulerM ()
updateFailureCount _ jobId newCountValue = do
  now <- getCurrentTime
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      let mbJob = Map.lookup jobId map_
      return $ maybe map_ (\(AnyJob job) -> Map.insert jobId (AnyJob job{currErrors = newCountValue, updatedAt = now}) map_) mbJob

reScheduleOnError :: Text -> AnyJob t -> Int -> UTCTime -> SchedulerM ()
reScheduleOnError _ (AnyJob x) newCountValue newScheduleTime = do
  now <- getCurrentTime
  let jobId :: Id AnyJob = x.id
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      let mbJob = Map.lookup jobId map_
      return $ maybe map_ (\(AnyJob job) -> Map.insert jobId (AnyJob job{scheduledAt = newScheduleTime, currErrors = newCountValue, updatedAt = now}) map_) mbJob
