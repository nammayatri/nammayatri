module Storage where

import App.Scheduler.Types
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Control.Concurrent
import qualified Data.Map as Map
import Environment (Flow)
import GHC.IO.Unsafe (unsafePerformIO)
import Lib.Scheduler

jobsList :: MVar (Map.Map (Id (AnyJob SchedulerJobType)) (AnyJob SchedulerJobType))
{-# NOINLINE jobsList #-}
jobsList = unsafePerformIO . newMVar $ Map.empty

createJobFunc :: AnyJob SchedulerJobType -> Flow ()
createJobFunc (AnyJob job@Job {id}) = do
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      return $ Map.insert id (AnyJob job) map_

-- create :: AnyJob SchedulerJobType -> SchedulerM ()
-- create =

findAll :: SchedulerM [AnyJob SchedulerJobType]
findAll = liftIO $ Map.elems <$> readMVar jobsList

findById :: Id (AnyJob SchedulerJobType) -> SchedulerM (Maybe (AnyJob SchedulerJobType))
findById jobId = liftIO $ Map.lookup jobId <$> readMVar jobsList

getTasksById :: [Id (AnyJob SchedulerJobType)] -> SchedulerM [AnyJob SchedulerJobType]
getTasksById ids = liftIO $ filter (\(AnyJob Job {id}) -> id `elem` ids) . Map.elems <$> readMVar jobsList

getReadyTasks :: SchedulerM [AnyJob SchedulerJobType]
getReadyTasks = do
  now <- getCurrentTime
  liftIO $ filter (filterFunc now) . Map.elems <$> readMVar jobsList
  where
    filterFunc now (AnyJob Job {scheduledAt, status}) =
      status == Pending && scheduledAt <= now

updateStatus :: JobStatus -> Id (AnyJob SchedulerJobType) -> SchedulerM ()
updateStatus newStatus jobId = do
  now <- getCurrentTime
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      let mbJob = Map.lookup jobId map_
      return $ maybe map_ (\(AnyJob job) -> Map.insert jobId (AnyJob job{status = newStatus, updatedAt = now}) map_) mbJob

markAsComplete :: Id (AnyJob SchedulerJobType) -> SchedulerM ()
markAsComplete = updateStatus Completed

markAsFailed :: Id (AnyJob SchedulerJobType) -> SchedulerM ()
markAsFailed = updateStatus Failed

updateErrorCountAndFail :: Id (AnyJob SchedulerJobType) -> Int -> SchedulerM ()
updateErrorCountAndFail jobId fCount = do
  now <- getCurrentTime
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      let mbJob = Map.lookup jobId map_
      return $ maybe map_ (\(AnyJob job) -> Map.insert jobId (AnyJob job{status = Failed, currErrors = fCount, updatedAt = now}) map_) mbJob

reSchedule :: Id (AnyJob SchedulerJobType) -> UTCTime -> SchedulerM ()
reSchedule jobId newScheduleTime = do
  now <- getCurrentTime
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      let mbJob = Map.lookup jobId map_
      return $ maybe map_ (\(AnyJob job) -> Map.insert jobId (AnyJob job{scheduledAt = newScheduleTime, updatedAt = now}) map_) mbJob

updateFailureCount :: Id (AnyJob SchedulerJobType) -> Int -> SchedulerM ()
updateFailureCount jobId newCountValue = do
  now <- getCurrentTime
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      let mbJob = Map.lookup jobId map_
      return $ maybe map_ (\(AnyJob job) -> Map.insert jobId (AnyJob job{currErrors = newCountValue, updatedAt = now}) map_) mbJob

reScheduleOnError :: Id (AnyJob SchedulerJobType) -> Int -> UTCTime -> SchedulerM ()
reScheduleOnError jobId newCountValue newScheduleTime = do
  now <- getCurrentTime
  liftIO $
    modifyMVar_ jobsList $ \map_ -> do
      let mbJob = Map.lookup jobId map_
      return $ maybe map_ (\(AnyJob job) -> Map.insert jobId (AnyJob job{scheduledAt = newScheduleTime, currErrors = newCountValue, updatedAt = now}) map_) mbJob
