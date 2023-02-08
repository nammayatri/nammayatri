module Lib.Scheduler.ScheduleJob
  ( createJob,
    createJobByTime,
    createJobIn,
  )
where

import Data.Singletons
import Kernel.Prelude hiding (mask, throwIO)
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.Types

createJob ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, JobTypeConstaints e) =>
  (AnyJob t -> m ()) ->
  JobEntry e ->
  m (Id (AnyJob t))
createJob createJobFunc jobEntry = do
  now <- getCurrentTime
  createJobImpl createJobFunc now jobEntry

createJobIn ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, JobTypeConstaints e) =>
  (AnyJob t -> m ()) ->
  NominalDiffTime ->
  JobEntry e ->
  m (Id (AnyJob t))
createJobIn createJobFunc diff jobEntry = do
  now <- getCurrentTime
  when (diff < 0) $ throwError $ InternalError "job can only be scheduled for now or for future"
  let scheduledAt = addUTCTime diff now
  createJobImpl createJobFunc scheduledAt jobEntry

createJobByTime ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, JobTypeConstaints e) =>
  (AnyJob t -> m ()) ->
  UTCTime ->
  JobEntry e ->
  m (Id (AnyJob t))
createJobByTime createJobFunc scheduledAt jobEntry = do
  now <- getCurrentTime
  when (scheduledAt <= now) $
    throwError $
      InternalError
        "job can only be scheduled for the future\
        \ using createJobByTime, for scheduling for\
        \ now use createJobIn function instead"
  createJobImpl createJobFunc scheduledAt jobEntry

createJobImpl ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, JobTypeConstaints e) =>
  (AnyJob t -> m ()) ->
  UTCTime ->
  JobEntry e ->
  m (Id (AnyJob t))
createJobImpl createJobFunc scheduledAt jobEntry = do
  when (jobEntry.maxErrors <= 0) $ throwError $ InternalError "maximum errors should be positive"
  now <- getCurrentTime
  id <- Id <$> generateGUIDText
  let job = makeJob id now
  createJobFunc $ AnyJob job
  pure id
  where
    makeJob id currentTime =
      Job
        { id = id,
          jobType = sing :: Sing e,
          jobData = jobEntry.jobData,
          scheduledAt = scheduledAt,
          maxErrors = jobEntry.maxErrors,
          createdAt = currentTime,
          updatedAt = currentTime,
          currErrors = 0,
          status = Pending
        }
