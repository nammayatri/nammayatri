{-# LANGUAGE DerivingVia #-}

module Beckn.Scheduler.Types where

import Beckn.Prelude
import Beckn.Scheduler.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.GenericPretty

-- Job initializer
-- (here one can think of discarding outdated jobs,
-- using maximumDelay :: (Maybe Int) field)
data JobEntry t d = JobEntry
  { jobType :: t,
    jobData :: d,
    maxErrors :: Int
  }
  deriving (Show, Generic, PrettyShow)

type JobEntryText = JobEntry Text Text

-- Main datatype
data Job t d = Job
  { id :: Id (Job t d),
    jobType :: t, -- user defined, one per server
    jobData :: d, -- user defined, one per job handler
    scheduledAt :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    maxErrors :: Int,
    currErrors :: Int,
    status :: JobStatus
  }
  deriving (Eq, Show, Generic, PrettyShow)

setJobType :: t2 -> Job t1 d -> Job t2 d
setJobType type2_ Job {..} = Job {id = cast id, jobType = type2_, ..}

setJobData :: d2 -> Job t d1 -> Job t d2
setJobData data2_ Job {..} = Job {id = cast id, jobData = data2_, ..}

type JobTypeConstraints a = (Eq a, Ord a, Show a, FromJSON a, ToJSON a)

type JobDataConstraints a = (Eq a, Show a, FromJSON a, ToJSON a)

encodeJob :: (JobTypeConstraints a, JobDataConstraints b) => Job a b -> JobText
encodeJob Job {..} =
  Job
    { id = cast id,
      jobType = encodeToText jobType,
      jobData = encodeToText jobData,
      ..
    }

decodeJob :: forall a b. (JobTypeConstraints a, JobDataConstraints b) => JobText -> Either JobDecodeError (Job a b)
decodeJob Job {..} = do
  jobType_ <- maybe (Left $ InvalidJobType jobType) Right $ decodeFromText jobType
  jobData_ <- maybe (Left $ InvalidJobData jobData) Right $ decodeFromText jobData
  pure
    Job
      { id = cast id,
        jobType = jobType_,
        jobData = jobData_,
        ..
      }

type JobText = Job Text Text

data JobStatus = Pending | Completed | Terminated
  deriving (Show, Eq, Read, Generic)
  deriving (PrettyShow) via Showable JobStatus

data ExecutionResult = Complete | Terminate | Retry | ReSchedule UTCTime
  deriving (Show, Generic, Exception)
  deriving (PrettyShow) via Showable ExecutionResult
