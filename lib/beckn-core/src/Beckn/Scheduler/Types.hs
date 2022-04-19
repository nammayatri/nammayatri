{-# LANGUAGE DerivingVia #-}

module Beckn.Scheduler.Types where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.GenericPretty

-- Job initializer
data JobEntry t d = JobEntry
  { jobType :: t,
    jobData :: d,
    maxErrors :: Int,
    maximumDelay :: Maybe Int
  }
  deriving (Show, Generic, PrettyShow)

type JobEntryText = JobEntry Text Text

-- Main datatype
data Job t d = Job
  { id :: Id (Job t d),
    jobType :: t, -- user defined
    jobData :: d, -- user defined
    scheduledAt :: UTCTime,
    maximumDelay :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    maxErrors :: Int,
    currErrors :: Int,
    status :: JobStatus
  }
  deriving (Eq, Show, Generic, PrettyShow)

setJobData :: d2 -> Job t d1 -> Job t d2
setJobData data2_ Job {..} = Job {id = cast id, jobData = data2_, ..}

type JobText = Job Text Text

data JobStatus = PENDING | COMPLETED | TERMINATED
  deriving (Show, Eq, Read, Generic)
  deriving (PrettyShow) via Showable JobStatus

data ExecutionResult = Completed | Terminate | Retry | ReSchedule UTCTime
  deriving (Show, Generic, Exception)
  deriving (PrettyShow) via Showable ExecutionResult
