{-# LANGUAGE DerivingVia #-}

module Beckn.Scheduler.Types where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.GenericPretty

-- Job initializer
data JobEntry = JobEntry
  { jobType :: Text,
    jobData :: Text,
    maxErrors :: Int,
    maximumDelay :: Maybe Int
  }
  deriving (Show, Generic, PrettyShow)

-- Main datatype
data Job = Job
  { id :: Id Job,
    jobType :: Text, -- user defined
    jobData :: Text, -- user defined
    scheduledAt :: UTCTime,
    maximumDelay :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    maxErrors :: Int,
    currErrors :: Int,
    status :: JobStatus
  }
  deriving (Show, Generic, ToJSON, FromJSON, PrettyShow)

data JobStatus = PENDING | COMPLETED | TERMINATED
  deriving (Show, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable JobStatus

data ExecutionResult = Completed | Terminate | Retry | ReSchedule UTCTime
  deriving (Show, Generic, Exception)
  deriving (PrettyShow) via Showable ExecutionResult
