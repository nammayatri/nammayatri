{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Lib.Scheduler.Types where

import Data.Singletons
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.GenericPretty

-- Job initializer
-- (here one can think of discarding outdated jobs,
-- using maximumDelay :: (Maybe Int) field)
data JobEntry (e :: t) = (JobTypeConstaints e) =>
  JobEntry
  { jobData :: JobContent e,
    maxErrors :: Int
  }

type family JobContent (e :: t) :: Type

type JobTypeConstaints (e :: t) = (Show (Sing e), SingI e, Eq (Demote t), SingKind t)

type JobDataConstraints d = (Show d)

data AnyJob t = forall (e :: t). (JobTypeConstaints e) => AnyJob (Job e)

data Job (e :: t) = (JobTypeConstaints e) =>
  Job
  { id :: Id (AnyJob t),
    jobType :: Sing e, -- user defined, one per server
    jobData :: JobContent e, -- user defined, one per job handler
    scheduledAt :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    maxErrors :: Int,
    currErrors :: Int,
    status :: JobStatus
  }

data JobStatus = Pending | Completed | Failed
  deriving (Show, Eq, Read, Generic)
  deriving (PrettyShow) via Showable JobStatus

derivePersistField "JobStatus"

data ExecutionResult = Complete | Terminate Text | Retry | ReSchedule UTCTime | DuplicateExecution
  deriving (Show, Generic, Exception)
  deriving (PrettyShow) via Showable ExecutionResult
