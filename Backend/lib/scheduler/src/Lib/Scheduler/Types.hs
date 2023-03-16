{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.Scheduler.Types where

import Data.Singletons
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty

-- Job initializer
-- (here one can think of discarding outdated jobs,
-- using maximumDelay :: (Maybe Int) field)
data JobEntry (e :: t) = (JobProcessor t, JobInfoProcessor e) =>
  JobEntry
  { jobData :: JobContent e,
    maxErrors :: Int
  }

data StoredJobInfo = StoredJobInfo
  { storedJobType :: Text,
    storedJobContent :: Text
  }
  deriving (Generic)

data AnyJobInfo t = forall (e :: t). (JobProcessor t, JobInfoProcessor e) => AnyJobInfo (JobInfo e)

data JobInfo (e :: t) = (JobProcessor t, JobInfoProcessor e) =>
  JobInfo
  { jobType :: Sing e, -- user defined, one per server
    jobData :: JobContent e -- user defined, one per job handler
  }

class (Show t, Read t, Demote t ~ t, SingKind t, Eq t) => JobProcessor t where
  restoreAnyJobInfoMain :: StoredJobInfo -> Maybe (AnyJobInfo t)
  default restoreAnyJobInfoMain :: StoredJobInfo -> Maybe (AnyJobInfo t)
  restoreAnyJobInfoMain storedContent = do
    jobType :: t <- restoreJobType (storedJobType storedContent)
    withSomeSing jobType $ \jobTypeSing -> do
      restoreAnyJobInfo jobTypeSing $ storedJobContent storedContent

  restoreJobType :: Text -> Maybe t
  default restoreJobType :: (Read t) => Text -> Maybe t
  restoreJobType storedJobType = readMaybe $ T.unpack storedJobType

  restoreAnyJobInfo :: Sing (e :: t) -> Text -> Maybe (AnyJobInfo t)

type family JobContent (e :: t) :: Type

class (JobProcessor t) => JobInfoProcessor (e :: t) where
  storeJobInfo :: JobInfo (e :: t) -> StoredJobInfo
  default storeJobInfo :: (Show t, ToJSON (JobContent e)) => JobInfo e -> StoredJobInfo
  storeJobInfo JobInfo {..} =
    StoredJobInfo
      { storedJobType = show $ fromSing jobType,
        storedJobContent = encodeToText jobData
      }

  restoreJobInfo :: Sing e -> Text -> Maybe (JobInfo (e :: t))
  default restoreJobInfo :: (FromJSON (JobContent e)) => Sing e -> Text -> Maybe (JobInfo (e :: t))
  restoreJobInfo jobType storedContent = do
    content <- decodeFromText storedContent
    return $ JobInfo jobType content

data AnyJob t = forall (e :: t). (JobProcessor t, JobInfoProcessor e) => AnyJob (Job e)

data Job (e :: t) = (JobProcessor t, JobInfoProcessor e) =>
  Job
  { id :: Id AnyJob,
    jobInfo :: JobInfo e,
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
