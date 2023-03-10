{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Scheduler.JobStorageType.DB.Table where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified Lib.Scheduler.Types as ST

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SchedulerJobT sql=scheduler_job
      id Text
      jobType Text
      jobData Text
      scheduledAt UTCTime
      createdAt UTCTime
      updatedAt UTCTime
      maxErrors Int
      currErrors Int
      status ST.JobStatus
      Primary id
      deriving Generic
    |]

instance TEntityKey SchedulerJobT where
  type DomainKey SchedulerJobT = Id ST.AnyJob
  fromKey (SchedulerJobTKey _id) = Id _id
  toKey (Id id) = SchedulerJobTKey id

instance (ST.JobProcessor t) => FromTType SchedulerJobT (ST.AnyJob t) where
  fromTType SchedulerJobT {..} = do
    (ST.AnyJobInfo anyJobInfo) :: ST.AnyJobInfo t <-
      ST.restoreAnyJobInfo @t (ST.StoredJobInfo jobType jobData)
        & fromMaybeM (InternalError "Unable to restore JobInfo.")
    return $
      ST.AnyJob $
        ST.Job
          { id = Id id,
            jobInfo = anyJobInfo,
            ..
          }

instance ToTType SchedulerJobT (ST.AnyJob t) where
  toTType (ST.AnyJob ST.Job {..}) = do
    let storedJobInfo = ST.storeJobInfo jobInfo
    SchedulerJobT
      { id = getId id,
        jobType = ST.storedJobType storedJobInfo,
        jobData = ST.storedJobContent storedJobInfo,
        ..
      }
