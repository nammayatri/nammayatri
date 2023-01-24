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

module Storage.Tabular.SchedulerJob where

import Data.Singletons
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Types.Logging
import Kernel.Utils.Error
import Kernel.Utils.Text
import qualified Lib.Scheduler.Types as ST
import SharedLogic.Scheduler
import Tools.Error
import Unsafe.Coerce (unsafeCoerce)

derivePersistField "SchedulerJobType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SchedulerJobT sql=scheduler_job
      id Text
      jobType SchedulerJobType
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
  type DomainKey SchedulerJobT = Id (ST.AnyJob SchedulerJobType)
  fromKey (SchedulerJobTKey _id) = Id _id
  toKey (Id id) = SchedulerJobTKey id

instance FromTType SchedulerJobT (ST.AnyJob SchedulerJobType) where
  fromTType :: (MonadThrow m, Log m) => SchedulerJobT -> m (ST.AnyJob SchedulerJobType)
  fromTType SchedulerJobT {..} = do
    case toSing jobType of
      SomeSing SAllocateRental -> buildAnyJob SAllocateRental
    where
      buildAnyJob :: forall (e :: SchedulerJobType) m. (MonadThrow m, Log m, FromJSON (ST.JobContent e), ST.JobTypeConstaints e) => Sing e -> m (ST.AnyJob SchedulerJobType)
      buildAnyJob jt = do
        jobDataDecoded <- decodeFromText jobData & fromMaybeM (InternalError $ "Unable to decode " <> show jobType <> ".")
        return $
          ST.AnyJob $
            ST.Job
              { id = Id id,
                jobType = jt,
                jobData = jobDataDecoded,
                ..
              }

instance ToTType SchedulerJobT (ST.AnyJob SchedulerJobType) where
  toTType (ST.AnyJob job) = do
    let ST.Job {jobType} = job
    case jobType of
      SAllocateRental -> mkSchedulerJobT @'AllocateRental $ unsafeCoerce job
    where
      mkSchedulerJobT :: forall (e :: SchedulerJobType). (ToJSON (ST.JobContent e), ST.JobTypeConstaints e) => ST.Job e -> SchedulerJobT
      mkSchedulerJobT ST.Job {..} = do
        SchedulerJobT
          { id = getId id,
            jobType = fromSing (sing :: Sing e),
            jobData = encodeToText jobData,
            ..
          }
