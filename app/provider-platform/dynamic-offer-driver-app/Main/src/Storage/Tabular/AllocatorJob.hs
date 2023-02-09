{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.AllocatorJob where

import Data.Singletons
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Types.Logging
import Kernel.Utils.Error
import Kernel.Utils.Text
import qualified Lib.Scheduler.Types as ST
import SharedLogic.Allocator
import Tools.Error
import Unsafe.Coerce (unsafeCoerce)

derivePersistField "AllocatorJobType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    AllocatorJobT sql=allocator_job
      id Text
      jobType AllocatorJobType
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

instance TEntityKey AllocatorJobT where
  type DomainKey AllocatorJobT = Id (ST.AnyJob AllocatorJobType)
  fromKey (AllocatorJobTKey _id) = Id _id
  toKey (Id id) = AllocatorJobTKey id

instance TType AllocatorJobT (ST.AnyJob AllocatorJobType) where
  fromTType :: (MonadThrow m, Log m) => AllocatorJobT -> m (ST.AnyJob AllocatorJobType)
  fromTType AllocatorJobT {..} = do
    case toSing jobType of
      SomeSing SSendSearchRequestToDriver -> buildAnyJob SSendSearchRequestToDriver
    where
      buildAnyJob :: forall (e :: AllocatorJobType) m. (MonadThrow m, Log m, FromJSON (ST.JobContent e), ST.JobTypeConstaints e) => Sing e -> m (ST.AnyJob AllocatorJobType)
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

  toTType (ST.AnyJob job) = do
    let ST.Job {jobType} = job
    case jobType of
      SSendSearchRequestToDriver -> mkAllocatorJobT @'SendSearchRequestToDriver $ unsafeCoerce job
    where
      mkAllocatorJobT :: forall (e :: AllocatorJobType). (ToJSON (ST.JobContent e), ST.JobTypeConstaints e) => ST.Job e -> AllocatorJobT
      mkAllocatorJobT ST.Job {..} = do
        AllocatorJobT
          { id = getId id,
            jobType = fromSing (sing :: Sing e),
            jobData = encodeToText jobData,
            ..
          }
