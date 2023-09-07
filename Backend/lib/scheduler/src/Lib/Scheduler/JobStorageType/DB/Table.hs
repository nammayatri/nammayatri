{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Scheduler.JobStorageType.DB.Table where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Lib.Scheduler.Types as ST

data SchedulerJobT f = SchedulerJobT
  { id :: B.C f Text,
    jobType :: B.C f Text,
    jobData :: B.C f Text,
    shardId :: B.C f Int,
    scheduledAt :: B.C f LocalTime,
    createdAt :: B.C f LocalTime,
    updatedAt :: B.C f LocalTime,
    maxErrors :: B.C f Int,
    currErrors :: B.C f Int,
    status :: B.C f ST.JobStatus,
    parentJobId :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table SchedulerJobT where
  data PrimaryKey SchedulerJobT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SchedulerJob = SchedulerJobT Identity

$(enableKVPG ''SchedulerJobT ['id] [])

$(mkTableInstances ''SchedulerJobT "scheduler_job" "atlas_driver_offer_bpp")
