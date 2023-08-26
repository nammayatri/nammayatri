{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Scheduler.JobStorageType.DB.Table where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import qualified Lib.Scheduler.Types as ST
import Sequelize

instance FromField ST.JobStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ST.JobStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be ST.JobStatus

instance FromBackendRow Postgres ST.JobStatus

instance IsString ST.JobStatus where
  fromString = show

data SchedulerJobT f = SchedulerJobT
  { id :: B.C f Text,
    jobType :: B.C f Text,
    jobData :: B.C f Text,
    shardId :: B.C f Int,
    scheduledAt :: B.C f Time.LocalTime,
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime,
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

schedulerJobTMod :: SchedulerJobT (B.FieldModification (B.TableField SchedulerJobT))
schedulerJobTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      jobType = B.fieldNamed "job_type",
      jobData = B.fieldNamed "job_data",
      shardId = B.fieldNamed "shard_id",
      scheduledAt = B.fieldNamed "scheduled_at",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      maxErrors = B.fieldNamed "max_errors",
      currErrors = B.fieldNamed "curr_errors",
      status = B.fieldNamed "status",
      parentJobId = B.fieldNamed "parent_job_id"
    }

$(enableKVPG ''SchedulerJobT ['id] [])

$(mkTableInstances ''SchedulerJobT "scheduler_job" "atlas_driver_offer_bpp")
