{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Scheduler.JobStorageType.DB.TableB where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import qualified Database.Beam.Schema.Tables as B
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Kernel.Prelude
import qualified Lib.Scheduler.Types as ST

data SchedulerJobT f = SchedulerJobT
  { id :: B.C f Text,
    jobType :: B.C f Text,
    jobData :: B.C f Text,
    shardId :: B.C f Int,
    scheduledAt :: B.C f UTCTime,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    maxErrors :: B.C f Int,
    currErrors :: B.C f Int,
    status :: B.C f ST.JobStatus
  }
  deriving (Generic, B.Beamable)

instance FromField ST.JobStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ST.JobStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be ST.JobStatus

instance FromBackendRow Postgres ST.JobStatus

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError DPSF.UnexpectedNull f mempty
  Just value' ->
    case readMaybe (unpackChars value') of
      Just val -> pure val
      _ -> DPSF.returnError DPSF.ConversionFailed f "Could not 'read' value for 'Rule'."

instance B.Table SchedulerJobT where
  data PrimaryKey SchedulerJobT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SchedulerJob = SchedulerJobT Identity

instance FromJSON SchedulerJob where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SchedulerJob where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SchedulerJob

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
      status = B.fieldNamed "status"
    }

data AtlasDB f = AtlasDB
  { schedulerJob :: f (B.TableEntity SchedulerJobT)
  }
  deriving (Generic, B.Database be)

atlasDB :: B.DatabaseSettings be AtlasDB
atlasDB =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { schedulerJob = schedulerJobTable
      }

schedulerJobTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity SchedulerJobT)
schedulerJobTable =
  B.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "scheduler_job"
    <> B.modifyTableFields schedulerJobTMod
