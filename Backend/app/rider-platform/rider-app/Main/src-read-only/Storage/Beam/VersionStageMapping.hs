{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VersionStageMapping where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VersionStageMapping
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VersionStageMappingT f = VersionStageMappingT
  { failureReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    stageData :: B.C f (Kernel.Prelude.Maybe Domain.Types.VersionStageMapping.StageData),
    stageId :: B.C f Kernel.Prelude.Text,
    stageName :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.VersionStageMapping.Status,
    versionId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VersionStageMappingT where
  data PrimaryKey VersionStageMappingT f = VersionStageMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VersionStageMappingId . id

type VersionStageMapping = VersionStageMappingT Identity

$(enableKVPG ''VersionStageMappingT ['id] [])

$(mkTableInstances ''VersionStageMappingT "version_stage_mapping")
