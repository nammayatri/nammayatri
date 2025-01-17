{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VersionStopMapping where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.Rollout
import qualified Domain.Types.VersionStopMapping
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VersionStopMappingT f = VersionStopMappingT
  { failureReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    stageData :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.Rollout.StageData)),
    stageId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.VersionStopMapping.Status),
    versionId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table VersionStopMappingT where
  data PrimaryKey VersionStopMappingT f = VersionStopMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VersionStopMappingId . id

type VersionStopMapping = VersionStopMappingT Identity

$(enableKVPG (''VersionStopMappingT) [('id)] [])

$(mkTableInstances (''VersionStopMappingT) "version_stop_mapping")
