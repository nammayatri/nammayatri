{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PortalConfigs where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PortalConfigsT f = PortalConfigsT
  { configName :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    value :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table PortalConfigsT where
  data PrimaryKey PortalConfigsT f = PortalConfigsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PortalConfigsId . id

type PortalConfigs = PortalConfigsT Identity

$(enableKVPG ''PortalConfigsT ['id] [])

$(mkTableInstances ''PortalConfigsT "portal_configs")
