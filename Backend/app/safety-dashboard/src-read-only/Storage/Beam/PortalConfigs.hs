{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PortalConfigs where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PortalConfigsT f
    = PortalConfigsT {configName :: (B.C f Kernel.Prelude.Text),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      id :: (B.C f Kernel.Prelude.Text),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                      value :: (B.C f Kernel.Prelude.Text)}
    deriving (Generic, B.Beamable)
instance B.Table PortalConfigsT
    where data PrimaryKey PortalConfigsT f = PortalConfigsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PortalConfigsId . id
type PortalConfigs = PortalConfigsT Identity

$(enableKVPG (''PortalConfigsT) [('id)] [])

$(mkTableInstances (''PortalConfigsT) "portal_configs")

