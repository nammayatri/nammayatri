{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BBPSConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data BBPSConfigT f
    = BBPSConfigT {bbpsAgentId :: (B.C f Kernel.Prelude.Text),
                   bbpsServerUrl :: (B.C f Kernel.Prelude.Text),
                   bbpsSignatureKey :: (B.C f Kernel.Prelude.Text),
                   merchantId :: (B.C f Kernel.Prelude.Text),
                   createdAt :: (B.C f Kernel.Prelude.UTCTime),
                   updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table BBPSConfigT
    where data PrimaryKey BBPSConfigT f = BBPSConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BBPSConfigId . merchantId
type BBPSConfig = BBPSConfigT Identity

$(enableKVPG (''BBPSConfigT) [('merchantId)] [])

$(mkTableInstances (''BBPSConfigT) "bbps_config")

