{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BBPSConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data BBPSConfigT f = BBPSConfigT
  { bbpsAgentId :: (B.C f Kernel.Prelude.Text),
    bbpsServerUrl :: (B.C f Kernel.Prelude.Text),
    bbpsSignatureKey :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table BBPSConfigT where
  data PrimaryKey BBPSConfigT f = BBPSConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BBPSConfigId . merchantId

type BBPSConfig = BBPSConfigT Identity

$(enableKVPG (''BBPSConfigT) [('merchantId)] [])

$(mkTableInstances (''BBPSConfigT) "bbps_config")
