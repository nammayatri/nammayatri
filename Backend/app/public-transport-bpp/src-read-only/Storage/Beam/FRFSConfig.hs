{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSConfigT f = FRFSConfigT
  { bppId :: (B.C f Kernel.Prelude.Text),
    config :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSConfigT where
  data PrimaryKey FRFSConfigT f = FRFSConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSConfigId . id

type FRFSConfig = FRFSConfigT Identity

$(enableKVPG (''FRFSConfigT) [('id)] [])

$(mkTableInstances (''FRFSConfigT) "frfs_config")
