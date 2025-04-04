{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MultiModalConfigs where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Time
import Tools.Beam.UtilsTH

data MultiModalConfigsT f = MultiModalConfigsT
  { busFilterTimeBufferInSeconds :: (B.C f Kernel.Types.Time.Seconds),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    enableBusFiltering :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    nearbyDriverSearchRadius :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MultiModalConfigsT where
  data PrimaryKey MultiModalConfigsT f = MultiModalConfigsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MultiModalConfigsId . id

type MultiModalConfigs = MultiModalConfigsT Identity

$(enableKVPG (''MultiModalConfigsT) [('id)] [])

$(mkTableInstances (''MultiModalConfigsT) "multi_modal_configs")
