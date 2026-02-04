{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.EDCMachineMapping where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data EDCMachineMappingT f = EDCMachineMappingT
  { clientId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    createdBy :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    id :: (B.C f Kernel.Prelude.Text),
    isActive :: (B.C f Kernel.Prelude.Bool),
    machineName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantChannelId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantKey :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    paytmMid :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    terminalId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table EDCMachineMappingT where
  data PrimaryKey EDCMachineMappingT f = EDCMachineMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = EDCMachineMappingId . id

type EDCMachineMapping = EDCMachineMappingT Identity

$(enableKVPG (''EDCMachineMappingT) [('id)] [])

$(mkTableInstances (''EDCMachineMappingT) "edc_machine_mapping")
