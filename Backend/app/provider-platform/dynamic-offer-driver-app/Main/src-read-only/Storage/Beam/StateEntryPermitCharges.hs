{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.StateEntryPermitCharges where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data StateEntryPermitChargesT f = StateEntryPermitChargesT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    geomId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table StateEntryPermitChargesT where
  data PrimaryKey StateEntryPermitChargesT f = StateEntryPermitChargesId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = StateEntryPermitChargesId . id

type StateEntryPermitCharges = StateEntryPermitChargesT Identity

$(enableKVPG (''StateEntryPermitChargesT) [('id)] [[('geomId)]])

$(mkTableInstances (''StateEntryPermitChargesT) "state_entry_permit_charges")
