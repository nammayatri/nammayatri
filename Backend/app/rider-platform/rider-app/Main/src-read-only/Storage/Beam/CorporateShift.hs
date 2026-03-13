{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CorporateShift where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CorporateShiftT f = CorporateShiftT
  { id :: B.C f Kernel.Prelude.Text,
    corporateEntityId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    pickupWindowStart :: B.C f Kernel.Prelude.Text,
    pickupWindowEnd :: B.C f Kernel.Prelude.Text,
    dropWindowStart :: B.C f Kernel.Prelude.Text,
    dropWindowEnd :: B.C f Kernel.Prelude.Text,
    activeDays :: B.C f Kernel.Prelude.Text,
    isNightShift :: B.C f Kernel.Prelude.Bool,
    maxOccupancy :: B.C f Kernel.Prelude.Int,
    allowedVehicleTiers :: B.C f Kernel.Prelude.Text,
    confirmationDeadlineMinutes :: B.C f Kernel.Prelude.Int,
    status :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CorporateShiftT where
  data PrimaryKey CorporateShiftT f = CorporateShiftId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CorporateShiftId . id

type CorporateShift = CorporateShiftT Identity

$(enableKVPG ''CorporateShiftT ['id] [['corporateEntityId]])

$(mkTableInstances ''CorporateShiftT "corporate_shift")
