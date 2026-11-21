{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleViolation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleViolation
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data VehicleViolationT f = VehicleViolationT
  { approvedBy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    fineAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    id :: B.C f Kernel.Prelude.Text,
    lat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    lon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mediaFileIds :: B.C f [Kernel.Prelude.Text],
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    reportedBy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    reporterPersonId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Domain.Types.VehicleViolation.ViolationStatus,
    ticketServiceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleNumber :: B.C f Kernel.Prelude.Text,
    violationType :: B.C f Domain.Types.VehicleViolation.ViolationType,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleViolationT where
  data PrimaryKey VehicleViolationT f = VehicleViolationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleViolationId . id

type VehicleViolation = VehicleViolationT Identity

$(enableKVPG ''VehicleViolationT ['id] [['vehicleNumber]])

$(mkTableInstances ''VehicleViolationT "vehicle_violation")
