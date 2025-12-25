{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RentalDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Utils.Common
import Tools.Beam.UtilsTH

data RentalDetailsT f = RentalDetailsT
  { baseFare :: B.C f Kernel.Utils.Common.Money,
    baseFareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Currency),
    deadKmFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    id :: B.C f Kernel.Prelude.Text,
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    includedDistancePerHrValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    includedKmPerHr :: B.C f Kernel.Types.Common.Kilometers,
    nightShiftCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Money),
    nightShiftChargeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    nightShiftEnd :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay),
    nightShiftStart :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay),
    perExtraKmRate :: B.C f Kernel.Utils.Common.Money,
    perExtraKmRateAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    perExtraMinRate :: B.C f Kernel.Utils.Common.Money,
    perExtraMinRateAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    perHourCharge :: B.C f Kernel.Utils.Common.Money,
    perHourChargeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    plannedPerKmRate :: B.C f Kernel.Utils.Common.Money,
    plannedPerKmRateAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)
  }
  deriving (Generic, B.Beamable)

instance B.Table RentalDetailsT where
  data PrimaryKey RentalDetailsT f = RentalDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RentalDetailsId . id

type RentalDetails = RentalDetailsT Identity

$(enableKVPG ''RentalDetailsT ['id] [])

$(mkTableInstances ''RentalDetailsT "rental_details")
