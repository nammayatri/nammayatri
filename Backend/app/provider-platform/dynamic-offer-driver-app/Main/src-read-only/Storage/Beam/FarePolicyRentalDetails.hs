{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicyRentalDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FarePolicyRentalDetailsT f = FarePolicyRentalDetailsT
  { baseFare :: (B.C f Kernel.Types.Common.Money),
    baseFareAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    deadKmFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    includedKmPerHr :: (B.C f Kernel.Types.Common.Kilometers),
    maxAdditionalKmsLimit :: (B.C f Kernel.Types.Common.Kilometers),
    nightShiftCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.NightShiftCharge)),
    perExtraKmRate :: (B.C f Kernel.Types.Common.Money),
    perExtraKmRateAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    perExtraMinRate :: (B.C f Kernel.Types.Common.Money),
    perExtraMinRateAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    perHourCharge :: (B.C f Kernel.Types.Common.Money),
    perHourChargeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    plannedPerKmRate :: (B.C f Kernel.Types.Common.Money),
    plannedPerKmRateAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    totalAdditionalKmsLimit :: (B.C f Kernel.Types.Common.Kilometers),
    freeWaitingTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Minutes)),
    waitingCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.WaitingCharge))
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyRentalDetailsT where
  data PrimaryKey FarePolicyRentalDetailsT f = FarePolicyRentalDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FarePolicyRentalDetailsId . farePolicyId

type FarePolicyRentalDetails = FarePolicyRentalDetailsT Identity

$(enableKVPG (''FarePolicyRentalDetailsT) [('farePolicyId)] [])

$(mkTableInstances (''FarePolicyRentalDetailsT) "fare_policy_rental_details")

$(Domain.Types.UtilsTH.mkCacParseInstance (''FarePolicyRentalDetailsT))
