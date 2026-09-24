{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicyProgressiveDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FarePolicyProgressiveDetailsT f = FarePolicyProgressiveDetailsT
  { baseDistance :: (B.C f Kernel.Types.Common.Meters),
    baseFare :: (B.C f Kernel.Types.Common.Money),
    baseFareAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    deadKmFare :: (B.C f Kernel.Types.Common.Money),
    deadKmFareAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    nightShiftCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.NightShiftCharge)),
    perMinRateDurationBasis :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.FarePolicyProgressiveDetails.PerMinRateDurationBasis)),
    pickupChargesMax :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    pickupChargesMaxAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    pickupChargesMin :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    pickupChargesMinAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    freeWatingTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Minutes)),
    waitingCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.WaitingCharge))
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyProgressiveDetailsT where
  data PrimaryKey FarePolicyProgressiveDetailsT f = FarePolicyProgressiveDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FarePolicyProgressiveDetailsId . farePolicyId

type FarePolicyProgressiveDetails = FarePolicyProgressiveDetailsT Identity

$(enableKVPG (''FarePolicyProgressiveDetailsT) [('farePolicyId)] [])

$(mkTableInstances (''FarePolicyProgressiveDetailsT) "fare_policy_progressive_details")

$(Domain.Types.UtilsTH.mkCacParseInstance (''FarePolicyProgressiveDetailsT))
