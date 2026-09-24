{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicySlabsDetailsSlab where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FarePolicySlabsDetailsSlabT f = FarePolicySlabsDetailsSlabT
  { baseFare :: (B.C f Kernel.Types.Common.Money),
    baseFareAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    nightShiftCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.NightShiftCharge)),
    platformFeeCgst :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    platformFeeCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab.PlatformFeeCharge)),
    platformFeeSgst :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    startDistance :: (B.C f Kernel.Types.Common.Meters),
    freeWatingTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Minutes)),
    waitingCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.WaitingCharge))
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicySlabsDetailsSlabT where
  data PrimaryKey FarePolicySlabsDetailsSlabT f = FarePolicySlabsDetailsSlabId (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)) deriving (Generic, B.Beamable)
  primaryKey = FarePolicySlabsDetailsSlabId . id

type FarePolicySlabsDetailsSlab = FarePolicySlabsDetailsSlabT Identity

$(enableKVPG (''FarePolicySlabsDetailsSlabT) [('id)] [[('farePolicyId)]])

$(mkTableInstances (''FarePolicySlabsDetailsSlabT) "fare_policy_slabs_details_slab")
