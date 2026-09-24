{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicyAmbulanceDetailsSlab where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FarePolicyAmbulanceDetailsSlabT f = FarePolicyAmbulanceDetailsSlabT
  { baseDistance :: (B.C f Kernel.Types.Common.Meters),
    baseFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    currency :: (B.C f Kernel.Types.Common.Currency),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Int),
    nightShiftCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.NightShiftCharge)),
    perKmRate :: (B.C f Kernel.Types.Common.HighPrecMoney),
    platformFeeCgst :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    platformFeeCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab.PlatformFeeCharge)),
    platformFeeSgst :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    vehicleAge :: (B.C f Kernel.Types.Common.Months),
    freeWaitingTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Minutes)),
    waitingCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.WaitingCharge))
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyAmbulanceDetailsSlabT where
  data PrimaryKey FarePolicyAmbulanceDetailsSlabT f = FarePolicyAmbulanceDetailsSlabId (B.C f Kernel.Prelude.Int) deriving (Generic, B.Beamable)
  primaryKey = FarePolicyAmbulanceDetailsSlabId . id

type FarePolicyAmbulanceDetailsSlab = FarePolicyAmbulanceDetailsSlabT Identity

$(enableKVPG (''FarePolicyAmbulanceDetailsSlabT) [('id)] [[('farePolicyId)]])

$(mkTableInstances (''FarePolicyAmbulanceDetailsSlabT) "fare_policy_ambulance_details_slab")

$(Domain.Types.UtilsTH.mkCacParseInstance (''FarePolicyAmbulanceDetailsSlabT))
