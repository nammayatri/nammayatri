{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicyDriverExtraFeeBounds where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FarePolicyDriverExtraFeeBoundsT f = FarePolicyDriverExtraFeeBoundsT
  { defaultStepFee :: (B.C f Kernel.Types.Common.Money),
    defaultStepFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    maxFee :: (B.C f Kernel.Types.Common.Money),
    maxFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    minFee :: (B.C f Kernel.Types.Common.Money),
    minFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    startDistance :: (B.C f Kernel.Types.Common.Meters),
    stepFee :: (B.C f Kernel.Types.Common.Money),
    stepFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney))
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyDriverExtraFeeBoundsT where
  data PrimaryKey FarePolicyDriverExtraFeeBoundsT f = FarePolicyDriverExtraFeeBoundsId (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)) deriving (Generic, B.Beamable)
  primaryKey = FarePolicyDriverExtraFeeBoundsId . id

type FarePolicyDriverExtraFeeBounds = FarePolicyDriverExtraFeeBoundsT Identity

$(enableKVPG (''FarePolicyDriverExtraFeeBoundsT) [('id)] [[('farePolicyId)]])

$(mkTableInstances (''FarePolicyDriverExtraFeeBoundsT) "fare_policy_driver_extra_fee_bounds")

$(Domain.Types.UtilsTH.mkCacParseInstance (''FarePolicyDriverExtraFeeBoundsT))
