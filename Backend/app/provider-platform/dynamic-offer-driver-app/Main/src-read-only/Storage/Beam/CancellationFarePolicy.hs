{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CancellationFarePolicy where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Utils.Common
import Tools.Beam.UtilsTH

data CancellationFarePolicyT f = CancellationFarePolicyT
  { currency :: B.C f Kernel.Utils.Common.Currency,
    description :: B.C f Kernel.Prelude.Text,
    freeCancellationTimeSeconds :: B.C f Kernel.Types.Common.Seconds,
    id :: B.C f Kernel.Prelude.Text,
    maxCancellationCharge :: B.C f Kernel.Types.Common.HighPrecMoney,
    maxWaitingTimeAtPickupSeconds :: B.C f Kernel.Types.Common.Seconds,
    minCancellationCharge :: B.C f Kernel.Types.Common.HighPrecMoney,
    perMetreCancellationCharge :: B.C f Kernel.Types.Common.HighPrecMoney,
    perMinuteCancellationCharge :: B.C f Kernel.Types.Common.HighPrecMoney,
    percentageOfRideFareToBeCharged :: B.C f Kernel.Types.Common.Centesimal,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CancellationFarePolicyT where
  data PrimaryKey CancellationFarePolicyT f = CancellationFarePolicyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CancellationFarePolicyId . id

type CancellationFarePolicy = CancellationFarePolicyT Identity

$(enableKVPG ''CancellationFarePolicyT ['id] [])

$(mkTableInstances ''CancellationFarePolicyT "cancellation_fare_policy")
