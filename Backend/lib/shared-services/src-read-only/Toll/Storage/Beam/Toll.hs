{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Toll.Storage.Beam.Toll where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Toll.Domain.Types.TollGate

data TollT f = TollT
  { id :: (B.C f Kernel.Prelude.Text),
    isAutoRickshawAllowed :: (B.C f Kernel.Prelude.Bool),
    isAutoRickshawTollChargeApplicable :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isTwoWheelerAllowed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isTwoWheelerTollChargeApplicable :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    name :: (B.C f Kernel.Prelude.Text),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    price :: (B.C f Kernel.Types.Common.HighPrecMoney),
    tollEndGates :: (B.C f [Toll.Domain.Types.TollGate.TollGate]),
    tollStartGates :: (B.C f [Toll.Domain.Types.TollGate.TollGate]),
    merchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TollT where
  data PrimaryKey TollT f = TollId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TollId . id

type Toll = TollT Identity

$(enableKVPG (''TollT) [('id)] [])

$(mkTableInstancesGenericSchema (''TollT) "toll")
