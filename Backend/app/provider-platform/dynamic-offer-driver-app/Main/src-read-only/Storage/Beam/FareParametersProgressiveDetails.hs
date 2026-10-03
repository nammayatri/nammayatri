{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareParametersProgressiveDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FareParametersProgressiveDetailsT f = FareParametersProgressiveDetailsT
  { currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    deadKmFare :: (B.C f Kernel.Types.Common.Money),
    deadKmFareAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    extraKmFare :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    extraKmFareAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    fareParametersId :: (B.C f Kernel.Prelude.Text),
    rideDurationFare :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney))
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersProgressiveDetailsT where
  data PrimaryKey FareParametersProgressiveDetailsT f = FareParametersProgressiveDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareParametersProgressiveDetailsId . fareParametersId

type FareParametersProgressiveDetails = FareParametersProgressiveDetailsT Identity

$(enableKVPG (''FareParametersProgressiveDetailsT) [('fareParametersId)] [])

$(mkTableInstances (''FareParametersProgressiveDetailsT) "fare_parameters_progressive_details")
