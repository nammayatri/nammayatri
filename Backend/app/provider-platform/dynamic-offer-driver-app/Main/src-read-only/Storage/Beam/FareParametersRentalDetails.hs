{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareParametersRentalDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FareParametersRentalDetailsT f = FareParametersRentalDetailsT
  { currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    deadKmFare :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    distBasedFare :: (B.C f Kernel.Types.Common.Money),
    distBasedFareAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    extraDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
    extraDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    fareParametersId :: (B.C f Kernel.Prelude.Text),
    timeBasedFare :: (B.C f Kernel.Types.Common.Money),
    timeBasedFareAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney))
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersRentalDetailsT where
  data PrimaryKey FareParametersRentalDetailsT f = FareParametersRentalDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareParametersRentalDetailsId . fareParametersId

type FareParametersRentalDetails = FareParametersRentalDetailsT Identity

$(enableKVPG (''FareParametersRentalDetailsT) [('fareParametersId)] [])

$(mkTableInstances (''FareParametersRentalDetailsT) "fare_parameters_rental_details")
