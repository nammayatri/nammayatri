{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareParametersInterCityDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FareParametersInterCityDetailsT f = FareParametersInterCityDetailsT
  { currency :: (B.C f Kernel.Types.Common.Currency),
    distanceFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    extraDistanceFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    extraTimeFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    fareParametersId :: (B.C f Kernel.Prelude.Text),
    pickupCharge :: (B.C f Kernel.Types.Common.HighPrecMoney),
    stateEntryPermitCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    timeFare :: (B.C f Kernel.Types.Common.HighPrecMoney)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersInterCityDetailsT where
  data PrimaryKey FareParametersInterCityDetailsT f = FareParametersInterCityDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareParametersInterCityDetailsId . fareParametersId

type FareParametersInterCityDetails = FareParametersInterCityDetailsT Identity

$(enableKVPG (''FareParametersInterCityDetailsT) [('fareParametersId)] [])

$(mkTableInstances (''FareParametersInterCityDetailsT) "fare_parameters_inter_city_details")
