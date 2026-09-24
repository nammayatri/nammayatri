{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicyInterCityDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FarePolicyInterCityDetailsT f = FarePolicyInterCityDetailsT
  { baseFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    currency :: (B.C f Kernel.Types.Common.Currency),
    deadKmFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    defaultWaitTimeAtDestination :: (B.C f Kernel.Types.Common.Minutes),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    kmPerPlannedExtraHour :: (B.C f Kernel.Types.Common.Kilometers),
    nightShiftCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.NightShiftCharge)),
    perDayMaxAllowanceInMins :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Minutes)),
    perDayMaxHourAllowance :: (B.C f Kernel.Types.Common.Hours),
    perExtraKmRate :: (B.C f Kernel.Types.Common.HighPrecMoney),
    perExtraMinRate :: (B.C f Kernel.Types.Common.HighPrecMoney),
    perHourCharge :: (B.C f Kernel.Types.Common.HighPrecMoney),
    perKmRateOneWay :: (B.C f Kernel.Types.Common.HighPrecMoney),
    perKmRateRoundTrip :: (B.C f Kernel.Types.Common.HighPrecMoney),
    stateEntryPermitCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    freeWatingTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Minutes)),
    waitingCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.WaitingCharge))
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyInterCityDetailsT where
  data PrimaryKey FarePolicyInterCityDetailsT f = FarePolicyInterCityDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FarePolicyInterCityDetailsId . farePolicyId

type FarePolicyInterCityDetails = FarePolicyInterCityDetailsT Identity

$(enableKVPG (''FarePolicyInterCityDetailsT) [('farePolicyId)] [])

$(mkTableInstances (''FarePolicyInterCityDetailsT) "fare_policy_inter_city_details")

$(Domain.Types.UtilsTH.mkCacParseInstance (''FarePolicyInterCityDetailsT))
