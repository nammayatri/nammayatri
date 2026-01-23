{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FarePolicy.FarePolicyInterCityDetails where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH

data FarePolicyInterCityDetailsT f = FarePolicyInterCityDetailsT
  { farePolicyId :: B.C f Text,
    baseFare :: B.C f HighPrecMoney,
    perHourCharge :: B.C f HighPrecMoney,
    perKmRateOneWay :: B.C f HighPrecMoney,
    perKmRateRoundTrip :: B.C f HighPrecMoney,
    perExtraKmRate :: B.C f HighPrecMoney,
    perExtraMinRate :: B.C f HighPrecMoney,
    kmPerPlannedExtraHour :: B.C f Kilometers,
    deadKmFare :: B.C f HighPrecMoney,
    perDayMaxHourAllowance :: B.C f Hours,
    waitingCharge :: B.C f (Maybe Domain.WaitingCharge),
    freeWatingTime :: B.C f (Maybe Minutes),
    perDayMaxAllowanceInMins :: B.C f (Maybe Minutes),
    defaultWaitTimeAtDestination :: B.C f Minutes,
    currency :: B.C f Currency,
    nightShiftCharge :: B.C f (Maybe Domain.NightShiftCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyInterCityDetailsT where
  data PrimaryKey FarePolicyInterCityDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

type FarePolicyInterCityDetails = FarePolicyInterCityDetailsT Identity

type FullFarePolicyInterCityDetails = (KTI.Id Domain.FarePolicy, Domain.FPInterCityDetails)

$(enableKVPG ''FarePolicyInterCityDetailsT ['farePolicyId] [])

$(mkTableInstances ''FarePolicyInterCityDetailsT "fare_policy_inter_city_details")

$(mkCacParseInstance ''FarePolicyInterCityDetailsT)
