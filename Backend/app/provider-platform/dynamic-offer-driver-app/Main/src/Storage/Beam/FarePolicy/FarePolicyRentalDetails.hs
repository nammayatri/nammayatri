{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FarePolicy.FarePolicyRentalDetails where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH

data FarePolicyRentalDetailsT f = FarePolicyRentalDetailsT
  { farePolicyId :: B.C f Text,
    baseFare :: B.C f Money,
    perHourCharge :: B.C f Money,
    perExtraMinRate :: B.C f Money,
    perExtraKmRate :: B.C f Money,
    baseFareAmount :: B.C f (Maybe HighPrecMoney),
    perHourChargeAmount :: B.C f (Maybe HighPrecMoney),
    perExtraMinRateAmount :: B.C f (Maybe HighPrecMoney),
    perExtraKmRateAmount :: B.C f (Maybe HighPrecMoney),
    nightShiftCharge :: B.C f (Maybe Domain.NightShiftCharge),
    deadKmFare :: B.C f HighPrecMoney,
    includedKmPerHr :: B.C f Kilometers,
    plannedPerKmRate :: B.C f Money,
    plannedPerKmRateAmount :: B.C f (Maybe HighPrecMoney),
    currency :: B.C f (Maybe Currency),
    maxAdditionalKmsLimit :: B.C f Kilometers,
    totalAdditionalKmsLimit :: B.C f Kilometers,
    freeWaitingTime :: B.C f (Maybe Minutes),
    waitingCharge :: B.C f (Maybe Domain.WaitingCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyRentalDetailsT where
  data PrimaryKey FarePolicyRentalDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

type FarePolicyRentalDetails = FarePolicyRentalDetailsT Identity

type FullFarePolicyRentalDetails = (KTI.Id Domain.FarePolicy, Domain.FPRentalDetails)

$(enableKVPG ''FarePolicyRentalDetailsT ['farePolicyId] [])

$(mkTableInstances ''FarePolicyRentalDetailsT "fare_policy_rental_details")

$(mkCacParseInstance ''FarePolicyRentalDetailsT)
