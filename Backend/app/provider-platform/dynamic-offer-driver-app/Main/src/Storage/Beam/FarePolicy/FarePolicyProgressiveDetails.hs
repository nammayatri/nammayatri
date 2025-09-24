{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FarePolicy.FarePolicyProgressiveDetails where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data FarePolicyProgressiveDetailsT f = FarePolicyProgressiveDetailsT
  { farePolicyId :: B.C f Text,
    baseDistance :: B.C f Meters,
    distanceUnit :: B.C f (Maybe DistanceUnit),
    baseFare :: B.C f Money,
    deadKmFare :: B.C f Money,
    pickupChargesMin :: B.C f (Maybe Money),
    pickupChargesMax :: B.C f (Maybe Money),
    pickupChargesMinAmount :: B.C f (Maybe HighPrecMoney),
    pickupChargesMaxAmount :: B.C f (Maybe HighPrecMoney),
    baseFareAmount :: B.C f (Maybe HighPrecMoney),
    deadKmFareAmount :: B.C f (Maybe HighPrecMoney),
    currency :: B.C f (Maybe Currency),
    waitingCharge :: B.C f (Maybe Domain.WaitingCharge),
    freeWatingTime :: B.C f (Maybe Minutes), -- FIXME typo
    nightShiftCharge :: B.C f (Maybe Domain.NightShiftCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyProgressiveDetailsT where
  data PrimaryKey FarePolicyProgressiveDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

type FarePolicyProgressiveDetails = FarePolicyProgressiveDetailsT Identity

$(enableKVPG ''FarePolicyProgressiveDetailsT ['farePolicyId] [])

$(mkTableInstances ''FarePolicyProgressiveDetailsT "fare_policy_progressive_details")

$(mkCacParseInstance ''FarePolicyProgressiveDetailsT)
