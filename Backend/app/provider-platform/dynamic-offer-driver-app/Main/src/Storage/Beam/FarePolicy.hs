{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FarePolicy where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data FarePolicyT f = FarePolicyT
  { id :: B.C f Text,
    cancellationFarePolicyId :: B.C f (Maybe Text),
    farePolicyType :: B.C f Domain.FarePolicyType,
    serviceCharge :: B.C f (Maybe Money),
    serviceChargeAmount :: B.C f (Maybe HighPrecMoney),
    parkingCharge :: B.C f (Maybe HighPrecMoney),
    perStopCharge :: B.C f (Maybe HighPrecMoney),
    petCharges :: B.C f (Maybe HighPrecMoney),
    priorityCharges :: B.C f (Maybe HighPrecMoney),
    tollCharges :: B.C f (Maybe HighPrecMoney),
    pickupBufferInSecsForNightShiftCal :: B.C f (Maybe Seconds),
    tipOptions :: B.C f (Maybe [Int]),
    currency :: B.C f (Maybe Currency),
    nightShiftStart :: B.C f (Maybe TimeOfDay),
    nightShiftEnd :: B.C f (Maybe TimeOfDay),
    maxAllowedTripDistance :: B.C f (Maybe Meters),
    minAllowedTripDistance :: B.C f (Maybe Meters),
    distanceUnit :: B.C f (Maybe DistanceUnit),
    govtCharges :: B.C f (Maybe Double),
    perMinuteRideExtraTimeCharge :: B.C f (Maybe HighPrecMoney),
    congestionCharge :: B.C f (Maybe Domain.CongestionChargeMultiplier),
    perDistanceUnitInsuranceCharge :: B.C f (Maybe HighPrecMoney),
    cardChargePerDistanceUnitMultiplier :: B.C f (Maybe Double),
    platformFee :: B.C f (Maybe HighPrecMoney),
    sgst :: B.C f (Maybe HighPrecMoney),
    cgst :: B.C f (Maybe HighPrecMoney),
    platformFeeChargesBy :: B.C f (Maybe Domain.PlatformFeeMethods),
    fixedCardCharge :: B.C f (Maybe HighPrecMoney),
    description :: B.C f (Maybe Text),
    driverCancellationPenaltyAmount :: B.C f (Maybe HighPrecMoney),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    merchantId :: B.C f (Maybe Text),
    merchantOperatingCityId :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyT where
  data PrimaryKey FarePolicyT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FarePolicy = FarePolicyT Identity

$(enableKVPG ''FarePolicyT ['id] [[]])

$(mkTableInstances ''FarePolicyT "fare_policy")

$(mkCacParseInstance ''FarePolicyT)
