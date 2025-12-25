{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FareParameters where

import qualified Database.Beam as B
import qualified Domain.Types.FareParameters as Domain
import qualified Domain.Types.FarePolicy as FP
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data FareParametersT f = FareParametersT
  { id :: B.C f Text,
    baseFare :: B.C f Money,
    driverSelectedFare :: B.C f (Maybe Money),
    customerExtraFee :: B.C f (Maybe Money),
    waitingCharge :: B.C f (Maybe Money),
    rideExtraTimeFare :: B.C f (Maybe Money),
    nightShiftCharge :: B.C f (Maybe Money),
    baseFareAmount :: B.C f (Maybe HighPrecMoney),
    petCharges :: B.C f (Maybe HighPrecMoney),
    businessDiscount :: B.C f (Maybe HighPrecMoney),
    shouldApplyBusinessDiscount :: B.C f (Maybe Bool),
    priorityCharges :: B.C f (Maybe HighPrecMoney),
    driverSelectedFareAmount :: B.C f (Maybe HighPrecMoney),
    customerExtraFeeAmount :: B.C f (Maybe HighPrecMoney),
    waitingChargeAmount :: B.C f (Maybe HighPrecMoney),
    rideExtraTimeFareAmount :: B.C f (Maybe HighPrecMoney),
    nightShiftChargeAmount :: B.C f (Maybe HighPrecMoney),
    nightShiftRateIfApplies :: B.C f (Maybe Double),
    serviceCharge :: B.C f (Maybe Money),
    stopCharges :: B.C f (Maybe HighPrecMoney),
    serviceChargeAmount :: B.C f (Maybe HighPrecMoney),
    parkingCharge :: B.C f (Maybe HighPrecMoney),
    fareParametersType :: B.C f Domain.FareParametersType,
    govtCharges :: B.C f (Maybe Money),
    govtChargesAmount :: B.C f (Maybe HighPrecMoney),
    customerCancellationDues :: B.C f (Maybe HighPrecMoney),
    tollCharges :: B.C f (Maybe HighPrecMoney),
    congestionCharge :: B.C f (Maybe Money),
    congestionChargeViaDp :: B.C f (Maybe HighPrecMoney),
    congestionChargeAmount :: B.C f (Maybe HighPrecMoney),
    insuranceCharge :: B.C f (Maybe HighPrecMoney),
    cardChargeOnFare :: B.C f (Maybe HighPrecMoney),
    fixedCardCharge :: B.C f (Maybe HighPrecMoney),
    luggageCharge :: B.C f (Maybe HighPrecMoney),
    returnFeeCharge :: B.C f (Maybe HighPrecMoney),
    boothCharge :: B.C f (Maybe HighPrecMoney),
    platformFee :: B.C f (Maybe HighPrecMoney),
    sgst :: B.C f (Maybe HighPrecMoney),
    cgst :: B.C f (Maybe HighPrecMoney),
    platformFeeChargesBy :: B.C f (Maybe FP.PlatformFeeMethods),
    conditionalCharges :: B.C f (Maybe Value),
    currency :: B.C f (Maybe Currency),
    driverCancellationPenaltyAmount :: B.C f (Maybe HighPrecMoney),
    updatedAt :: B.C f (Maybe UTCTime),
    merchantId :: B.C f (Maybe Text),
    merchantOperatingCityId :: B.C f (Maybe Text),
    paymentProcessingFee :: B.C f (Maybe HighPrecMoney),
    rideVat :: B.C f (Maybe HighPrecMoney),
    tollVat :: B.C f (Maybe HighPrecMoney),
    commission :: B.C f (Maybe HighPrecMoney)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersT where
  data PrimaryKey FareParametersT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FareParameters = FareParametersT Identity

$(enableKVPG ''FareParametersT ['id] [])

$(mkTableInstances ''FareParametersT "fare_parameters")
