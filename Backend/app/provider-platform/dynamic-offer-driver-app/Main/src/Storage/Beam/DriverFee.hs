{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.DriverFee where

import qualified Database.Beam as B
import qualified Domain.Types.DriverFee as Domain
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data DriverFeeT f = DriverFeeT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    driverId :: B.C f Text,
    totalEarnings :: B.C f Money,
    govtCharges :: B.C f Money,
    platformFee :: B.C f HighPrecMoney,
    cgst :: B.C f HighPrecMoney,
    sgst :: B.C f HighPrecMoney,
    payBy :: B.C f UTCTime,
    startTime :: B.C f UTCTime,
    endTime :: B.C f UTCTime,
    numRides :: B.C f Int,
    status :: B.C f Domain.DriverFeeStatus,
    collectedBy :: B.C f (Maybe Text),
    collectedAt :: B.C f (Maybe UTCTime),
    feeType :: B.C f Domain.FeeType,
    offerId :: B.C f (Maybe Text),
    planOfferTitle :: B.C f (Maybe Text),
    stageUpdatedAt :: B.C f (Maybe UTCTime),
    billNumber :: B.C f (Maybe Int),
    autopayPaymentStage :: B.C f (Maybe Domain.AutopayPaymentStage),
    schedulerTryCount :: B.C f Int,
    feeWithoutDiscount :: B.C f (Maybe HighPrecMoney),
    amountPaidByCoin :: B.C f (Maybe HighPrecMoney),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    overlaySent :: B.C f Bool,
    specialZoneRideCount :: B.C f Int,
    specialZoneAmount :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverFeeT where
  data PrimaryKey DriverFeeT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverFee = DriverFeeT Identity

$(enableKVPG ''DriverFeeT ['id] [['driverId]])

$(mkTableInstances ''DriverFeeT "driver_fee")
