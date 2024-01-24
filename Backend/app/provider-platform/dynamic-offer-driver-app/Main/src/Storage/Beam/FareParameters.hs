{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.FareParameters where

import qualified Database.Beam as B
import qualified Domain.Types.FareParameters as Domain
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
    nightShiftRateIfApplies :: B.C f (Maybe Double),
    serviceCharge :: B.C f (Maybe Money),
    fareParametersType :: B.C f Domain.FareParametersType,
    govtCharges :: B.C f (Maybe Money),
    customerCancellationDues :: B.C f (Maybe HighPrecMoney),
    updatedAt :: B.C f UTCTime
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
