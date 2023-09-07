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

module Storage.Beam.Estimate where

import qualified Database.Beam as B
import qualified Domain.Types.Estimate as Domain
import qualified Domain.Types.VehicleVariant as VehVar
import Tools.Beam.UtilsTH
import Kernel.External.Maps hiding (status)
import Kernel.Prelude
import Kernel.Types.Common hiding (id)

data EstimateT f = EstimateT
  { id :: B.C f Text,
    requestId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    bppEstimateId :: B.C f Text,
    itemId :: B.C f Text,
    estimatedFare :: B.C f HighPrecMoney,
    discount :: B.C f (Maybe HighPrecMoney),
    estimatedTotalFare :: B.C f HighPrecMoney,
    minTotalFare :: B.C f HighPrecMoney,
    maxTotalFare :: B.C f HighPrecMoney,
    estimatedDuration :: B.C f (Maybe Seconds),
    estimatedDistance :: B.C f (Maybe HighPrecMeters),
    device :: B.C f (Maybe Text),
    providerId :: B.C f Text,
    providerUrl :: B.C f Text,
    providerName :: B.C f Text,
    providerMobileNumber :: B.C f Text,
    providerCompletedRidesCount :: B.C f Int,
    vehicleVariant :: B.C f VehVar.VehicleVariant,
    driversLocation :: B.C f [LatLong],
    tripTermsId :: B.C f (Maybe Text),
    nightShiftCharge :: B.C f (Maybe Money),
    oldNightShiftCharge :: B.C f (Maybe Centesimal),
    nightShiftStart :: B.C f (Maybe TimeOfDay),
    nightShiftEnd :: B.C f (Maybe TimeOfDay),
    status :: B.C f Domain.EstimateStatus,
    waitingChargePerMin :: B.C f (Maybe Money),
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateT where
  data PrimaryKey EstimateT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Estimate = EstimateT Identity

$(enableKVPG ''EstimateT ['id] [['requestId], ['bppEstimateId]])

$(mkTableInstancesWithTModifier ''EstimateT "estimate" [("oldNightShiftCharge", "night_shift_multiplier")])
