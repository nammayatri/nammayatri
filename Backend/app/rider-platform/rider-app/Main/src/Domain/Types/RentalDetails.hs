{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.RentalDetails where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.JSON (removeNullFields)

data RentalDetails = RentalDetails
  { id :: Id RentalDetails,
    baseFare :: Price,
    perHourCharge :: Price,
    perExtraMinRate :: Price,
    perExtraKmRate :: Price,
    includedDistancePerHr :: Distance, -- Kilometers,
    plannedPerKmRate :: Price,
    nightShiftInfo :: Maybe NightShiftInfo
  }
  deriving (Generic, Show)

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Price,
    oldNightShiftCharge :: Maybe Centesimal,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show)

data NightShiftInfoAPIEntity = NightShiftInfoAPIEntity
  { nightShiftCharge :: Money,
    nightShiftChargeWithCurrency :: PriceAPIEntity,
    oldNightShiftCharge :: Maybe Centesimal,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkNightShiftInfoAPIEntity :: NightShiftInfo -> NightShiftInfoAPIEntity
mkNightShiftInfoAPIEntity NightShiftInfo {..} =
  NightShiftInfoAPIEntity
    { nightShiftCharge = nightShiftCharge.amountInt,
      nightShiftChargeWithCurrency = mkPriceAPIEntity nightShiftCharge,
      ..
    }

data RentalDetailsAPIEntity = RentalDetailsAPIEntity
  { baseFare :: Money,
    perHourCharge :: Money,
    perExtraMinRate :: Money,
    baseFareWithCurrency :: PriceAPIEntity,
    perHourChargeWithCurrency :: PriceAPIEntity,
    perExtraMinRateWithCurrency :: PriceAPIEntity,
    includedKmPerHr :: Kilometers,
    includedDistancePerHrWithUnit :: Distance,
    plannedPerKmRate :: Money,
    perExtraKmRate :: Money,
    plannedPerKmRateWithCurrency :: PriceAPIEntity,
    perExtraKmRateWithCurrency :: PriceAPIEntity,
    nightShiftInfo :: Maybe NightShiftInfoAPIEntity
  }
  deriving (Generic, FromJSON, Show, ToSchema)

instance ToJSON RentalDetailsAPIEntity where
  toJSON = genericToJSON removeNullFields

mkRentalDetailsAPIEntity :: RentalDetails -> RentalDetailsAPIEntity
mkRentalDetailsAPIEntity RentalDetails {..} = do
  RentalDetailsAPIEntity
    { baseFare = baseFare.amountInt,
      perHourCharge = perHourCharge.amountInt,
      perExtraMinRate = perExtraMinRate.amountInt,
      baseFareWithCurrency = mkPriceAPIEntity baseFare,
      perHourChargeWithCurrency = mkPriceAPIEntity perHourCharge,
      perExtraMinRateWithCurrency = mkPriceAPIEntity perExtraMinRate,
      plannedPerKmRate = plannedPerKmRate.amountInt,
      perExtraKmRate = perExtraKmRate.amountInt,
      plannedPerKmRateWithCurrency = mkPriceAPIEntity plannedPerKmRate,
      perExtraKmRateWithCurrency = mkPriceAPIEntity perExtraKmRate,
      nightShiftInfo = mkNightShiftInfoAPIEntity <$> nightShiftInfo,
      includedKmPerHr = metersToKilometers $ distanceToMeters includedDistancePerHr,
      includedDistancePerHrWithUnit = includedDistancePerHr,
      ..
    }
