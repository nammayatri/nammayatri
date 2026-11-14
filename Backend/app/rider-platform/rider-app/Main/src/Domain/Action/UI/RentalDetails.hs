module Domain.Action.UI.RentalDetails where

import Domain.Types.RentalDetails
import Kernel.Prelude
import Kernel.Types.Common

mkRentalDetailsAPIEntity :: (Price -> Price) -> RentalDetails -> Maybe PriceAPIEntity -> RentalDetailsAPIEntity
mkRentalDetailsAPIEntity grossRate RentalDetails {..} tollCharges = do
  RentalDetailsAPIEntity
    { baseFare = (grossRate baseFare).amountInt,
      perHourCharge = (grossRate perHourCharge).amountInt,
      perExtraMinRate = (grossRate perExtraMinRate).amountInt,
      baseFareWithCurrency = mkPriceAPIEntity (grossRate baseFare),
      perHourChargeWithCurrency = mkPriceAPIEntity (grossRate perHourCharge),
      perExtraMinRateWithCurrency = mkPriceAPIEntity (grossRate perExtraMinRate),
      plannedPerKmRate = (grossRate plannedPerKmRate).amountInt,
      perExtraKmRate = (grossRate perExtraKmRate).amountInt,
      plannedPerKmRateWithCurrency = mkPriceAPIEntity (grossRate plannedPerKmRate),
      perExtraKmRateWithCurrency = mkPriceAPIEntity (grossRate perExtraKmRate),
      nightShiftInfo = mkNightShiftInfoAPIEntity <$> nightShiftInfo,
      includedKmPerHr = metersToKilometers $ distanceToMeters includedDistancePerHr,
      includedDistancePerHrWithUnit = includedDistancePerHr,
      deadKmFare = mkPriceAPIEntity (grossRate deadKmFare),
      ..
    }
