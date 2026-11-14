module Domain.Action.UI.InterCityDetails where

import Domain.Types.InterCityDetails
import Domain.Types.RentalDetails
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

mkInterCityDetailsAPIEntity :: (Price -> Price) -> InterCityDetails -> Maybe PriceAPIEntity -> InterCityDetailsAPIEntity
mkInterCityDetailsAPIEntity grossRate InterCityDetails {..} tollCharges = do
  InterCityDetailsAPIEntity
    { quoteId = getId id,
      baseFare = mkPriceAPIEntity (grossRate baseFare),
      perHourCharge = mkPriceAPIEntity (grossRate perHourCharge),
      perExtraMinRate = mkPriceAPIEntity (grossRate perExtraMinRate),
      perExtraKmRate = mkPriceAPIEntity (grossRate perExtraKmRate),
      deadKmFare = mkPriceAPIEntity (grossRate deadKmFare),
      plannedPerKmRateOneWay = mkPriceAPIEntity (grossRate plannedPerKmRateOneWay),
      plannedPerKmRateRoundTrip = mkPriceAPIEntity (grossRate plannedPerKmRateRoundTrip),
      nightShiftInfo = mkNightShiftInfoAPIEntity <$> nightShiftInfo,
      ..
    }
