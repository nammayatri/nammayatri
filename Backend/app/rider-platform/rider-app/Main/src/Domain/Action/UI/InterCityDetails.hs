module Domain.Action.UI.InterCityDetails where

import Domain.Types.InterCityDetails
import Domain.Types.RentalDetails
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

mkInterCityDetailsAPIEntity :: InterCityDetails -> Maybe PriceAPIEntity -> InterCityDetailsAPIEntity
mkInterCityDetailsAPIEntity InterCityDetails {..} tollCharges = do
  InterCityDetailsAPIEntity
    { quoteId = getId id,
      baseFare = mkPriceAPIEntity baseFare,
      perHourCharge = mkPriceAPIEntity perHourCharge,
      perExtraMinRate = mkPriceAPIEntity perExtraMinRate,
      perExtraKmRate = mkPriceAPIEntity perExtraKmRate,
      deadKmFare = mkPriceAPIEntity deadKmFare,
      plannedPerKmRateOneWay = mkPriceAPIEntity plannedPerKmRateOneWay,
      plannedPerKmRateRoundTrip = mkPriceAPIEntity plannedPerKmRateRoundTrip,
      nightShiftInfo = mkNightShiftInfoAPIEntity <$> nightShiftInfo,
      ..
    }
