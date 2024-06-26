module Domain.Action.UI.AmbulanceDetails where

import Domain.Types.AmbulanceDetails
import Kernel.Prelude
import Kernel.Types.Common

mkAmbulanceDetailsAPIEntity :: AmbulanceDetails -> Maybe PriceAPIEntity -> AmbulanceDetailsAPIEntity
mkAmbulanceDetailsAPIEntity AmbulanceDetails {..} tollCharges = do
  AmbulanceDetailsAPIEntity
    { minEstimatedFare = mkPriceAPIEntity minEstimatedFare,
      maxEstimatedFare = mkPriceAPIEntity maxEstimatedFare,
      ambulanceQuoteBreakupList = mkAmbulanceQuoteBreakupAPIEntity <$> ambulanceQuoteBreakupList,
      ..
    }
  where
    mkAmbulanceQuoteBreakupAPIEntity ambulanceQuoteBreakup = do
      AmbulanceQuoteBreakupAPIEntity
        { price = PriceAPIEntity ambulanceQuoteBreakup.price.value.amount ambulanceQuoteBreakup.price.value.currency,
          title = ambulanceQuoteBreakup.title
        }
