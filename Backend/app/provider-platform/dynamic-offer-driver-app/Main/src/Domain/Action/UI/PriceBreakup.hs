module Domain.Action.UI.PriceBreakup (getPriceBreakup) where

import qualified API.Types.UI.DriverOnboardingV2 as DOVT
import qualified Domain.Action.UI.DriverOnboardingV2 as DOV
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude as Prelude
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FarePolicy
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide

getPriceBreakup ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow [DOVT.RateCardItem]
  )
getPriceBreakup (_, _, _) rideId = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  quote <- B.runInReplica $ QQuote.findById (Id booking.quoteId)
  case quote of
    Just quote' -> do
      let fareDetails_ = catMaybes $ maybe [] (mkFarePolicyBreakups Prelude.id mkBreakupItem booking.estimatedDistance Nothing booking.estimatedFare quote'.fareParams.congestionChargeViaDp) quote'.farePolicy
      pure fareDetails_
    _ -> pure []
  where
    mkBreakupItem :: Text -> Text -> Maybe DOVT.RateCardItem
    mkBreakupItem title valueInText = do
      priceObject <- DOV.stringToPrice INR valueInText
      return $
        DOVT.RateCardItem
          { title,
            price = priceObject.amountInt,
            priceWithCurrency = mkPriceAPIEntity priceObject
          }
