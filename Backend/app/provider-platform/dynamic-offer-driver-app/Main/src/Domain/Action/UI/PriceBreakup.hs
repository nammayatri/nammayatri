module Domain.Action.UI.PriceBreakup (getPriceBreakup, getMeterRidePrice) where

import qualified API.Types.UI.DriverOnboardingV2 as DOVT
import qualified API.Types.UI.PriceBreakup
import qualified Domain.Action.UI.DriverOnboardingV2 as DOV
import qualified Domain.Action.UI.FareCalculator as FC
import Domain.Types
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude as Prelude
import Kernel.Beam.Functions as B
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.LocationUpdates.Internal as LU
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

getMeterRidePrice ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow API.Types.UI.PriceBreakup.MeterRidePriceRes
  )
getMeterRidePrice (Nothing, _, _) rideId = throwError . InternalError $ "PersonId is requried while fetching meter ride fare: " <> rideId.getId
getMeterRidePrice (Just driverId, merchantId, merchantOpCityId) rideId = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  let tripStartTime = ride.tripStartTime
  let rideStatus = ride.status
  traveledDistance <-
    if ride.status `elem` [Domain.Types.Ride.COMPLETED, Domain.Types.Ride.CANCELLED]
      then do
        pure ride.traveledDistance
      else do
        prevSnapToRoadState :: LU.SnapToRoadState <-
          Redis.safeGet (LU.onRideSnapToRoadStateKey driverId)
            <&> fromMaybe (LU.SnapToRoadState 0 (Just 0) 0 0 (Just 0) (Just False))
        pure prevSnapToRoadState.distanceTravelled
  fareEstimates <- FC.calculateFareUtil merchantId merchantOpCityId Nothing (LatLong ride.fromLocation.lat ride.fromLocation.lon) (Just $ highPrecMetersToMeters traveledDistance) Nothing Nothing (OneWay MeterRide)
  let mbMeterRideEstimate = Kernel.Prelude.listToMaybe fareEstimates.estimatedFares
  maybe
    (throwError . InternalError $ "Nahi aa rha hai fare :(" <> rideId.getId)
    ( \meterRideEstiamte -> do
        return $ API.Types.UI.PriceBreakup.MeterRidePriceRes {fare = meterRideEstiamte.minFare, distance = traveledDistance, tripStartTime = tripStartTime, status = Just rideStatus}
    )
    mbMeterRideEstimate
