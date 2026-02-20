module Domain.Action.UI.PriceBreakup (getPriceBreakup, postMeterRidePrice) where

import qualified API.Types.UI.DriverOnboardingV2 as DOVT
import Control.Lens ((^?))
import qualified API.Types.UI.PriceBreakup
import qualified Data.List.NonEmpty as NE
import qualified Domain.Action.Internal.BulkLocUpdate as BLoc
import qualified Domain.Action.UI.DriverOnboardingV2 as DOV
import qualified Domain.Action.UI.FareCalculator as FC
import Domain.Types
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id, (^?))
import qualified EulerHS.Prelude as Prelude
import Kernel.Beam.Functions as B
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.Prelude
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
      let fareDetails_ = catMaybes $ maybe [] (mkFarePolicyBreakups Prelude.id (mkBreakupItem booking.currency) booking.estimatedDistance booking.fareParams.customerCancellationDues Nothing booking.estimatedFare quote'.fareParams.congestionChargeViaDp) quote'.farePolicy
      pure fareDetails_
    _ -> pure []
  where
    mkBreakupItem :: Currency -> Text -> Text -> Maybe DOVT.RateCardItem
    mkBreakupItem currency title valueInText = do
      priceObject <- DOV.stringToPrice currency valueInText
      return $
        DOVT.RateCardItem
          { title,
            price = priceObject.amountInt,
            priceWithCurrency = mkPriceAPIEntity priceObject
          }

postMeterRidePrice ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    API.Types.UI.PriceBreakup.MeterRidePriceReq ->
    Environment.Flow API.Types.UI.PriceBreakup.MeterRidePriceRes
  )
postMeterRidePrice (Nothing, _, _) rideId _ = throwError . InternalError $ "PersonId is requried while fetching meter ride fare: " <> rideId.getId
postMeterRidePrice (Just driverId, merchantId, merchantOpCityId) rideId req = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let tripStartTime = ride.tripStartTime
  let rideStatus = ride.status
  whenJust (NE.nonEmpty (fromMaybe [] req.locationUpdates)) $ \locUpdates -> do
    void $ BLoc.bulkLocUpdate (BLoc.BulkLocUpdateReq ride.id driverId locUpdates)
  traveledDistance <- LU.getTravelledDistance driverId
  fareEstimates <- FC.calculateFareUtil merchantId merchantOpCityId Nothing (LatLong ride.fromLocation.lat ride.fromLocation.lon) (Just $ highPrecMetersToMeters traveledDistance) Nothing Nothing (OneWay MeterRide) (Just booking.vehicleServiceTier) booking.configInExperimentVersions
  let mbMeterRideEstimate = listToMaybe fareEstimates.estimatedFares
  maybe
    (throwError . InternalError $ "Nahi aa rha hai fare :(" <> rideId.getId)
    ( \meterRideEstiamte -> do
        return $ API.Types.UI.PriceBreakup.MeterRidePriceRes {fare = meterRideEstiamte.minFare, distance = traveledDistance, tripStartTime = tripStartTime, status = Just rideStatus}
    )
    mbMeterRideEstimate
