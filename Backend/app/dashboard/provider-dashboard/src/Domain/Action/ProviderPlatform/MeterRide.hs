module Domain.Action.ProviderPlatform.MeterRide where

import qualified "dynamic-offer-driver-app" API.Types.UI.DriverOnboardingV2
import qualified "dynamic-offer-driver-app" API.Types.UI.DriverOnboardingV2 as DOVT
import qualified "dynamic-offer-driver-app" API.Types.UI.FareCalculator
import qualified "dynamic-offer-driver-app" API.Types.UI.PriceBreakup
import qualified Data.Geohash as Geohash
import qualified Data.Text as T
import qualified "dynamic-offer-driver-app" Domain.Action.Beckn.Search as DBS
import qualified "dynamic-offer-driver-app" Domain.Action.UI.DriverOnboardingV2 as DOV
import qualified "dynamic-offer-driver-app" Domain.Action.UI.FareCalculator as FC
import Domain.Types
import qualified "dynamic-offer-driver-app" Domain.Types.FarePolicy as DFP
import qualified "dynamic-offer-driver-app" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.MerchantOperatingCity
import qualified "dynamic-offer-driver-app" Domain.Types.Ride
import "lib-dashboard" Environment
import EulerHS.Prelude
import qualified EulerHS.Prelude as Prelude
import Kernel.Beam.Functions as B
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Maps.Utils as Utils
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.LocationUpdates.Internal as LU
import "dynamic-offer-driver-app" SharedLogic.FarePolicy
import qualified "dynamic-offer-driver-app" SharedLogic.TollsDetector as TD
import qualified "dynamic-offer-driver-app" Storage.Cac.TransporterConfig as CCT
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified "dynamic-offer-driver-app" Storage.Queries.Booking as QRB
import qualified "dynamic-offer-driver-app" Storage.Queries.Quote as QQuote
import qualified "dynamic-offer-driver-app" Storage.Queries.Ride as QRide
import "dynamic-offer-driver-app" Tools.Error

getPriceBreakup :: Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.Flow [DOVT.RateCardItem]
getPriceBreakup rideId = do
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

getMeterRidePrice :: Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.Flow API.Types.UI.PriceBreakup.MeterRidePriceRes
getMeterRidePrice rideId = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  let driverId = ride.driverId
  merchantId <- fromMaybeM (RideNotFound rideId.getId) ride.merchantId
  let merchantOpCityId = ride.merchantOperatingCityId
  traveledDistance <-
    if ride.status `elem` [Domain.Types.Ride.COMPLETED, Domain.Types.Ride.CANCELLED]
      then do
        pure ride.traveledDistance
      else do
        prevSnapToRoadState :: LU.SnapToRoadState <-
          Redis.safeGet (LU.onRideSnapToRoadStateKey driverId)
            <&> fromMaybe (LU.SnapToRoadState 0 (Just 0) 0 0 (Just 0) (Just False))
        pure prevSnapToRoadState.distanceTravelled
  fareEstimates <- calculateFareUtil merchantId merchantOpCityId Nothing (LatLong ride.fromLocation.lat ride.fromLocation.lon) (Just $ highPrecMetersToMeters traveledDistance) Nothing Nothing (OneWay MeterRide)
  let mbMeterRideEstimate = Kernel.Prelude.listToMaybe fareEstimates.estimatedFares
  maybe
    (throwError . InternalError $ "No fare bro" <> rideId.getId)
    ( \meterRideEstiamte -> do
        return $ API.Types.UI.PriceBreakup.MeterRidePriceRes {fare = meterRideEstiamte.minFare, distance = traveledDistance}
    )
    mbMeterRideEstimate

calculateFareUtil ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Maybe Kernel.External.Maps.Types.LatLong ->
  Kernel.External.Maps.Types.LatLong ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Utils.RouteInfo ->
  TripCategory ->
  Environment.Flow API.Types.UI.FareCalculator.FareResponse
calculateFareUtil merchantId merchanOperatingCityId mbDropLatLong pickupLatlong mbDistance mbDuration mbRoute tripCategory = do
  now <- getCurrentTime
  transporterConfig <- CCT.findByMerchantOpCityId merchanOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchanOperatingCityId.getId)
  let mbFromLocGeohash = T.pack <$> Geohash.encode (fromMaybe 5 transporterConfig.dpGeoHashPercision) (pickupLatlong.lat, pickupLatlong.lon)
  let mbToLocGeohash = T.pack <$> ((\dropLatLong -> Geohash.encode (fromMaybe 5 transporterConfig.dpGeoHashPercision) (dropLatLong.lat, dropLatLong.lon)) =<< mbDropLatLong)
  fareProducts <- getAllFarePoliciesProduct merchantId merchanOperatingCityId False pickupLatlong mbDropLatLong Nothing mbFromLocGeohash mbToLocGeohash mbDistance mbDuration Nothing tripCategory
  mbTollChargesAndNames <- TD.getTollInfoOnRoute merchanOperatingCityId Nothing (maybe [] (\x -> x.points) mbRoute)
  let mbTollCharges = (\(tollCharges, _, _, _) -> tollCharges) <$> mbTollChargesAndNames
  let mbTollNames = (\(_, tollNames, _, _) -> tollNames) <$> mbTollChargesAndNames
  let mbIsAutoRickshawAllowed = (\(_, _, mbIsAutoRickshawAllowed', _) -> mbIsAutoRickshawAllowed') <$> mbTollChargesAndNames
  let mbIsTwoWheelerAllowed = join ((\(_, _, _, isTwoWheelerAllowed) -> isTwoWheelerAllowed) <$> mbTollChargesAndNames)
  let allFarePolicies = selectFarePolicy (fromMaybe 0 mbDistance) (fromMaybe 0 mbDuration) mbIsAutoRickshawAllowed mbIsTwoWheelerAllowed fareProducts.farePolicies
  estimates <- mapM (\fp -> buildEstimateHelper fp mbTollCharges mbTollNames now) allFarePolicies
  let estimateAPIEntity = map FC.buildEstimateApiEntity estimates
  return API.Types.UI.FareCalculator.FareResponse {estimatedFares = estimateAPIEntity}
  where
    buildEstimateHelper fp mbTollCharges mbTollNames now = do
      vehicleServiceTierItem <-
        CQVST.findByServiceTierTypeAndCityId fp.vehicleServiceTier merchanOperatingCityId
          >>= fromMaybeM (VehicleServiceTierNotFound $ show fp.vehicleServiceTier)
      DBS.buildEstimate merchantId merchanOperatingCityId INR Meter Nothing now False Nothing False mbDistance Nothing mbTollCharges mbTollNames Nothing Nothing 0 Nothing False vehicleServiceTierItem fp

    selectFarePolicy distance' duration' mbIsAutoRickshawAllowed' mbIsTwoWheelerAllowed' =
      filter isValid
      where
        isValid farePolicy = checkDistanceBounds farePolicy && checkExtendUpto farePolicy && (autosAllowedOnTollRoute farePolicy || bikesAllowedOnTollRoute farePolicy)
        autosAllowedOnTollRoute farePolicy = if farePolicy.vehicleServiceTier == AUTO_RICKSHAW then fromMaybe True mbIsAutoRickshawAllowed' else True
        bikesAllowedOnTollRoute farePolicy = if farePolicy.vehicleServiceTier == BIKE then fromMaybe True mbIsTwoWheelerAllowed' else True
        checkDistanceBounds farePolicy = maybe True checkBounds farePolicy.allowedTripDistanceBounds
        checkBounds bounds = bounds.minAllowedTripDistance <= distance' && distance' <= bounds.maxAllowedTripDistance
        checkExtendUpto farePolicy = case farePolicy.farePolicyDetails of
          DFP.RentalDetails det -> checkLimits det
          _ -> True
          where
            checkLimits det =
              let distInKm = distance'.getMeters `div` 1000
                  timeInHr = duration'.getSeconds `div` 3600
                  includedKm = (timeInHr * det.includedKmPerHr.getKilometers)
                  maxAllowed = min (min det.maxAdditionalKmsLimit.getKilometers includedKm) (det.totalAdditionalKmsLimit.getKilometers - includedKm)
               in distInKm - includedKm <= maxAllowed
