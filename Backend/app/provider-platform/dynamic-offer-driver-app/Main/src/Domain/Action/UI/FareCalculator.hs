module Domain.Action.UI.FareCalculator where

import qualified API.Types.UI.FareCalculator
import Control.Monad.Extra (mapMaybeM)
import qualified Data.Geohash as Geohash
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Search as DBS
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.Estimate
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id, map)
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Maps.Utils as Utils
import Kernel.Prelude hiding (concatMap)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.FarePolicy as FP
import qualified SharedLogic.TollsDetector as TD
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMM
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Merchant as QM
import Tools.Error
import qualified Tools.Maps as Maps

getCalculateFare ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Int ->
    Kernel.External.Maps.Types.LatLong ->
    Kernel.External.Maps.Types.LatLong ->
    Environment.Flow API.Types.UI.FareCalculator.FareResponse
  )
getCalculateFare (_, merchantId, merchanOperatingCityId) distanceWeightage dropLatLong pickupLatlong = do
  (_, mbDistance, mbDuration, mbRoute, _) <- calculateDistanceAndRoutes merchantId merchanOperatingCityId distanceWeightage [pickupLatlong, dropLatLong] -----------------if you want congestionCharges to be considered then pass geohash imstead of nothing
  calculateFareUtil merchantId merchanOperatingCityId (Just dropLatLong) pickupLatlong mbDistance mbDuration mbRoute (DTC.OneWay DTC.OneWayOnDemandDynamicOffer) Nothing []

data CalculateFareReq = CalculateFareReq
  { dropLatLong :: Maybe Kernel.External.Maps.Types.LatLong,
    pickupLatLong :: Kernel.External.Maps.Types.LatLong,
    mbDistance :: Maybe Meters,
    mbDuration :: Maybe Seconds,
    mbTripCategory :: Maybe DTC.TripCategory
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

calculateFare :: Id Merchant -> Context.City -> Maybe Text -> CalculateFareReq -> Environment.Flow API.Types.UI.FareCalculator.FareResponse
calculateFare merchantId merchantCity apiKey CalculateFareReq {..} = do
  let tripCategory = fromMaybe (DTC.OneWay DTC.OneWayOnDemandDynamicOffer) mbTripCategory
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  merchantOperatingCity <- CQMM.findByMerchantIdAndCity merchantId merchantCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show merchantCity)
  calculateFareUtil merchantId merchantOperatingCity.id dropLatLong pickupLatLong mbDistance mbDuration Nothing tripCategory Nothing []

calculateFareUtil ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Maybe Kernel.External.Maps.Types.LatLong ->
  Kernel.External.Maps.Types.LatLong ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Utils.RouteInfo ->
  DTC.TripCategory ->
  Maybe DVST.ServiceTierType ->
  [LYT.ConfigVersionMap] ->
  Environment.Flow API.Types.UI.FareCalculator.FareResponse
calculateFareUtil merchantId merchanOperatingCityId mbDropLatLong pickupLatlong mbDistance mbDuration mbRoute tripCategory mbVehicleServiceTier configsInExperimentVersions = do
  now <- getCurrentTime
  transporterConfig <- CCT.findByMerchantOpCityId merchanOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchanOperatingCityId.getId)
  let mbFromLocGeohash = T.pack <$> Geohash.encode (fromMaybe 5 transporterConfig.dpGeoHashPercision) (pickupLatlong.lat, pickupLatlong.lon)
  let mbToLocGeohash = T.pack <$> ((\dropLatLong -> Geohash.encode (fromMaybe 5 transporterConfig.dpGeoHashPercision) (dropLatLong.lat, dropLatLong.lon)) =<< mbDropLatLong)
  fareProducts <- FP.getAllFarePoliciesProduct merchantId merchanOperatingCityId False pickupLatlong mbDropLatLong Nothing mbFromLocGeohash mbToLocGeohash mbDistance mbDuration Nothing tripCategory configsInExperimentVersions
  mbTollChargesAndNames <- TD.getTollInfoOnRoute merchanOperatingCityId Nothing (maybe [] (\x -> x.points) mbRoute)
  let mbTollCharges = (\(tollCharges, _, _, _, _) -> tollCharges) <$> mbTollChargesAndNames
  let mbTollNames = (\(_, tollNames, _, _, _) -> tollNames) <$> mbTollChargesAndNames
  let mbTollIds = (\(_, _, tollIds, _, _) -> tollIds) <$> mbTollChargesAndNames
  let mbIsAutoRickshawAllowed = (\(_, _, _, mbIsAutoRickshawAllowed', _) -> mbIsAutoRickshawAllowed') <$> mbTollChargesAndNames
  let mbIsTwoWheelerAllowed = join ((\(_, _, _, _, isTwoWheelerAllowed) -> isTwoWheelerAllowed) <$> mbTollChargesAndNames)
  let allFarePolicies = selectFarePolicy (fromMaybe 0 mbDistance) (fromMaybe 0 mbDuration) mbIsAutoRickshawAllowed mbIsTwoWheelerAllowed fareProducts.farePolicies
  estimates <- mapMaybeM (\fp -> buildEstimateHelper fp mbTollCharges mbTollNames mbTollIds now transporterConfig.currency) allFarePolicies
  let estimateAPIEntity = map buildEstimateApiEntity estimates
  return API.Types.UI.FareCalculator.FareResponse {estimatedFares = estimateAPIEntity}
  where
    buildEstimateHelper fp mbTollCharges mbTollNames mbTollIds now currency = do
      CQVST.findByServiceTierTypeAndCityIdInRideFlow fp.vehicleServiceTier merchanOperatingCityId configsInExperimentVersions
        >>= \case
          Just vehicleServiceTierItem -> do
            estimate <- DBS.buildEstimate merchantId merchanOperatingCityId currency Meter Nothing now False Nothing False mbDistance Nothing mbTollCharges mbTollNames mbTollIds Nothing Nothing 0 mbDuration False vehicleServiceTierItem fp
            return $ Just estimate
          Nothing -> return Nothing

    selectFarePolicy distance' duration' mbIsAutoRickshawAllowed' mbIsTwoWheelerAllowed' =
      filter (\farePolicy -> isValid farePolicy mbVehicleServiceTier)
      where
        isValid farePolicy (Just vehicleServiceTier) = farePolicy.vehicleServiceTier == vehicleServiceTier && checkDistanceBounds farePolicy && checkExtendUpto farePolicy && (autosAllowedOnTollRoute farePolicy || bikesAllowedOnTollRoute farePolicy)
        isValid farePolicy Nothing = checkDistanceBounds farePolicy && checkExtendUpto farePolicy && (autosAllowedOnTollRoute farePolicy || bikesAllowedOnTollRoute farePolicy)
        autosAllowedOnTollRoute farePolicy = if farePolicy.vehicleServiceTier == DVST.AUTO_RICKSHAW then fromMaybe True mbIsAutoRickshawAllowed' else True
        bikesAllowedOnTollRoute farePolicy = if farePolicy.vehicleServiceTier == DVST.BIKE then fromMaybe True mbIsTwoWheelerAllowed' else True
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

buildEstimateApiEntity :: Domain.Types.Estimate.Estimate -> API.Types.UI.FareCalculator.EstimateApi
buildEstimateApiEntity Domain.Types.Estimate.Estimate {..} = do
  API.Types.UI.FareCalculator.EstimateApi {..}

calculateDistanceAndRoutes ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Kernel.Prelude.Int ->
  [Maps.LatLong] ->
  Environment.Flow (Maybe Meters, Maybe Meters, Maybe Seconds, Maybe Utils.RouteInfo, Maybe [Utils.RouteInfo])
calculateDistanceAndRoutes merchantId merchantOpCityId distanceWeightage latLongs = do
  let request =
        Utils.GetRoutesReq
          { waypoints = NE.fromList latLongs,
            calcPoints = True,
            mode = Just Utils.CAR
          }
  routeResponse <- Maps.getRoutes merchantId merchantOpCityId Nothing request
  let durationWeightage = 100 - distanceWeightage
      (shortestRouteInfo, shortestRouteIndex) = Utils.getEfficientRouteInfo routeResponse distanceWeightage durationWeightage
      longestRouteDistance = (.distance) =<< Utils.getLongestRouteDistance routeResponse
      shortestRouteDistance = (.distance) =<< shortestRouteInfo
      shortestRouteDuration = (.duration) =<< shortestRouteInfo
  return (longestRouteDistance, shortestRouteDistance, shortestRouteDuration, shortestRouteInfo, Just $ Utils.updateEfficientRoutePosition routeResponse shortestRouteIndex)
