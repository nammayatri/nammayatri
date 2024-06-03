{-# OPTIONS_GHC -Wno-orphans #-}

-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FareCalculator where

import qualified API.Types.UI.FareCalculator
import qualified Data.List.NonEmpty as NE
import qualified Domain.Action.Beckn.Search as DBS
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Estimate
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.ServiceTierType as DVST
import qualified Environment
import EulerHS.Prelude hiding (id, map)
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Maps.Utils as Utils
import Kernel.Prelude hiding (concatMap)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FarePolicy as FP
import qualified SharedLogic.TollsDetector as TD
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
  now <- getCurrentTime
  fareProducts <- FP.getAllFarePoliciesProduct merchantId merchanOperatingCityId False pickupLatlong (Just dropLatLong) Nothing (DTC.OneWay DTC.OneWayOnDemandDynamicOffer)
  (_, mbDistance, mbDuration, mbRoute, _) <- calculateDistanceAndRoutes merchantId merchanOperatingCityId distanceWeightage [pickupLatlong, dropLatLong]
  mbTollChargesAndNames <- TD.getTollInfoOnRoute merchanOperatingCityId Nothing (maybe [] (\x -> x.points) mbRoute)
  let mbTollCharges = (\(tollCharges, _, _) -> tollCharges) <$> mbTollChargesAndNames
  let mbTollNames = (\(_, tollNames, _) -> tollNames) <$> mbTollChargesAndNames
  let mbIsAutoRickshawAllowed = (\(_, _, mbIsAutoRickshawAllowed') -> mbIsAutoRickshawAllowed') <$> mbTollChargesAndNames
  let allFarePolicies = selectFarePolicy (fromMaybe 0 mbDistance) (fromMaybe 0 mbDuration) mbIsAutoRickshawAllowed fareProducts.farePolicies
  estimates <- mapM (DBS.buildEstimate merchanOperatingCityId INR Meter (Id "") now False Nothing False mbDistance Nothing mbTollCharges mbTollNames Nothing Nothing False) allFarePolicies
  let estimateAPIEntity = map buildEstimateApiEntity estimates
  return API.Types.UI.FareCalculator.FareResponse {estimatedFares = estimateAPIEntity}
  where
    selectFarePolicy distance' duration' mbIsAutoRickshawAllowed' =
      filter isValid
      where
        isValid farePolicy = checkDistanceBounds farePolicy && checkExtendUpto farePolicy && autosAllowedOnTollRoute farePolicy
        autosAllowedOnTollRoute farePolicy = if farePolicy.vehicleServiceTier == DVST.AUTO_RICKSHAW then (fromMaybe True mbIsAutoRickshawAllowed') else True
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
  routeResponse <- Maps.getRoutes merchantId merchantOpCityId request
  let durationWeightage = 100 - distanceWeightage
      (shortestRouteInfo, shortestRouteIndex) = Utils.getEfficientRouteInfo routeResponse distanceWeightage durationWeightage
      longestRouteDistance = (.distance) =<< Utils.getLongestRouteDistance routeResponse
      shortestRouteDistance = (.distance) =<< shortestRouteInfo
      shortestRouteDuration = (.duration) =<< shortestRouteInfo
  return (longestRouteDistance, shortestRouteDistance, shortestRouteDuration, shortestRouteInfo, Just $ Utils.updateEfficientRoutePosition routeResponse shortestRouteIndex)
