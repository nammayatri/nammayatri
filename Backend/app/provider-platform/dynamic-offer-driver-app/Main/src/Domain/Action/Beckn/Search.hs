{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Search
  ( DSearchReq (..),
    DSearchRes (..),
    NearestDriverInfo (..),
    handler,
    validateRequest,
  )
where

import qualified Beckn.Types.Core.Taxi.Search as BA
import Control.Applicative ((<|>))
import Data.List (sortBy)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Ord
import qualified Data.Text as T
import qualified Domain.Action.UI.Maps as DMaps
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant.TransporterConfig as DTMT
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RideRoute
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.VehicleServiceTier as DVST
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.Queries.GateInfo (findGateInfoByLatLongWithoutGeoJson)
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.BlockedRouteDetector
import SharedLogic.DriverPool
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import SharedLogic.GoogleMaps
import qualified SharedLogic.Merchant as SMerchant
import SharedLogic.Ride
import SharedLogic.TollsDetector
import Storage.Cac.DriverPoolConfig as CDP
import Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantState as CQMS
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics.ARDUBPPMetrics as Metrics
import Utils.Common.Cac.KeyNameConstants

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCountry :: Context.Country,
    customerPhoneNum :: Maybe Text,
    pickupLocation :: LatLong,
    pickupTime :: UTCTime,
    pickupAddress :: Maybe BA.Address,
    device :: Maybe Text,
    customerLanguage :: Maybe Maps.Language,
    disabilityTag :: Maybe Text,
    isReallocationEnabled :: Maybe Bool,
    dropLocation :: Maybe LatLong,
    dropAddrress :: Maybe BA.Address,
    routeDistance :: Maybe Meters,
    routeDuration :: Maybe Seconds,
    routePoints :: Maybe [LatLong],
    multipleRoutes :: Maybe [Maps.RouteInfo]
  }

data ValidatedDSearchReq = ValidatedDSearchReq
  { transporterConfig :: DTMT.TransporterConfig,
    possibleTripOption :: DTC.TripOption,
    bapCity :: Context.City,
    merchantOpCityId :: Id DMOC.MerchantOperatingCity,
    merchant :: DM.Merchant
  }

data DSearchRes = DSearchRes
  { specialLocationTag :: Maybe Text,
    searchMetricsMVar :: Metrics.SearchMetricsMVar,
    paymentMethodsInfo :: [DMPM.PaymentMethodInfo],
    provider :: DM.Merchant,
    fromLocation :: LatLong,
    toLocation :: Maybe LatLong,
    now :: UTCTime,
    quotes :: [(DQuote.Quote, DVST.VehicleServiceTier, Maybe NearestDriverInfo)],
    estimates :: [(DEst.Estimate, DVST.VehicleServiceTier, Maybe NearestDriverInfo)]
  }

data NearestDriverInfo = NearestDriverInfo
  { locationId :: Text,
    distanceToNearestDriver :: Meters,
    driverLatLongs :: NonEmpty LatLong
  }
  deriving (Generic, Show)

getRouteServiceability :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> LatLong -> Maybe [LatLong] -> Maybe Meters -> Maybe Seconds -> Maybe [Maps.RouteInfo] -> Flow RouteServiceability
getRouteServiceability _ merchantOpCityId _ _ (Just routePoints) (Just distance) (Just duration) (Just multipleRoutes) = do
  checkRouteServiceability merchantOpCityId (0, routePoints, distance, duration) multipleRoutes
getRouteServiceability _ merchantOpCityId _ _ (Just routePoints) (Just distance) (Just duration) Nothing = do
  checkRouteServiceability merchantOpCityId (0, routePoints, distance, duration) []
getRouteServiceability merchantId merchantOpCityId fromLocation toLocation _ _ _ _ = do
  response <-
    Maps.getDistance merchantId merchantOpCityId $
      Maps.GetDistanceReq
        { origin = fromLocation,
          destination = toLocation,
          travelMode = Just Maps.CAR
        }
  return $
    RouteServiceability
      { routePoints = [],
        routeDistance = response.distance,
        routeDuration = response.duration,
        multipleRoutes = [],
        isCustomerPrefferedSearchRoute = True,
        isBlockedRoute = False
      }

handler :: ValidatedDSearchReq -> DSearchReq -> Flow DSearchRes
handler ValidatedDSearchReq {..} sReq = do
  searchMetricsMVar <- Metrics.startSearchMetrics merchant.name
  let merchantId = merchant.id
  sessiontoken <- generateGUIDText
  fromLocation <- buildSearchReqLocation merchant.id merchantOpCityId sessiontoken sReq.pickupAddress sReq.customerLanguage sReq.pickupLocation

  (mbSetRouteInfo, mbToLocation, mbDistance, mbDuration, mbIsCustomerPrefferedSearchRoute, mbIsBlockedRoute, mbTollCharges, mbTollNames, mbIsAutoRickshawAllowed) <-
    case sReq.dropLocation of
      Just dropLoc -> do
        serviceableRoute <- getRouteServiceability merchant.id merchantOpCityId sReq.pickupLocation dropLoc sReq.routePoints sReq.routeDistance sReq.routeDuration sReq.multipleRoutes
        let estimatedDistance = serviceableRoute.routeDistance
            estimatedDuration = serviceableRoute.routeDuration
        logDebug $ "distance: " <> show estimatedDistance
        let routeInfo = RouteInfo {distance = Just estimatedDistance, duration = Just estimatedDuration, points = Just serviceableRoute.routePoints}
        toLocation <- buildSearchReqLocation merchant.id merchantOpCityId sessiontoken sReq.dropAddrress sReq.customerLanguage dropLoc
        let setRouteInfo transactionId =
              ( do
                  Redis.setExp (searchRequestKey transactionId) routeInfo 3600
                  Redis.setExp (multipleRouteKey transactionId) (createMultipleRouteInfo <$> serviceableRoute.multipleRoutes) 3600
              )
        mbTollChargesAndNames <- getTollInfoOnRoute merchantOpCityId Nothing serviceableRoute.routePoints
        return (Just setRouteInfo, Just toLocation, Just estimatedDistance, Just estimatedDuration, Just serviceableRoute.isCustomerPrefferedSearchRoute, Just serviceableRoute.isBlockedRoute, (\(charges, _, _) -> charges) <$> mbTollChargesAndNames, (\(_, names, _) -> names) <$> mbTollChargesAndNames, (\(_, _, isAutoRickshawAllowed) -> isAutoRickshawAllowed) <$> mbTollChargesAndNames)
      _ -> return (Nothing, Nothing, sReq.routeDistance, sReq.routeDuration, Nothing, Nothing, Nothing, Nothing, Nothing) -- estimate distance and durations by user
  allFarePoliciesProduct <- combineFarePoliciesProducts <$> ((getAllFarePoliciesProduct merchant.id merchantOpCityId sReq.pickupLocation sReq.dropLocation (Just (TransactionId (Id sReq.transactionId)))) `mapM` possibleTripOption.tripCategories)
  let farePolicies = selectFarePolicy (fromMaybe 0 mbDistance) (fromMaybe 0 mbDuration) mbIsAutoRickshawAllowed allFarePoliciesProduct.farePolicies

  (driverPool, selectedFarePolicies) <-
    if transporterConfig.considerDriversForSearch
      then do
        (pool, policies) <- selectDriversAndMatchFarePolicies merchantId merchantOpCityId mbDistance fromLocation transporterConfig possibleTripOption.isScheduled allFarePoliciesProduct.area farePolicies
        pure (nonEmpty pool, policies)
      else return (Nothing, catMaybes $ everyPossibleVariant <&> \var -> find ((== var) . (.vehicleServiceTier)) farePolicies)
  (mbSpecialZoneGateId, mbDefaultDriverExtra) <- getSpecialPickupZoneInfo allFarePoliciesProduct.specialLocationTag fromLocation
  logDebug $ "Pickingup Gate info result : " <> show (mbSpecialZoneGateId, mbDefaultDriverExtra)
  let specialLocationTag = maybe allFarePoliciesProduct.specialLocationTag (\_ -> allFarePoliciesProduct.specialLocationTag <&> (<> "_PickupZone")) mbSpecialZoneGateId
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  searchReq <- buildSearchRequest sReq bapCity mbSpecialZoneGateId mbDefaultDriverExtra possibleTripOption.schedule possibleTripOption.isScheduled merchantId merchantOpCityId fromLocation mbToLocation mbDistance mbDuration specialLocationTag allFarePoliciesProduct.area mbTollCharges mbTollNames mbIsCustomerPrefferedSearchRoute mbIsBlockedRoute currency
  whenJust mbSetRouteInfo $ \setRouteInfo -> setRouteInfo sReq.transactionId
  triggerSearchEvent SearchEventData {searchRequest = searchReq, merchantId = merchantId}
  void $ QSR.createDSReq searchReq

  let buildEstimateHelper = buildEstimate merchantOpCityId currency searchReq.id possibleTripOption.schedule possibleTripOption.isScheduled mbDistance specialLocationTag mbTollCharges mbTollNames mbIsCustomerPrefferedSearchRoute mbIsBlockedRoute
  let buildQuoteHelper = buildQuote merchantOpCityId searchReq merchantId possibleTripOption.schedule possibleTripOption.isScheduled mbDistance mbDuration specialLocationTag mbTollCharges mbTollNames mbIsCustomerPrefferedSearchRoute mbIsBlockedRoute
  (estimates, quotes) <- foldrM (processPolicy buildEstimateHelper buildQuoteHelper) ([], []) selectedFarePolicies
  QEst.createMany estimates
  for_ quotes QQuote.create

  forM_ estimates $ \est -> triggerEstimateEvent EstimateEventData {estimate = est, merchantId = merchantId}
  driverInfoQuotes <- addNearestDriverInfo merchantOpCityId driverPool quotes
  driverInfoEstimates <- addNearestDriverInfo merchantOpCityId driverPool estimates
  buildDSearchResp sReq.pickupLocation sReq.dropLocation specialLocationTag searchMetricsMVar driverInfoQuotes driverInfoEstimates
  where
    getSpecialPickupZoneInfo :: Maybe Text -> DLoc.Location -> Flow (Maybe Text, Maybe HighPrecMoney)
    getSpecialPickupZoneInfo Nothing _ = pure (Nothing, Nothing)
    getSpecialPickupZoneInfo (Just _) fromLocation = do
      mbPickupZone <- findGateInfoByLatLongWithoutGeoJson (LatLong fromLocation.lat fromLocation.lon)
      if ((.canQueueUpOnGate) <$> mbPickupZone) == Just True
        then pure $ ((.id.getId) <$> mbPickupZone, fmap (toHighPrecMoney . Money) . (.defaultDriverExtra) =<< mbPickupZone) -- FIXME
        else pure (Nothing, Nothing)

    combineFarePoliciesProducts :: [FarePoliciesProduct] -> FarePoliciesProduct
    combineFarePoliciesProducts products =
      FarePoliciesProduct
        { farePolicies = concatMap farePolicies products,
          area = maybe SL.Default (.area) $ listToMaybe products,
          specialLocationTag = (listToMaybe products) >>= (.specialLocationTag)
        }

    processPolicy ::
      (Bool -> DFP.FullFarePolicy -> Flow DEst.Estimate) ->
      (Bool -> DFP.FullFarePolicy -> Flow DQuote.Quote) ->
      DFP.FullFarePolicy ->
      ([DEst.Estimate], [DQuote.Quote]) ->
      Flow ([DEst.Estimate], [DQuote.Quote])
    processPolicy buildEstimateHelper buildQuoteHelper fp (estimates, quotes) =
      case fp.tripCategory of
        DTC.OneWay DTC.OneWayOnDemandDynamicOffer -> (buildEstimateHelper False) fp >>= \est -> pure (est : estimates, quotes)
        DTC.Rental _ -> (buildQuoteHelper True) fp >>= \quote -> pure (estimates, quote : quotes)
        _ -> (buildQuoteHelper False) fp >>= \quote -> pure (estimates, quote : quotes)

    buildDSearchResp fromLocation toLocation specialLocationTag searchMetricsMVar quotes estimates = do
      merchantPaymentMethods <- CQMPM.findAllByMerchantOpCityId merchantOpCityId
      let paymentMethodsInfo = DMPM.mkPaymentMethodInfo <$> merchantPaymentMethods
      now <- getCurrentTime
      return $
        DSearchRes
          { provider = merchant,
            ..
          }

    selectFarePolicy distance duration mbIsAutoRickshawAllowed =
      filter isValid
      where
        isValid farePolicy = checkDistanceBounds farePolicy && checkExtendUpto farePolicy && autosAllowedOnTollRoute farePolicy

        autosAllowedOnTollRoute farePolicy = if farePolicy.vehicleServiceTier == DVST.AUTO_RICKSHAW then (fromMaybe True mbIsAutoRickshawAllowed) else True

        checkDistanceBounds farePolicy = maybe True checkBounds farePolicy.allowedTripDistanceBounds

        checkBounds bounds = bounds.minAllowedTripDistance <= distance && distance <= bounds.maxAllowedTripDistance

        checkExtendUpto farePolicy = case farePolicy.farePolicyDetails of
          DFP.RentalDetails det -> checkLimits det
          _ -> True
          where
            checkLimits det =
              let distInKm = distance.getMeters `div` 1000
                  timeInHr = duration.getSeconds `div` 3600
                  includedKm = (timeInHr * det.includedKmPerHr.getKilometers)
                  maxAllowed = min (min det.maxAdditionalKmsLimit.getKilometers includedKm) (det.totalAdditionalKmsLimit.getKilometers - includedKm)
               in distInKm - includedKm <= maxAllowed

addNearestDriverInfo ::
  (HasField "vehicleServiceTier" a DVST.ServiceTierType) =>
  Id DMOC.MerchantOperatingCity ->
  (Maybe (NonEmpty DriverPoolResult)) ->
  [a] ->
  Flow [(a, DVST.VehicleServiceTier, Maybe NearestDriverInfo)]
addNearestDriverInfo merchantOpCityId Nothing estdOrQuotes = do
  forM estdOrQuotes $ \estdOrQuote -> do
    vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityId estdOrQuote.vehicleServiceTier merchantOpCityId >>= fromMaybeM (VehicleServiceTierNotFound (show estdOrQuote.vehicleServiceTier))
    return (estdOrQuote, vehicleServiceTierItem, Nothing)
addNearestDriverInfo merchantOpCityId (Just driverPool) estdOrQuotes = do
  let mapOfDPRByServiceTier = foldl (\m dpr -> M.insertWith (<>) dpr.serviceTier (pure dpr) m) mempty driverPool
  traverse (matchInputWithNearestDriver mapOfDPRByServiceTier) estdOrQuotes
  where
    matchInputWithNearestDriver ::
      (HasField "vehicleServiceTier" a DVST.ServiceTierType) =>
      M.Map DVST.ServiceTierType (NonEmpty DriverPoolResult) ->
      a ->
      Flow (a, DVST.VehicleServiceTier, Maybe NearestDriverInfo)
    matchInputWithNearestDriver driverPools input = do
      vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityId input.vehicleServiceTier merchantOpCityId >>= fromMaybeM (VehicleServiceTierNotFound (show input.vehicleServiceTier))
      let driverPool' = M.lookup input.vehicleServiceTier driverPools
      logDebug $ "DP:Nearest driver info " <> show driverPool'
      case driverPool' of
        Nothing -> return (input, vehicleServiceTierItem, Nothing)
        Just dp -> do
          let driverLatLongs = fmap (\x -> LatLong x.lat x.lon) dp
              distanceToNearestDriver = NE.head dp & (.distanceToPickup)
              locationId = NE.head dp & (.driverId) & (.getId)
              nearestDriverInfo = NearestDriverInfo {..}
          logDebug $ "Nearest driver info " <> show nearestDriverInfo
          return (input, vehicleServiceTierItem, Just nearestDriverInfo)

selectDriversAndMatchFarePolicies :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Meters -> DLoc.Location -> DTMT.TransporterConfig -> Bool -> SL.Area -> [DFP.FullFarePolicy] -> Flow ([DriverPoolResult], [DFP.FullFarePolicy])
selectDriversAndMatchFarePolicies merchantId merchantOpCityId mbDistance fromLocation transporterConfig isScheduled area farePolicies = do
  driverPoolCfg <- CDP.getSearchDriverPoolConfig merchantOpCityId mbDistance area
  cityServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId
  driverPoolNotOnRide <- calculateDriverPool cityServiceTiers Estimate (fromJust driverPoolCfg) [] fromLocation merchantId True Nothing False False
  logDebug $ "Driver Pool not on ride " <> show driverPoolNotOnRide
  driverPoolCurrentlyOnRide <-
    if null driverPoolNotOnRide
      then do
        if transporterConfig.includeDriverCurrentlyOnRide
          then calculateDriverPoolCurrentlyOnRide cityServiceTiers Estimate (fromJust driverPoolCfg) [] fromLocation merchantId Nothing False False
          else pure []
      else pure []
  let driverPool =
        driverPoolNotOnRide
          <> map (\DriverPoolResultCurrentlyOnRide {..} -> DriverPoolResult {..}) driverPoolCurrentlyOnRide
  logDebug $ "Search handler: driver pool " <> show driverPool
  let onlyFPWithDrivers = filter (\fp -> isScheduled || (skipDriverPoolCheck fp.tripCategory) || (isJust (find (\dp -> dp.serviceTier == fp.vehicleServiceTier) driverPool))) farePolicies
  return (driverPool, onlyFPWithDrivers)

skipDriverPoolCheck :: DTC.TripCategory -> Bool
skipDriverPoolCheck (DTC.OneWay DTC.OneWayOnDemandStaticOffer) = False
skipDriverPoolCheck (DTC.OneWay DTC.OneWayOnDemandDynamicOffer) = False
skipDriverPoolCheck _ = True

buildSearchRequest ::
  ( KvDbFlow m r,
    EsqDBReplicaFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DSearchReq ->
  Context.City ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  UTCTime ->
  Bool ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DLoc.Location ->
  Maybe DLoc.Location ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Text ->
  SL.Area ->
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe Bool ->
  Maybe Bool ->
  Currency ->
  m DSR.SearchRequest
buildSearchRequest DSearchReq {..} bapCity mbSpecialZoneGateId mbDefaultDriverExtra startTime isScheduled providerId merchantOpCityId fromLocation mbToLocation mbDistance mbDuration specialLocationTag area tollCharges tollNames isCustomerPrefferedSearchRoute isBlockedRoute currency = do
  uuid <- generateGUID
  now <- getCurrentTime
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill = searchRequestExpirationSeconds `addUTCTime` startTime
  pure
    DSR.SearchRequest
      { id = Id uuid,
        messageId = Nothing,
        area = Just area,
        bapCity = Just bapCity,
        bapCountry = Just bapCountry,
        autoAssignEnabled = Nothing,
        merchantOperatingCityId = merchantOpCityId,
        toLocation = mbToLocation,
        estimatedDistance = mbDistance,
        estimatedDuration = mbDuration,
        riderId = Nothing,
        createdAt = now,
        driverDefaultExtraFee = mbDefaultDriverExtra,
        pickupZoneGateId = mbSpecialZoneGateId,
        customerCancellationDues = Nothing,
        currency,
        ..
      }

buildQuote ::
  ( KvDbFlow m r,
    EsqDBReplicaFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  Id DMOC.MerchantOperatingCity ->
  DSR.SearchRequest ->
  Id DM.Merchant ->
  UTCTime ->
  Bool ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe Bool ->
  Maybe Bool ->
  Bool ->
  DFP.FullFarePolicy ->
  m DQuote.Quote
buildQuote merchantOpCityId searchRequest transporterId pickupTime isScheduled mbDistance mbDuration specialLocationTag tollCharges tollNames isCustomerPrefferedSearchRoute isBlockedRoute nightShiftOverlapChecking fullFarePolicy = do
  let dist = fromMaybe 0 mbDistance
  fareParams <-
    calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = fullFarePolicy,
          actualDistance = Just dist,
          rideTime = pickupTime,
          waitingTime = Nothing,
          actualRideDuration = Nothing,
          avgSpeedOfVehicle = Nothing,
          driverSelectedFare = Nothing,
          customerExtraFee = Nothing,
          nightShiftCharge = Nothing,
          customerCancellationDues = Nothing,
          nightShiftOverlapChecking = nightShiftOverlapChecking,
          estimatedDistance = searchRequest.estimatedDistance,
          estimatedRideDuration = searchRequest.estimatedDuration,
          timeDiffFromUtc = Nothing,
          tollCharges = tollCharges,
          currency = searchRequest.currency
        }
  quoteId <- Id <$> generateGUID
  void $ cacheFarePolicyByQuoteId quoteId.getId fullFarePolicy
  now <- getCurrentTime
  let estimatedFare = fareSum fareParams
      estimatedFinishTime = (\duration -> fromIntegral duration `addUTCTime` now) <$> mbDuration
  -- Keeping quote expiry as search request expiry. Slack discussion: https://juspay.slack.com/archives/C0139KHBFU1/p1683349807003679
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityId fullFarePolicy.vehicleServiceTier merchantOpCityId >>= fromMaybeM (VehicleServiceTierNotFound (show fullFarePolicy.vehicleServiceTier))
  let validTill = searchRequestExpirationSeconds `addUTCTime` now
      isTollApplicableForServiceTier = DTC.isTollApplicable fullFarePolicy.vehicleServiceTier
  pure
    DQuote.Quote
      { id = quoteId,
        searchRequestId = searchRequest.id,
        providerId = transporterId,
        distance = mbDistance,
        vehicleServiceTier = fullFarePolicy.vehicleServiceTier,
        vehicleServiceTierName = Just vehicleServiceTierItem.name,
        tripCategory = fullFarePolicy.tripCategory,
        farePolicy = Just $ DFP.fullFarePolicyToFarePolicy fullFarePolicy,
        tollNames = if isTollApplicableForServiceTier then tollNames else Nothing,
        createdAt = now,
        updatedAt = now,
        currency = searchRequest.currency,
        ..
      }

buildEstimate ::
  (KvDbFlow m r, EsqDBReplicaFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  Id DSR.SearchRequest ->
  UTCTime ->
  Bool ->
  Maybe Meters ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe Bool ->
  Maybe Bool ->
  Bool ->
  DFP.FullFarePolicy ->
  m DEst.Estimate
buildEstimate merchantOpCityId currency searchReqId startTime isScheduled mbDistance specialLocationTag tollCharges tollNames isCustomerPrefferedSearchRoute isBlockedRoute nightShiftOverlapChecking fullFarePolicy = do
  let dist = fromMaybe 0 mbDistance -- TODO: Fix Later
  fareParams <-
    calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = fullFarePolicy,
          actualDistance = Just dist,
          rideTime = startTime,
          waitingTime = Nothing,
          actualRideDuration = Nothing,
          avgSpeedOfVehicle = Nothing,
          driverSelectedFare = Nothing,
          customerExtraFee = Nothing,
          nightShiftCharge = Nothing,
          customerCancellationDues = Nothing,
          nightShiftOverlapChecking = nightShiftOverlapChecking,
          estimatedDistance = Nothing,
          estimatedRideDuration = Nothing,
          timeDiffFromUtc = Nothing,
          tollCharges = tollCharges,
          currency
        }
  let baseFare = fareSum fareParams
  logDebug $ "baseFare: " <> show baseFare
  estimateId <- Id <$> generateGUID
  now <- getCurrentTime
  void $ cacheFarePolicyByEstimateId estimateId.getId fullFarePolicy
  let mbDriverExtraFeeBounds = DFP.findDriverExtraFeeBoundsByDistance dist <$> fullFarePolicy.driverExtraFeeBounds
  vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityId fullFarePolicy.vehicleServiceTier merchantOpCityId >>= fromMaybeM (VehicleServiceTierNotFound (show fullFarePolicy.vehicleServiceTier))
  let isTollApplicableForServiceTier = DTC.isTollApplicable fullFarePolicy.vehicleServiceTier
  pure
    DEst.Estimate
      { id = estimateId,
        requestId = searchReqId,
        vehicleServiceTier = fullFarePolicy.vehicleServiceTier,
        vehicleServiceTierName = Just vehicleServiceTierItem.name,
        tripCategory = fullFarePolicy.tripCategory,
        estimatedDistance = mbDistance,
        minFare = baseFare + maybe 0.0 (.minFee) mbDriverExtraFeeBounds,
        maxFare = baseFare + maybe 0.0 (.maxFee) mbDriverExtraFeeBounds,
        currency,
        fareParams = Just fareParams,
        farePolicy = Just $ DFP.fullFarePolicyToFarePolicy fullFarePolicy,
        specialLocationTag = specialLocationTag,
        isScheduled = isScheduled,
        tollNames = if isTollApplicableForServiceTier then tollNames else Nothing,
        createdAt = now,
        updatedAt = now,
        ..
      }

validateRequest :: Id DM.Merchant -> DSearchReq -> Flow ValidatedDSearchReq
validateRequest merchantId sReq = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless merchant.enabled $ throwError AgencyDisabled
  -- This checks for origin serviceability too
  NearestOperatingAndSourceCity {nearestOperatingCity, sourceCity} <- getNearestOperatingAndSourceCity merchant sReq.pickupLocation
  let bapCity = nearestOperatingCity.city
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just bapCity)
  transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId (Just (TransactionId (Id sReq.transactionId))) >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)
  (isInterCity, isCrossCity) <-
    case sReq.dropLocation of
      Just dropLoc -> do
        destinationCityState <- getDestinationCity merchant dropLoc -- This checks for destination serviceability too
        if destinationCityState.city == sourceCity.city && destinationCityState.city /= Context.AnyCity
          then return (False, False)
          else do
            mbMerchantState <- CQMS.findByMerchantIdAndState merchant.id sourceCity.state
            let allowedStates = maybe [sourceCity.state] (.allowedDestinationStates) mbMerchantState
            -- Destination states should be in the allowed states of the origin state
            if destinationCityState.state `elem` allowedStates
              then do
                if destinationCityState.city `elem` transporterConfig.crossTravelCities
                  then return (True, True)
                  else return (True, False)
              else throwError (RideNotServiceableInState $ show destinationCityState.state)
      Nothing -> pure (False, False)

  now <- getCurrentTime
  let possibleTripOption = getPossibleTripOption now transporterConfig sReq isInterCity isCrossCity
  return ValidatedDSearchReq {..}

getPossibleTripOption :: UTCTime -> DTMT.TransporterConfig -> DSearchReq -> Bool -> Bool -> DTC.TripOption
getPossibleTripOption now tConf dsReq isInterCity isCrossCity = do
  let (schedule, isScheduled) =
        if tConf.scheduleRideBufferTime `addUTCTime` now < dsReq.pickupTime
          then (dsReq.pickupTime, True)
          else (now, False)
      tripCategories =
        case dsReq.dropLocation of
          Just _ -> do
            if isInterCity
              then do
                if isCrossCity
                  then do
                    [DTC.CrossCity DTC.OneWayOnDemandStaticOffer, DTC.RoundTrip DTC.OnDemandStaticOffer, DTC.Rental DTC.OnDemandStaticOffer]
                      <> (if not isScheduled then [DTC.CrossCity DTC.OneWayRideOtp, DTC.CrossCity DTC.OneWayOnDemandDynamicOffer, DTC.RoundTrip DTC.RideOtp, DTC.Rental DTC.RideOtp] else [])
                  else do
                    [DTC.InterCity DTC.OneWayOnDemandStaticOffer, DTC.RoundTrip DTC.OnDemandStaticOffer, DTC.Rental DTC.OnDemandStaticOffer]
                      <> (if not isScheduled then [DTC.InterCity DTC.OneWayRideOtp, DTC.InterCity DTC.OneWayOnDemandDynamicOffer, DTC.RoundTrip DTC.RideOtp, DTC.Rental DTC.RideOtp] else [])
              else do
                [DTC.OneWay DTC.OneWayOnDemandStaticOffer, DTC.RoundTrip DTC.OnDemandStaticOffer, DTC.Rental DTC.OnDemandStaticOffer]
                  <> (if not isScheduled then [DTC.OneWay DTC.OneWayRideOtp, DTC.OneWay DTC.OneWayOnDemandDynamicOffer, DTC.RoundTrip DTC.RideOtp, DTC.Rental DTC.RideOtp] else [])
          Nothing ->
            [DTC.Rental DTC.OnDemandStaticOffer]
              <> [DTC.Rental DTC.RideOtp | not isScheduled]

  DTC.TripOption {..}

data NearestOperatingAndSourceCity = NearestOperatingAndSourceCity
  { nearestOperatingCity :: CityState,
    sourceCity :: CityState
  }

data CityState = CityState
  { city :: Context.City,
    state :: Context.IndianState
  }

getNearestOperatingAndSourceCity :: DM.Merchant -> LatLong -> Flow NearestOperatingAndSourceCity
getNearestOperatingAndSourceCity merchant pickupLatLong = do
  let geoRestriction = merchant.geofencingConfig.origin
  let merchantCityState = CityState {city = merchant.city, state = merchant.state}
  case geoRestriction of
    Unrestricted -> do
      pure $ NearestOperatingAndSourceCity {nearestOperatingCity = merchantCityState, sourceCity = merchantCityState}
    Regions regions -> do
      {-
        Below logic is to find the nearest operating city for the pickup location.
        If the pickup location is in the operating city, then return the city.
        If the pickup location is not in the city, then return the nearest city for that state else the merchant default city.
      -}
      geoms <- B.runInReplica $ QGeometry.findGeometriesContaining pickupLatLong regions
      case filter (\geom -> geom.city /= Context.AnyCity) geoms of
        [] ->
          find (\geom -> geom.city == Context.AnyCity) geoms & \case
            Just anyCityGeom -> do
              cities <- CQMOC.findAllByMerchantIdAndState merchant.id anyCityGeom.state >>= mapM (\m -> return (distanceBetweenInMeters pickupLatLong m.location, m.city))
              let nearestOperatingCity = maybe merchantCityState (\p -> CityState {city = snd p, state = anyCityGeom.state}) (listToMaybe $ sortBy (comparing fst) cities)
              return $ NearestOperatingAndSourceCity {sourceCity = CityState {city = anyCityGeom.city, state = anyCityGeom.state}, nearestOperatingCity}
            Nothing -> do
              logError $ "No geometry found for pickupLatLong: " <> show pickupLatLong <> " for regions: " <> show regions
              throwError RideNotServiceable
        (g : _) -> do
          -- Nearest operating city and source city are same
          let operatingCityState = CityState {city = g.city, state = g.state}
          return $ NearestOperatingAndSourceCity {nearestOperatingCity = operatingCityState, sourceCity = operatingCityState}

getDestinationCity :: DM.Merchant -> LatLong -> Flow CityState
getDestinationCity merchant dropLatLong = do
  let geoRestriction = merchant.geofencingConfig.destination
  case geoRestriction of
    Unrestricted -> return CityState {city = merchant.city, state = merchant.state}
    Regions regions -> do
      geoms <- B.runInReplica $ QGeometry.findGeometriesContaining dropLatLong regions
      case filter (\geom -> geom.city /= Context.AnyCity) geoms of
        [] ->
          find (\geom -> geom.city == Context.AnyCity) geoms & \case
            Just anyCityGeom -> return CityState {city = anyCityGeom.city, state = anyCityGeom.state}
            Nothing -> do
              logError $ "No geometry found for dropLatLong: " <> show dropLatLong <> " for regions: " <> show regions
              throwError RideNotServiceable
        (g : _) -> return CityState {city = g.city, state = g.state}

buildSearchReqLocation :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Maybe BA.Address -> Maybe Maps.Language -> LatLong -> m DLoc.Location
buildSearchReqLocation merchantId merchantOpCityId sessionToken address customerLanguage latLong@Maps.LatLong {..} = do
  updAddress <- case address of
    Just loc
      | customerLanguage == Just Maps.ENGLISH && isJust loc.ward ->
        pure $
          Address
            { areaCode = loc.area_code,
              street = loc.street,
              door = loc.door,
              city = loc.city,
              state = loc.state,
              country = loc.country,
              building = loc.building,
              area = loc.ward,
              full_address = decodeAddress loc
            }
    _ -> getAddressByGetPlaceName merchantId merchantOpCityId sessionToken latLong
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure $
    DLoc.Location
      { address =
          DLoc.LocationAddress
            { areaCode = (address >>= (.area_code)) <|> updAddress.areaCode,
              street = (address >>= (.street)) <|> updAddress.street,
              door = (address >>= (.door)) <|> updAddress.door,
              city = (address >>= (.city)) <|> updAddress.city,
              state = (address >>= (.state)) <|> updAddress.state,
              country = (address >>= (.country)) <|> updAddress.country,
              building = (address >>= (.building)) <|> updAddress.building,
              area = (address >>= (.ward)) <|> updAddress.area,
              fullAddress = (address >>= decodeAddress) <|> updAddress.full_address
            },
        ..
      }

getAddressByGetPlaceName :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> LatLong -> m Address
getAddressByGetPlaceName merchantId merchantOpCityId sessionToken latLong = do
  pickupRes <-
    DMaps.getPlaceName merchantId merchantOpCityId $
      Maps.GetPlaceNameReq
        { getBy = Maps.ByLatLong latLong,
          sessionToken = Just sessionToken,
          language = Nothing
        }
  pure $ mkLocation pickupRes

decodeAddress :: BA.Address -> Maybe Text
decodeAddress BA.Address {..} = do
  let strictFields = catMaybes $ filter (not . isEmpty) [door, building, street, locality, city, state, area_code, country]
  if null strictFields
    then Nothing
    else Just $ T.intercalate ", " strictFields

isEmpty :: Maybe Text -> Bool
isEmpty = maybe True (T.null . T.replace " " "")
