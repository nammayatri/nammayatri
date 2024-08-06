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
    getNearestOperatingAndSourceCity,
    handler,
    validateRequest,
    buildEstimate,
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
import Domain.Types.BapMetadata
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RideRoute
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.TransporterConfig as DTMT
import qualified Domain.Types.VehicleServiceTier as DVST
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
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
import qualified SharedLogic.MerchantPaymentMethod as DMPM
import SharedLogic.Ride
import SharedLogic.TollsDetector
import Storage.Cac.DriverPoolConfig as CDP
import Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData
import qualified Storage.CachedQueries.InterCityTravelCities as CQITC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantState as CQMS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
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
    returnTime :: Maybe UTCTime,
    roundTrip :: Bool,
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
    isDashboardRequest :: Bool,
    multipleRoutes :: Maybe [Maps.RouteInfo]
  }

data ValidatedDSearchReq = ValidatedDSearchReq
  { transporterConfig :: DTMT.TransporterConfig,
    possibleTripOption :: DTC.TripOption,
    bapCity :: Context.City,
    merchantOpCityId :: Id DMOC.MerchantOperatingCity,
    distanceUnit :: DistanceUnit,
    merchant :: DM.Merchant,
    isValueAddNP :: Bool
  }

data DSearchRes = DSearchRes
  { specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text,
    searchMetricsMVar :: Metrics.SearchMetricsMVar,
    paymentMethodsInfo :: [DMPM.PaymentMethodInfo],
    provider :: DM.Merchant,
    fromLocation :: LatLong,
    toLocation :: Maybe LatLong,
    now :: UTCTime,
    quotes :: [(DQuote.Quote, DVST.VehicleServiceTier, Maybe NearestDriverInfo)],
    estimates :: [(DEst.Estimate, DVST.VehicleServiceTier, Maybe NearestDriverInfo)],
    transporterConfig :: DTMT.TransporterConfig,
    bapId :: Text
  }

data NearestDriverInfo = NearestDriverInfo
  { locationId :: Text,
    distanceToNearestDriver :: Meters,
    driverLatLongs :: NonEmpty LatLong
  }
  deriving (Generic, Show)

getRouteServiceability :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DistanceUnit -> LatLong -> LatLong -> Maybe [LatLong] -> Maybe Meters -> Maybe Seconds -> Maybe [Maps.RouteInfo] -> Flow RouteServiceability
getRouteServiceability _ merchantOpCityId _ _ _ (Just routePoints) (Just distance) (Just duration) (Just multipleRoutes) = do
  checkRouteServiceability merchantOpCityId (0, routePoints, distance, duration) multipleRoutes
getRouteServiceability _ merchantOpCityId _ _ _ (Just routePoints) (Just distance) (Just duration) Nothing = do
  checkRouteServiceability merchantOpCityId (0, routePoints, distance, duration) []
getRouteServiceability merchantId merchantOpCityId distanceUnit fromLocation toLocation _ _ _ _ = do
  response <-
    Maps.getDistance merchantId merchantOpCityId $
      Maps.GetDistanceReq
        { origin = fromLocation,
          destination = toLocation,
          travelMode = Just Maps.CAR,
          distanceUnit
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
  bapMetadata <- mkBapMetaData
  CQBapMetaData.createIfNotPresent bapMetadata (Id sReq.bapId) (show Domain.MOBILITY)
  searchMetricsMVar <- Metrics.startSearchMetrics merchant.name
  let merchantId = merchant.id
  sessiontoken <- generateGUIDText
  fromLocation <- buildSearchReqLocation merchant.id merchantOpCityId sessiontoken sReq.pickupAddress sReq.customerLanguage sReq.pickupLocation
  (mbSetRouteInfo, mbToLocation, mbDistance, mbDuration, mbIsCustomerPrefferedSearchRoute, mbIsBlockedRoute, mbTollCharges, mbTollNames, mbIsAutoRickshawAllowed) <-
    case sReq.dropLocation of
      Just dropLoc -> do
        serviceableRoute <- getRouteServiceability merchant.id merchantOpCityId distanceUnit sReq.pickupLocation dropLoc sReq.routePoints sReq.routeDistance sReq.routeDuration sReq.multipleRoutes
        let estimatedDistance = serviceableRoute.routeDistance
            estimatedDuration = serviceableRoute.routeDuration
        logDebug $ "distance: " <> show estimatedDistance
        let routeInfo = RouteInfo {distance = Just estimatedDistance, distanceWithUnit = Just $ convertMetersToDistance distanceUnit estimatedDistance, duration = Just estimatedDuration, points = Just serviceableRoute.routePoints}
        toLocation <- buildSearchReqLocation merchant.id merchantOpCityId sessiontoken sReq.dropAddrress sReq.customerLanguage dropLoc
        let setRouteInfo transactionId =
              ( do
                  Redis.setExp (searchRequestKey transactionId) routeInfo 3600
                  Redis.setExp (multipleRouteKey transactionId) (createMultipleRouteInfo <$> serviceableRoute.multipleRoutes) 3600
              )
        logDebug $ "Route serviceability: " <> show serviceableRoute.multipleRoutes
        mbTollChargesAndNames <- getTollInfoOnRoute merchantOpCityId Nothing serviceableRoute.routePoints
        return (Just setRouteInfo, Just toLocation, Just estimatedDistance, Just estimatedDuration, Just serviceableRoute.isCustomerPrefferedSearchRoute, Just serviceableRoute.isBlockedRoute, (\(charges, _, _) -> charges) <$> mbTollChargesAndNames, (\(_, names, _) -> names) <$> mbTollChargesAndNames, (\(_, _, isAutoRickshawAllowed) -> isAutoRickshawAllowed) <$> mbTollChargesAndNames)
      _ -> return (Nothing, Nothing, sReq.routeDistance, sReq.routeDuration, Nothing, Nothing, Nothing, Nothing, Nothing) -- estimate distance and durations by user
  allFarePoliciesProduct <- combineFarePoliciesProducts <$> ((getAllFarePoliciesProduct merchant.id merchantOpCityId sReq.isDashboardRequest sReq.pickupLocation sReq.dropLocation (Just (TransactionId (Id sReq.transactionId)))) `mapM` possibleTripOption.tripCategories)
  let farePolicies = selectFarePolicy (fromMaybe 0 mbDistance) (fromMaybe 0 mbDuration) mbIsAutoRickshawAllowed allFarePoliciesProduct.farePolicies
  now <- getCurrentTime
  (driverPool, selectedFarePolicies) <-
    if transporterConfig.considerDriversForSearch
      then do
        (pool, policies) <- selectDriversAndMatchFarePolicies merchant merchantOpCityId mbDistance fromLocation transporterConfig possibleTripOption.isScheduled allFarePoliciesProduct.area farePolicies now isValueAddNP
        pure (nonEmpty pool, policies)
      else return (Nothing, farePolicies)
  (mbSpecialZoneGateId, mbDefaultDriverExtra) <- getSpecialPickupZoneInfo allFarePoliciesProduct.specialLocationTag fromLocation
  logDebug $ "Pickingup Gate info result : " <> show (mbSpecialZoneGateId, mbDefaultDriverExtra)
  let specialLocationTag = maybe allFarePoliciesProduct.specialLocationTag (\_ -> allFarePoliciesProduct.specialLocationTag <&> (<> "_PickupZone")) mbSpecialZoneGateId
      specialLocationName = allFarePoliciesProduct.specialLocationName
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  searchReq <- buildSearchRequest sReq bapCity mbSpecialZoneGateId mbDefaultDriverExtra possibleTripOption.schedule possibleTripOption.isScheduled merchantId merchantOpCityId fromLocation mbToLocation mbDistance mbDuration specialLocationTag allFarePoliciesProduct.area mbTollCharges mbTollNames mbIsCustomerPrefferedSearchRoute mbIsBlockedRoute currency distanceUnit
  whenJust mbSetRouteInfo $ \setRouteInfo -> setRouteInfo sReq.transactionId
  triggerSearchEvent SearchEventData {searchRequest = searchReq, merchantId = merchantId}
  void $ QSR.createDSReq searchReq

  let buildEstimateHelper = buildEstimate merchantOpCityId currency distanceUnit (Just searchReq) possibleTripOption.schedule possibleTripOption.isScheduled sReq.returnTime sReq.roundTrip mbDistance specialLocationTag mbTollCharges mbTollNames mbIsCustomerPrefferedSearchRoute mbIsBlockedRoute searchReq.estimatedDuration
  let buildQuoteHelper = buildQuote merchantOpCityId searchReq merchantId possibleTripOption.schedule possibleTripOption.isScheduled sReq.returnTime sReq.roundTrip mbDistance mbDuration specialLocationTag mbTollCharges mbTollNames mbIsCustomerPrefferedSearchRoute mbIsBlockedRoute
  (estimates, quotes) <- foldrM (processPolicy buildEstimateHelper buildQuoteHelper) ([], []) selectedFarePolicies
  QEst.createMany estimates
  for_ quotes QQuote.create

  forM_ estimates $ \est -> triggerEstimateEvent EstimateEventData {estimate = est, merchantId = merchantId}
  driverInfoQuotes <- addNearestDriverInfo merchantOpCityId driverPool quotes
  driverInfoEstimates <- addNearestDriverInfo merchantOpCityId driverPool estimates
  buildDSearchResp sReq.pickupLocation sReq.dropLocation specialLocationTag searchMetricsMVar driverInfoQuotes driverInfoEstimates specialLocationName now
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
          specialLocationTag = (listToMaybe products) >>= (.specialLocationTag),
          specialLocationName = (listToMaybe products) >>= (.specialLocationName)
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
        DTC.CrossCity DTC.OneWayOnDemandDynamicOffer _ -> (buildEstimateHelper False) fp >>= \est -> pure (est : estimates, quotes)
        DTC.InterCity DTC.OneWayOnDemandDynamicOffer _ -> (buildEstimateHelper False) fp >>= \est -> pure (est : estimates, quotes)
        DTC.Rental _ -> (buildQuoteHelper True) fp >>= \quote -> pure (estimates, quote : quotes)
        DTC.InterCity _ _ -> (buildQuoteHelper True) fp >>= \quote -> pure (estimates, quote : quotes)
        DTC.Ambulance DTC.OneWayOnDemandDynamicOffer -> (buildEstimateHelper True) fp >>= \est -> pure (est : estimates, quotes)
        _ -> (buildQuoteHelper False) fp >>= \quote -> pure (estimates, quote : quotes)

    buildDSearchResp fromLocation toLocation specialLocationTag searchMetricsMVar quotes estimates specialLocationName now = do
      merchantPaymentMethods <- CQMPM.findAllByMerchantOpCityId merchantOpCityId
      let paymentMethodsInfo = DMPM.mkPaymentMethodInfo <$> merchantPaymentMethods
      return $
        DSearchRes
          { provider = merchant,
            bapId = sReq.bapId,
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

    mkBapMetaData :: Flow BapMetadata
    mkBapMetaData = do
      now <- getCurrentTime
      return $
        BapMetadata
          { id = Id sReq.bapId,
            domain = Just $ show Domain.MOBILITY,
            name = "THIRD PARTY BAP",
            logoUrl = Nothing, -- TODO: Parse this from on_search req
            createdAt = now,
            updatedAt = now
          }

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
      case driverPool' of
        Nothing -> return (input, vehicleServiceTierItem, Nothing)
        Just dp -> do
          let driverLatLongs = fmap (\x -> LatLong x.lat x.lon) dp
              distanceToNearestDriver = NE.head dp & (.distanceToPickup)
              locationId = NE.head dp & (.driverId) & (.getId)
              nearestDriverInfo = NearestDriverInfo {..}
          return (input, vehicleServiceTierItem, Just nearestDriverInfo)

selectDriversAndMatchFarePolicies :: DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Meters -> DLoc.Location -> DTMT.TransporterConfig -> Bool -> SL.Area -> [DFP.FullFarePolicy] -> UTCTime -> Bool -> Flow ([DriverPoolResult], [DFP.FullFarePolicy])
selectDriversAndMatchFarePolicies merchant merchantOpCityId mbDistance fromLocation transporterConfig isScheduled area farePolicies now isValueAddNP = do
  driverPoolCfg <- CDP.getSearchDriverPoolConfig merchantOpCityId mbDistance area
  cityServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId
  let calculateDriverPoolReq =
        CalculateDriverPoolReq
          { poolStage = Estimate,
            driverPoolCfg = fromJust driverPoolCfg,
            serviceTiers = [],
            pickup = fromLocation,
            merchantOperatingCityId = merchantOpCityId,
            merchantId = merchant.id,
            onlinePayment = merchant.onlinePayment,
            mRadiusStep = Nothing,
            isRental = False,
            isInterCity = False,
            ..
          }
  driverPoolNotOnRide <- calculateDriverPool calculateDriverPoolReq
  logDebug $ "Driver Pool not on ride " <> show driverPoolNotOnRide
  driverPoolCurrentlyOnRide <-
    if null driverPoolNotOnRide
      then do
        if transporterConfig.includeDriverCurrentlyOnRide && (fromJust driverPoolCfg).enableForwardBatching
          then snd <$> calculateDriverPoolCurrentlyOnRide calculateDriverPoolReq Nothing
          else pure []
      else pure []
  let driverPool =
        driverPoolNotOnRide
          <> map (\DriverPoolResultCurrentlyOnRide {..} -> DriverPoolResult {..}) driverPoolCurrentlyOnRide
  logDebug $ "Search handler: driver pool " <> show driverPool
  let onlyFPWithDrivers = filter (\fp -> (isScheduled || (skipDriverPoolCheck fp.tripCategory) || isJust (find (\dp -> dp.serviceTier == fp.vehicleServiceTier) driverPool)) && (isValueAddNP || fp.vehicleServiceTier `elem` DVST.offUsVariants)) farePolicies
  return (driverPool, onlyFPWithDrivers)

skipDriverPoolCheck :: DTC.TripCategory -> Bool
skipDriverPoolCheck (DTC.OneWay DTC.OneWayOnDemandStaticOffer) = False
skipDriverPoolCheck (DTC.OneWay DTC.OneWayOnDemandDynamicOffer) = False
skipDriverPoolCheck (DTC.Ambulance DTC.OneWayOnDemandDynamicOffer) = False
skipDriverPoolCheck _ = True

buildSearchRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
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
  DistanceUnit ->
  m DSR.SearchRequest
buildSearchRequest DSearchReq {..} bapCity mbSpecialZoneGateId mbDefaultDriverExtra startTime isScheduled providerId merchantOpCityId fromLocation mbToLocation mbDistance mbDuration specialLocationTag area tollCharges tollNames isCustomerPrefferedSearchRoute isBlockedRoute currency distanceUnit = do
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
        roundTrip = Just roundTrip,
        isAdvanceBookingEnabled = False,
        ..
      }

buildQuote ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  Id DMOC.MerchantOperatingCity ->
  DSR.SearchRequest ->
  Id DM.Merchant ->
  UTCTime ->
  Bool ->
  Maybe UTCTime ->
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
buildQuote merchantOpCityId searchRequest transporterId pickupTime isScheduled returnTime roundTrip mbDistance mbDuration specialLocationTag tollCharges tollNames isCustomerPrefferedSearchRoute isBlockedRoute nightShiftOverlapChecking fullFarePolicy = do
  let dist = fromMaybe 0 mbDistance
  fareParams <-
    calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = fullFarePolicy,
          actualDistance = Just dist,
          rideTime = pickupTime,
          returnTime,
          roundTrip,
          waitingTime = Nothing,
          actualRideDuration = Nothing,
          vehicleAge = Nothing,
          avgSpeedOfVehicle = Nothing,
          driverSelectedFare = Nothing,
          customerExtraFee = Nothing,
          nightShiftCharge = Nothing,
          customerCancellationDues = Nothing,
          nightShiftOverlapChecking = nightShiftOverlapChecking,
          estimatedDistance = searchRequest.estimatedDistance,
          estimatedRideDuration = searchRequest.estimatedDuration,
          rideDurationForFareCalc = secondsToMinutesCeiling <$> searchRequest.estimatedDuration,
          timeDiffFromUtc = Nothing,
          tollCharges = tollCharges,
          currency = searchRequest.currency,
          distanceUnit = searchRequest.distanceUnit
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
      isTollApplicable = DTC.isTollApplicableForTrip fullFarePolicy.vehicleServiceTier fullFarePolicy.tripCategory
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
        tollNames = if isTollApplicable then tollNames else Nothing,
        createdAt = now,
        updatedAt = now,
        currency = searchRequest.currency,
        distanceUnit = searchRequest.distanceUnit,
        ..
      }

buildEstimate ::
  (EsqDBFlow m r, CacheFlow m r, EsqDBReplicaFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  DistanceUnit ->
  Maybe DSR.SearchRequest ->
  UTCTime ->
  Bool ->
  Maybe UTCTime ->
  Bool ->
  Maybe Meters ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Seconds ->
  Bool ->
  DFP.FullFarePolicy ->
  m DEst.Estimate
buildEstimate merchantOpCityId currency distanceUnit mbSearchReq startTime isScheduled returnTime roundTrip mbDistance specialLocationTag tollCharges tollNames isCustomerPrefferedSearchRoute isBlockedRoute mbEstimatedDuration nightShiftOverlapChecking fullFarePolicy = do
  let dist = fromMaybe 0 mbDistance -- TODO: Fix Later
      isAmbulanceEstimate = DTC.isAmbulanceTrip fullFarePolicy.tripCategory
  (minFareParams, maxFareParams) <- do
    let params =
          CalculateFareParametersParams
            { farePolicy = fullFarePolicy,
              actualDistance = Just dist,
              rideTime = startTime,
              returnTime,
              roundTrip,
              waitingTime = Nothing,
              actualRideDuration = Nothing,
              vehicleAge = Nothing,
              avgSpeedOfVehicle = Nothing,
              driverSelectedFare = Nothing,
              customerExtraFee = Nothing,
              nightShiftCharge = Nothing,
              customerCancellationDues = Nothing,
              nightShiftOverlapChecking = nightShiftOverlapChecking,
              estimatedDistance = Nothing,
              estimatedRideDuration = Nothing,
              rideDurationForFareCalc = secondsToMinutesCeiling <$> mbEstimatedDuration,
              timeDiffFromUtc = Nothing,
              tollCharges = tollCharges,
              currency,
              distanceUnit
            }
    fareParamsMax <- calculateFareParameters params
    fareParamsMin <-
      if isAmbulanceEstimate
        then calculateFareParameters params {vehicleAge = Just 100000} -- high value
        else return fareParamsMax
    return (fareParamsMin, fareParamsMax)

  estimateId <- Id <$> generateGUID
  now <- getCurrentTime
  void $ cacheFarePolicyByEstimateId estimateId.getId fullFarePolicy
  let mbDriverExtraFeeBounds = DFP.findDriverExtraFeeBoundsByDistance dist <$> fullFarePolicy.driverExtraFeeBounds
      minFare = fareSum minFareParams + maybe 0.0 (.minFee) mbDriverExtraFeeBounds
      maxFare = fareSum maxFareParams + maybe 0.0 (.maxFee) mbDriverExtraFeeBounds
  vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityId fullFarePolicy.vehicleServiceTier merchantOpCityId >>= fromMaybeM (VehicleServiceTierNotFound (show fullFarePolicy.vehicleServiceTier))
  let isTollApplicable = DTC.isTollApplicableForTrip fullFarePolicy.vehicleServiceTier fullFarePolicy.tripCategory
  pure
    DEst.Estimate
      { id = estimateId,
        requestId = maybe (Id "") (.id) mbSearchReq,
        vehicleServiceTier = fullFarePolicy.vehicleServiceTier,
        vehicleServiceTierName = Just vehicleServiceTierItem.name,
        tripCategory = fullFarePolicy.tripCategory,
        estimatedDistance = mbDistance,
        currency,
        fareParams = Just maxFareParams, -- Todo: fix it
        farePolicy = Just $ DFP.fullFarePolicyToFarePolicy fullFarePolicy,
        specialLocationTag = specialLocationTag,
        isScheduled = isScheduled,
        tollNames = if isTollApplicable then tollNames else Nothing,
        createdAt = now,
        updatedAt = now,
        ..
      }

validateRequest :: Id DM.Merchant -> DSearchReq -> Flow ValidatedDSearchReq
validateRequest merchantId sReq = do
  isValueAddNP <- CQVAN.isValueAddNP sReq.bapId
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless merchant.enabled $ throwError (AgencyDisabled merchantId.getId)
  -- This checks for origin serviceability too
  NearestOperatingAndSourceCity {nearestOperatingCity, sourceCity} <- getNearestOperatingAndSourceCity merchant sReq.pickupLocation
  let bapCity = nearestOperatingCity.city
  merchantOpCity <- CQMOC.getMerchantOpCity merchant (Just bapCity)
  let (distanceUnit, merchantOpCityId) = (merchantOpCity.distanceUnit, merchantOpCity.id)
  transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId (Just (TransactionId (Id sReq.transactionId))) >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)
  (isInterCity, isCrossCity, destinationTravelCityName) <-
    case sReq.dropLocation of
      Just dropLoc -> do
        (destinationCityState, mbDestinationTravelCityName) <- getDestinationCity merchant dropLoc -- This checks for destination serviceability too
        if destinationCityState.city == sourceCity.city && destinationCityState.city /= Context.AnyCity
          then return (False, False, Nothing)
          else do
            mbMerchantState <- CQMS.findByMerchantIdAndState merchant.id sourceCity.state
            let allowedStates = maybe [sourceCity.state] (.allowedDestinationStates) mbMerchantState
            -- Destination states should be in the allowed states of the origin state
            if destinationCityState.state `elem` allowedStates
              then do
                if destinationCityState.city `elem` transporterConfig.crossTravelCities
                  then return (True, True, mbDestinationTravelCityName)
                  else return (True, False, mbDestinationTravelCityName)
              else throwError (RideNotServiceableInState $ show destinationCityState.state)
      Nothing -> pure (False, False, Nothing)

  now <- getCurrentTime
  let possibleTripOption = getPossibleTripOption now transporterConfig sReq isInterCity isCrossCity destinationTravelCityName
  return ValidatedDSearchReq {..}

getPossibleTripOption :: UTCTime -> DTMT.TransporterConfig -> DSearchReq -> Bool -> Bool -> Maybe Text -> DTC.TripOption
getPossibleTripOption now tConf dsReq isInterCity isCrossCity destinationTravelCityName = do
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
                    [DTC.CrossCity DTC.OneWayOnDemandStaticOffer destinationTravelCityName]
                      <> (if not isScheduled then [DTC.CrossCity DTC.OneWayRideOtp destinationTravelCityName, DTC.CrossCity DTC.OneWayOnDemandDynamicOffer destinationTravelCityName] else [])
                  else do
                    [DTC.InterCity DTC.OneWayOnDemandStaticOffer destinationTravelCityName]
                      <> (if not isScheduled then [DTC.InterCity DTC.OneWayRideOtp destinationTravelCityName, DTC.InterCity DTC.OneWayOnDemandDynamicOffer destinationTravelCityName] else [])
              else do
                [DTC.OneWay DTC.OneWayOnDemandStaticOffer, DTC.Rental DTC.OnDemandStaticOffer]
                  <> (if not isScheduled then [DTC.OneWay DTC.OneWayRideOtp, DTC.OneWay DTC.OneWayOnDemandDynamicOffer, DTC.Ambulance DTC.OneWayOnDemandDynamicOffer, DTC.Rental DTC.RideOtp] else [])
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

getDestinationCity :: DM.Merchant -> LatLong -> Flow (CityState, Maybe Text)
getDestinationCity merchant dropLatLong = do
  let geoRestriction = merchant.geofencingConfig.destination
  case geoRestriction of
    Unrestricted -> return (CityState {city = merchant.city, state = merchant.state}, Nothing)
    Regions regions -> do
      geoms <- B.runInReplica $ QGeometry.findGeometriesContaining dropLatLong regions
      case filter (\geom -> geom.city /= Context.AnyCity) geoms of
        [] ->
          find (\geom -> geom.city == Context.AnyCity) geoms & \case
            Just anyCityGeom -> do
              interTravelCities <- CQITC.findByMerchantIdAndState merchant.id anyCityGeom.state >>= mapM (\m -> return (distanceBetweenInMeters dropLatLong (LatLong m.lat m.lng), m.cityName))
              mbNearestCity <-
                if null interTravelCities
                  then do
                    operatingCities <- CQMOC.findAllByMerchantIdAndState merchant.id anyCityGeom.state >>= mapM (\m -> return (distanceBetweenInMeters dropLatLong m.location, show m.city))
                    return $ snd <$> (listToMaybe $ sortBy (comparing fst) operatingCities)
                  else do
                    intercityTravelAreas <- CQITC.findInterCityAreasContainingGps dropLatLong
                    return $ (.cityName) <$> listToMaybe intercityTravelAreas
              return (CityState {city = anyCityGeom.city, state = anyCityGeom.state}, mbNearestCity)
            Nothing -> do
              logError $ "No geometry found for dropLatLong: " <> show dropLatLong <> " for regions: " <> show regions
              throwError RideNotServiceable
        (g : _) -> return (CityState {city = g.city, state = g.state}, Just $ show g.city)

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
