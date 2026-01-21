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
    DSearchReqLocation (..),
    IsIntercityReq (..),
    IsIntercityResp (..),
    getNearestOperatingAndSourceCity,
    handler,
    validateRequest,
    buildEstimate,
    getIsInterCity,
  )
where

import qualified Beckn.Types.Core.Taxi.Search as BA
import Control.Applicative ((<|>))
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Geohash as Geohash
import Data.List (sortBy)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Ord
import qualified Data.Text as T
import qualified Domain.Action.Internal.Estimate as DBppEstimate
import qualified Domain.Action.UI.DemandHotspots as DemandHotspots
import qualified Domain.Action.UI.Maps as DMaps
import Domain.Types
import Domain.Types.BapMetadata
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Extra.ConditionalCharges as DAC
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RefereeLink as DRL
import Domain.Types.RideRoute
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.ServiceTierType as STT
import qualified Domain.Types.TransporterConfig as DTMT
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as DVST
import qualified Domain.Types.Yudhishthira as Y
import Environment
import EulerHS.Prelude ((+||), (||+))
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
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
import qualified Lib.Yudhishthira.Event as Yudhishthira
import Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as Yudhishthira
import SharedLogic.BlockedRouteDetector
import SharedLogic.DriverPool
import SharedLogic.FareCalculator
import qualified SharedLogic.FareCalculatorV2 as FCV2
import SharedLogic.FarePolicy
import SharedLogic.GoogleMaps
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.MerchantPaymentMethod as DMPM
import SharedLogic.Ride
import qualified SharedLogic.RiderDetails as SRD
import SharedLogic.TollsDetector
import Storage.Beam.Yudhishthira ()
import Storage.Cac.DriverPoolConfig as CDP
import qualified Storage.Cac.FarePolicy as QFPolicy
import Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData
import qualified Storage.CachedQueries.InterCityTravelCities as CQITC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantState as CQMS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.FareParameters as QFP
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.Vehicle as QVeh
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.DynamicLogic
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
    stops :: [DSearchReqLocation],
    customerLanguage :: Maybe Maps.Language,
    customerNammaTags :: Maybe [LYT.TagNameValue],
    isReallocationEnabled :: Maybe Bool,
    isMeterRideSearch :: Maybe Bool,
    fareParametersInRateCard :: Maybe Bool,
    disabilityTag :: Maybe Text,
    dropLocation :: Maybe LatLong,
    dropAddrress :: Maybe BA.Address,
    routeDistance :: Maybe Meters,
    routeDuration :: Maybe Seconds,
    routePoints :: Maybe [LatLong],
    isDashboardRequest :: Bool,
    multipleRoutes :: Maybe [Maps.RouteInfo],
    driverIdentifier :: Maybe DRL.DriverIdentifier,
    isMultimodalSearch :: Maybe Bool,
    isReserveRide :: Maybe Bool,
    mbAdditonalChargeCategories :: Maybe [DAC.ConditionalChargesCategories],
    reserveRideEstimate :: Maybe DBppEstimate.BppEstimate,
    numberOfLuggages :: Maybe Int,
    paymentMode :: Maybe DMPM.PaymentMode,
    fromSpecialLocationId :: Maybe (Id SL.SpecialLocation), -- Fixed route: from area ID
    toSpecialLocationId :: Maybe (Id SL.SpecialLocation) -- Fixed route: to area ID
  }

-- data EstimateExtraInfo = EstimateExtraInfo
--   { congestionMultiplier :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
--     currency :: Kernel.Types.Common.Currency,
--     dpVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
--     eligibleForUpgrade :: Kernel.Prelude.Bool,
--     fareParams :: Kernel.Prelude.Maybe Domain.Types.FareParameters.FareParameters,
--     farePolicy :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.FarePolicy,
--     fromLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
--     isScheduled :: Kernel.Prelude.Bool,
--     tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text]
--   }

data DSearchReqLocation = DSearchReqLocation
  { address :: Maybe BA.Address,
    gps :: LatLong
  }

data ValidatedDSearchReq = ValidatedDSearchReq
  { transporterConfig :: DTMT.TransporterConfig,
    possibleTripOption :: TripOption,
    bapCity :: Context.City,
    merchantOpCityId :: Id DMOC.MerchantOperatingCity,
    cityDistanceUnit :: DistanceUnit,
    merchant :: DM.Merchant,
    isValueAddNP :: Bool,
    driverIdForSearch :: Maybe (Id DP.Person),
    isMeterRideSearch :: Maybe Bool,
    numberOfLuggages :: Maybe Int,
    isReserveRide :: Maybe Bool,
    reserveRideEstimate :: Maybe DBppEstimate.BppEstimate
  }

data DSearchRes = DSearchRes
  { specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text,
    searchMetricsMVar :: Metrics.SearchMetricsMVar,
    paymentMethodsInfo :: [DMPM.PaymentMethodInfo],
    provider :: DM.Merchant,
    fromLocation :: LatLong,
    toLocation :: Maybe LatLong,
    stops :: [LatLong],
    now :: UTCTime,
    quotes :: [(DQuote.Quote, DVST.VehicleServiceTier, Maybe NearestDriverInfo, Maybe BaseUrl)],
    estimates :: [(DEst.Estimate, DVST.VehicleServiceTier, Maybe NearestDriverInfo, Maybe BaseUrl)],
    transporterConfig :: DTMT.TransporterConfig,
    bapId :: Text,
    fareParametersInRateCard :: Maybe Bool,
    isMultimodalSearch :: Maybe Bool
  }

data NearestDriverInfo = NearestDriverInfo
  { locationId :: Text,
    distanceToNearestDriver :: Meters,
    driverLatLongs :: NonEmpty LatLong
  }
  deriving (Generic, Show)

getRouteServiceability :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DistanceUnit -> LatLong -> LatLong -> Maybe [LatLong] -> Maybe Meters -> Maybe Seconds -> Maybe [Maps.RouteInfo] -> Text -> Flow RouteServiceability
getRouteServiceability _ merchantOpCityId _ _ _ (Just routePoints) (Just distance) (Just duration) (Just multipleRoutes) _ = do
  checkRouteServiceability merchantOpCityId (0, routePoints, distance, duration) multipleRoutes
getRouteServiceability _ merchantOpCityId _ _ _ (Just routePoints) (Just distance) (Just duration) Nothing _ = do
  checkRouteServiceability merchantOpCityId (0, routePoints, distance, duration) []
getRouteServiceability merchantId merchantOpCityId _distanceUnit fromLocation toLocation _ _ _ _ transactionId = do
  -- as of now this case only happens for off-us BAPs
  responses <- Maps.getRoutes merchantId merchantOpCityId (Just transactionId) Maps.GetRoutesReq {waypoints = NE.fromList [fromLocation, toLocation], mode = Just Maps.CAR, calcPoints = True}
  if null responses
    then do
      logWarning $ "No route found for transactionId: " <> transactionId
      checkRouteServiceability merchantOpCityId (0, [], 0, 0) []
    else do
      transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId (Just (TransactionId (Id transactionId))) >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)
      let distanceWeightage = fromMaybe 20 transporterConfig.distanceWeightage
          durationWeightage = 100 - distanceWeightage
          (mbShortestRoute, index) = Maps.getEfficientRouteInfo responses distanceWeightage durationWeightage
          routeDistance = fromMaybe 0 (mbShortestRoute >>= (.distance))
          routeDuration = fromMaybe 0 (mbShortestRoute >>= (.duration))
          routePoints = maybe [] (.points) mbShortestRoute
      when (isNothing mbShortestRoute) $ do
        -- this case should never happen as we filter out null responses
        logWarning $ "Efficient route selection returned Nothing for transactionId: " <> transactionId
      checkRouteServiceability merchantOpCityId (index, routePoints, routeDistance, routeDuration) responses

handler :: ValidatedDSearchReq -> DSearchReq -> Flow DSearchRes
handler ValidatedDSearchReq {..} sReq = do
  bapMetadata <- mkBapMetaData
  CQBapMetaData.createIfNotPresent bapMetadata (Id sReq.bapId) (show Domain.MOBILITY)
  searchMetricsMVar <- Metrics.startSearchMetrics merchant.name
  let merchantId' = merchant.id
  sessiontoken <- generateGUIDText
  let fromLocGeohashh = T.pack <$> Geohash.encode (fromMaybe 5 transporterConfig.dpGeoHashPercision) (sReq.pickupLocation.lat, sReq.pickupLocation.lon)
  let toLocGeohash = join $ fmap (\(LatLong lat lng) -> T.pack <$> Geohash.encode (fromMaybe 5 transporterConfig.dpGeoHashPercision) (lat, lng)) sReq.dropLocation
  fromLocation <- buildSearchReqLocation merchant.id merchantOpCityId sessiontoken sReq.pickupAddress sReq.customerLanguage sReq.pickupLocation
  stops <- mapM (\stop -> buildSearchReqLocation merchant.id merchantOpCityId sessiontoken stop.address sReq.customerLanguage stop.gps) sReq.stops
  (mbSetRouteInfo, mbToLocation, mbDistance, mbDuration, mbIsCustomerPrefferedSearchRoute, mbIsBlockedRoute, mbTollCharges, mbTollNames, mbTollIds, mbIsAutoRickshawAllowed, mbIsTwoWheelerAllowed) <-
    case (sReq.dropLocation, sReq.fromSpecialLocationId, sReq.toSpecialLocationId) of
      (Just dropLoc, Nothing, Nothing) -> do
        serviceableRoute <- getRouteServiceability merchant.id merchantOpCityId cityDistanceUnit sReq.pickupLocation dropLoc sReq.routePoints sReq.routeDistance sReq.routeDuration sReq.multipleRoutes sReq.transactionId
        let estimatedDistance = serviceableRoute.routeDistance
            estimatedDuration = serviceableRoute.routeDuration
        logDebug $ "distance: " <> show estimatedDistance
        let routeInfo = RouteInfo {distance = Just estimatedDistance, distanceWithUnit = Just $ convertMetersToDistance cityDistanceUnit estimatedDistance, duration = Just estimatedDuration, points = Just serviceableRoute.routePoints}
        --------------build stops locations ---------------
        toLocation <- buildSearchReqLocation merchant.id merchantOpCityId sessiontoken sReq.dropAddrress sReq.customerLanguage dropLoc
        let setRouteInfo transactionId =
              ( do
                  Redis.setExp (searchRequestKey transactionId) routeInfo 3600
                  Redis.setExp (multipleRouteKey transactionId) (createMultipleRouteInfo <$> serviceableRoute.multipleRoutes) 3600
              )
        logDebug $ "Route serviceability: " <> show serviceableRoute.multipleRoutes
        mbTollChargesAndNames <- getTollInfoOnRoute merchantOpCityId Nothing serviceableRoute.routePoints
        return
          ( Just setRouteInfo,
            Just toLocation,
            Just estimatedDistance,
            Just estimatedDuration,
            Just serviceableRoute.isCustomerPrefferedSearchRoute,
            Just serviceableRoute.isBlockedRoute,
            (\(charges, _, _, _, _) -> charges) <$> mbTollChargesAndNames,
            (\(_, names, _, _, _) -> names) <$> mbTollChargesAndNames,
            (\(_, _, ids, _, _) -> ids) <$> mbTollChargesAndNames,
            (\(_, _, _, isAutoRickshawAllowed, _) -> isAutoRickshawAllowed) <$> mbTollChargesAndNames,
            join ((\(_, _, _, _, isTwoWheelerAllowed) -> isTwoWheelerAllowed) <$> mbTollChargesAndNames)
          )
      (Just dropLoc, Just _, Just _) -> do
        toLocation <- buildSearchReqLocation merchant.id merchantOpCityId sessiontoken sReq.dropAddrress sReq.customerLanguage dropLoc
        return (Nothing, Just toLocation, sReq.routeDistance, sReq.routeDuration, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
      _ -> return (Nothing, Nothing, sReq.routeDistance, sReq.routeDuration, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) -- estimate distance and durations by user
  let localTimeZoneSeconds = transporterConfig.timeDiffFromUtc
  localTime <- getLocalCurrentTime localTimeZoneSeconds
  configVersionMap <- getConfigVersionMapForStickiness (cast merchantOpCityId)
  (_, mbVersion) <- getAppDynamicLogic (cast merchantOpCityId) LYT.DYNAMIC_PRICING_UNIFIED localTime Nothing Nothing
  allFarePoliciesProduct <- combineFarePoliciesProducts <$> (mapM (\tripCategory -> getAllFarePoliciesProduct merchant.id merchantOpCityId sReq.isDashboardRequest sReq.pickupLocation sReq.dropLocation sReq.fromSpecialLocationId sReq.toSpecialLocationId (Just (TransactionId (Id sReq.transactionId))) fromLocGeohashh toLocGeohash mbDistance mbDuration mbVersion tripCategory configVersionMap) possibleTripOption.tripCategories)
  mbVehicleServiceTier <- getVehicleServiceTierForMeterRideSearch isMeterRideSearch driverIdForSearch configVersionMap
  let farePolicies = selectFarePolicy (fromMaybe 0 mbDistance) (fromMaybe 0 mbDuration) mbIsAutoRickshawAllowed mbIsTwoWheelerAllowed mbVehicleServiceTier allFarePoliciesProduct.farePolicies
  now <- getCurrentTime
  (mbSpecialZoneGateId, mbDefaultDriverExtra) <- getSpecialPickupZoneInfo allFarePoliciesProduct.specialLocationTag fromLocation
  logDebug $ "Pickingup Gate info result : " <> show (mbSpecialZoneGateId, mbDefaultDriverExtra)
  let spcllocationTag = maybe allFarePoliciesProduct.specialLocationTag (\_ -> allFarePoliciesProduct.specialLocationTag <&> (<> "_PickupZone")) mbSpecialZoneGateId
      specialLocationName = allFarePoliciesProduct.specialLocationName
  cityCurrency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  customerCancellationDue <-
    if transporterConfig.canAddCancellationFee && sReq.isMultimodalSearch == Just False
      then do
        case sReq.customerPhoneNum of
          Just number -> do
            (riderDetails, isNewRider) <- SRD.getRiderDetails cityCurrency merchant.id (Just merchantOpCityId) (fromMaybe "+91" merchant.mobileCountryCode) number sReq.bapId False
            when isNewRider $ QRD.create riderDetails
            return riderDetails.cancellationDues
          Nothing -> do
            logWarning "Failed to calculate Customer Cancellation Dues as BAP Phone Number is NULL"
            return 0
      else return 0
  let mbDriverInfo = driverIdForSearch
  searchReq <- buildSearchRequest sReq bapCity mbSpecialZoneGateId mbDefaultDriverExtra possibleTripOption.schedule possibleTripOption.isScheduled merchantId' merchantOpCityId customerCancellationDue fromLocation mbToLocation mbDistance mbDuration spcllocationTag allFarePoliciesProduct.area mbTollCharges mbTollNames mbTollIds mbIsCustomerPrefferedSearchRoute mbIsBlockedRoute cityCurrency cityDistanceUnit fromLocGeohashh toLocGeohash mbVersion stops mbDriverInfo configVersionMap
  whenJust mbSetRouteInfo $ \setRouteInfo -> setRouteInfo sReq.transactionId
  triggerSearchEvent SearchEventData {searchRequest = searchReq, merchantId = merchantId'}
  void $ QSR.createDSReq searchReq

  fork "Add Namma Tags" $ do
    let tagData =
          Y.TagData
            { searchRequest = searchReq,
              area = show allFarePoliciesProduct.area,
              specialLocationTag = spcllocationTag,
              specialLocationName = allFarePoliciesProduct.specialLocationName
            }
    addNammaTags tagData
  fork "Updating Demand Hotspots on search" $ do
    DemandHotspots.updateDemandHotspotsOnSearch searchReq.id merchantOpCityId transporterConfig sReq.pickupLocation

  (driverPool, selectedFarePolicies) <-
    if transporterConfig.considerDriversForSearch
      then do
        (pool, policies) <- selectDriversAndMatchFarePolicies merchant merchantOpCityId mbDistance fromLocation transporterConfig possibleTripOption.isScheduled allFarePoliciesProduct.area farePolicies now isValueAddNP searchReq sReq.paymentMode
        pure (nonEmpty pool, policies)
      else return (Nothing, farePolicies)
  -- This is to filter the fare policies based on the driverId, if passed during search
  -- (driverPool, selectedFarePolicies) <- maybe (pure (driverPool', selectedFarePolicies')) (filterFPsForDriverId (driverPool', selectedFarePolicies')) searchReq.driverIdForSearch
  let buildEstimateHelper = buildEstimate merchantId' merchantOpCityId cityCurrency cityDistanceUnit (Just searchReq) possibleTripOption.schedule possibleTripOption.isScheduled sReq.returnTime sReq.roundTrip mbDistance spcllocationTag mbTollCharges mbTollNames mbTollIds mbIsCustomerPrefferedSearchRoute mbIsBlockedRoute (length stops) searchReq.estimatedDuration
  let buildQuoteHelper = buildQuote merchantOpCityId searchReq merchantId' possibleTripOption.schedule possibleTripOption.isScheduled sReq.returnTime sReq.roundTrip mbDistance mbDuration spcllocationTag mbTollCharges mbTollNames mbTollIds mbIsCustomerPrefferedSearchRoute mbIsBlockedRoute
  (estimates', quotes) <- foldrM (\fp acc -> processPolicy buildEstimateHelper buildQuoteHelper fp configVersionMap acc) ([], []) selectedFarePolicies

  let mbAutoMaxFare = find (\est -> est.vehicleServiceTier == AUTO_RICKSHAW) estimates' <&> (.maxFare)
  estimates <-
    if isNothing reserveRideEstimate
      then do
        return $ maybe estimates' (\_ -> map (\DEst.Estimate {..} -> DEst.Estimate {eligibleForUpgrade = False, ..}) estimates') mbAutoMaxFare
      else do
        est <- transformReserveRideEsttoEst (fromJust reserveRideEstimate)
        return [est]

  QEst.createMany estimates
  for_ quotes QQuote.create

  forM_ estimates $ \est -> triggerEstimateEvent EstimateEventData {estimate = est, merchantId = merchantId'}
  driverInfoQuotes <- addNearestDriverInfo merchantOpCityId driverPool quotes configVersionMap
  driverInfoEstimates <- addNearestDriverInfo merchantOpCityId driverPool estimates configVersionMap
  buildDSearchResp sReq.pickupLocation sReq.dropLocation (stopsLatLong sReq.stops) spcllocationTag searchMetricsMVar driverInfoQuotes driverInfoEstimates specialLocationName now sReq.fareParametersInRateCard sReq.isMultimodalSearch
  where
    stopsLatLong = map (.gps)
    getSpecialPickupZoneInfo :: Maybe Text -> DLoc.Location -> Flow (Maybe Text, Maybe HighPrecMoney)
    getSpecialPickupZoneInfo Nothing _ = pure (Nothing, Nothing)
    getSpecialPickupZoneInfo (Just _) fromLocation = do
      mbPickupZone <- Esq.runInReplica $ findGateInfoByLatLongWithoutGeoJson (LatLong fromLocation.lat fromLocation.lon)
      if ((.canQueueUpOnGate) <$> mbPickupZone) == Just True
        then pure $ ((.id.getId) <$> mbPickupZone, fmap (toHighPrecMoney . Money) . (.defaultDriverExtra) =<< mbPickupZone) -- FIXME
        else pure (Nothing, Nothing)

    combineFarePoliciesProducts :: [FarePoliciesProduct] -> FarePoliciesProduct
    combineFarePoliciesProducts products =
      FarePoliciesProduct
        { farePolicies = concatMap farePolicies products,
          area = maybe SL.Default (.area) $ listToMaybe products,
          specialLocationTag = listToMaybe products >>= (.specialLocationTag),
          specialLocationName = listToMaybe products >>= (.specialLocationName)
        }

    processPolicy ::
      (Bool -> DVST.VehicleServiceTier -> DFP.FullFarePolicy -> Flow DEst.Estimate) ->
      (Bool -> DVST.VehicleServiceTier -> DFP.FullFarePolicy -> Flow DQuote.Quote) ->
      DFP.FullFarePolicy ->
      [LYT.ConfigVersionMap] ->
      ([DEst.Estimate], [DQuote.Quote]) ->
      Flow ([DEst.Estimate], [DQuote.Quote])
    processPolicy buildEstimateHelper buildQuoteHelper fp configVersionMap (estimates, quotes) = do
      mbVehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow fp.vehicleServiceTier merchantOpCityId configVersionMap
      case mbVehicleServiceTierItem of
        Just vehicleServiceTierItem ->
          case tripCategoryToPricingPolicy fp.tripCategory of
            EstimateBased {..} -> buildEstimateHelper nightShiftOverlapChecking vehicleServiceTierItem fp >>= \est -> pure (est : estimates, quotes)
            QuoteBased {..} -> buildQuoteHelper nightShiftOverlapChecking vehicleServiceTierItem fp >>= \quote -> pure (estimates, quote : quotes)
        Nothing -> do
          logError $ "Vehicle service tier not found for " <> show fp.vehicleServiceTier
          pure (estimates, quotes)

    buildDSearchResp fromLocation toLocation stops specialLocationTag searchMetricsMVar quotes estimates specialLocationName now fareParametersInRateCard isMultimodalSearch = do
      merchantPaymentMethods <- CQMPM.findAllByMerchantOpCityId merchantOpCityId
      let paymentMethodsInfo = DMPM.mkPaymentMethodInfo <$> merchantPaymentMethods
      return $
        DSearchRes
          { provider = merchant,
            bapId = sReq.bapId,
            ..
          }

    selectFarePolicy distance duration mbIsAutoRickshawAllowed mbIsTwoWheelerAllowed mbVehicleServiceTier =
      filter (\farePolicy -> isValid farePolicy mbVehicleServiceTier)
      where
        isValid farePolicy Nothing = checkDistanceBounds farePolicy && checkExtendUpto farePolicy && vehicleAllowedOnTollRoute farePolicy
        isValid farePolicy (Just vehicleServiceTier) = farePolicy.vehicleServiceTier == vehicleServiceTier && checkDistanceBounds farePolicy && checkExtendUpto farePolicy && vehicleAllowedOnTollRoute farePolicy

        vehicleAllowedOnTollRoute farePolicy = case farePolicy.vehicleServiceTier of
          AUTO_RICKSHAW -> fromMaybe True mbIsAutoRickshawAllowed
          BIKE -> fromMaybe True mbIsTwoWheelerAllowed
          _ -> True

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

    addNammaTags :: Y.TagData -> Flow ()
    addNammaTags tagData = do
      newSearchTags <- withTryCatch "computeNammaTags:Search" (Yudhishthira.computeNammaTags Yudhishthira.Search tagData)
      let tags = tagData.searchRequest.searchTags <> eitherToMaybe newSearchTags
      QSR.updateSearchTags tags tagData.searchRequest.id

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

    getVehicleServiceTierForMeterRideSearch :: Maybe Bool -> Maybe (Id DP.Person) -> [LYT.ConfigVersionMap] -> Flow (Maybe STT.ServiceTierType)
    getVehicleServiceTierForMeterRideSearch (Just True) (Just driverId) configVersionMap = do
      QVehicle.findById driverId >>= \case
        Just vehicle -> do
          mbVst <- CQVST.findByServiceTierTypeAndCityIdInRideFlow (DVST.castVariantToServiceTier vehicle.variant) merchantOpCityId configVersionMap
          pure $ mbVst <&> (.serviceTierType)
        Nothing -> pure Nothing
    getVehicleServiceTierForMeterRideSearch _ _ _ = pure Nothing

addNearestDriverInfo ::
  (HasField "vehicleServiceTier" a ServiceTierType) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe (NonEmpty DriverPoolResult) ->
  [a] ->
  [LYT.ConfigVersionMap] ->
  Flow [(a, DVST.VehicleServiceTier, Maybe NearestDriverInfo, Maybe BaseUrl)]
addNearestDriverInfo merchantOpCityId Nothing estdOrQuotes configInExperimentVersions = do
  forM estdOrQuotes $ \estdOrQuote -> do
    vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow estdOrQuote.vehicleServiceTier merchantOpCityId configInExperimentVersions >>= fromMaybeM (VehicleServiceTierNotFound (show estdOrQuote.vehicleServiceTier))
    return (estdOrQuote, vehicleServiceTierItem, Nothing, vehicleServiceTierItem.vehicleIconUrl)
addNearestDriverInfo merchantOpCityId (Just driverPool) estdOrQuotes configInExperimentVersions = do
  let mapOfDPRByServiceTier = foldl (\m dpr -> M.insertWith (<>) dpr.serviceTier (pure dpr) m) mempty driverPool
  traverse (matchInputWithNearestDriver mapOfDPRByServiceTier) estdOrQuotes
  where
    matchInputWithNearestDriver ::
      (HasField "vehicleServiceTier" a ServiceTierType) =>
      M.Map ServiceTierType (NonEmpty DriverPoolResult) ->
      a ->
      Flow (a, DVST.VehicleServiceTier, Maybe NearestDriverInfo, Maybe BaseUrl)
    matchInputWithNearestDriver driverPools input = do
      vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow input.vehicleServiceTier merchantOpCityId configInExperimentVersions >>= fromMaybeM (VehicleServiceTierNotFound (show input.vehicleServiceTier))
      let driverPool' = M.lookup input.vehicleServiceTier driverPools
      case driverPool' of
        Nothing -> return (input, vehicleServiceTierItem, Nothing, vehicleServiceTierItem.vehicleIconUrl)
        Just dp -> do
          let driverLatLongs = fmap (\x -> LatLong x.lat x.lon) dp
              distanceToNearestDriver = NE.head dp & (.distanceToPickup)
              locationId = NE.head dp & (.driverId) & (.getId)
              nearestDriverInfo = NearestDriverInfo {..}
          return (input, vehicleServiceTierItem, Just nearestDriverInfo, vehicleServiceTierItem.vehicleIconUrl)

selectDriversAndMatchFarePolicies :: DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Meters -> DLoc.Location -> DTMT.TransporterConfig -> Bool -> SL.Area -> [DFP.FullFarePolicy] -> UTCTime -> Bool -> DSR.SearchRequest -> Maybe DMPM.PaymentMode -> Flow ([DriverPoolResult], [DFP.FullFarePolicy])
selectDriversAndMatchFarePolicies merchant merchantOpCityId mbDistance fromLocation transporterConfig isScheduled area farePolicies now isValueAddNP sreq paymentMode = do
  driverPoolCfg <- CDP.getSearchDriverPoolConfig merchantOpCityId mbDistance area sreq
  cityServiceTiers <- CQVST.findAllByMerchantOpCityIdInRideFlow merchantOpCityId sreq.configInExperimentVersions
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
            rideFare = Nothing,
            paymentInstrument = Nothing,
            prepaidSubscriptionAndWalletEnabled = False,
            ..
          }
  (driverPoolNotOnRide, _) <- calculateDriverPool calculateDriverPoolReq
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
          <> map (\DriverPoolResultCurrentlyOnRide {..} -> DriverPoolResult {customerTags = Nothing, ..}) driverPoolCurrentlyOnRide
  logDebug $ "Search handler: driver pool " <> show driverPool
  let onlyFPWithDrivers = filter (\fp -> (isScheduled || (skipDriverPoolCheck fp.tripCategory) || isJust (find (\dp -> dp.serviceTier == fp.vehicleServiceTier) driverPool)) && (isValueAddNP || fp.vehicleServiceTier `elem` offUsVariants)) farePolicies
  return (driverPool, onlyFPWithDrivers)

buildSearchRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "searchRequestExpirationSecondsForMultimodal" r NominalDiffTime
  ) =>
  DSearchReq ->
  Context.City ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  UTCTime ->
  Bool ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  HighPrecMoney ->
  DLoc.Location ->
  Maybe DLoc.Location ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Text ->
  SL.Area ->
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe [Text] ->
  Maybe Bool ->
  Maybe Bool ->
  Currency ->
  DistanceUnit ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  [DLoc.Location] ->
  Maybe (Id DP.Person) ->
  [ConfigVersionMap] ->
  m DSR.SearchRequest
buildSearchRequest DSearchReq {..} bapCity mbSpecialZoneGateId mbDefaultDriverExtra startTime isScheduled providerId merchantOpCityId cancellationDues fromLocation mbToLocation mbDistance mbDuration specialLocationTag area tollCharges tollNames tollIds isCustomerPrefferedSearchRoute isBlockedRoute currency distanceUnit fromLocGeohash toLocGeohash dynamicPricingLogicVersion stops' mbDriverInfo configVersionMap = do
  uuid <- generateGUID
  now <- getCurrentTime
  validTill <-
    case isMultimodalSearch of
      Just True -> do
        searchRequestExpirationSecondsForMultimodal <- asks (.searchRequestExpirationSecondsForMultimodal)
        return $ searchRequestExpirationSecondsForMultimodal `addUTCTime` startTime
      _ -> do
        searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
        return $ searchRequestExpirationSeconds `addUTCTime` startTime
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
        customerCancellationDues = Just cancellationDues,
        currency,
        roundTrip = Just roundTrip,
        isAdvanceBookingEnabled = False,
        searchTags = Nothing,
        tripCategory = Nothing,
        poolingLogicVersion = Nothing,
        poolingConfigVersion = Nothing,
        stops = stops',
        hasStops = Just . not $ null stops',
        driverIdForSearch = mbDriverInfo,
        configInExperimentVersions = configVersionMap,
        parcelType = Nothing,
        parcelQuantity = Nothing,
        preferSafetyPlus = False,
        numberOfLuggages = numberOfLuggages,
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
  Maybe [Text] ->
  Maybe Bool ->
  Maybe Bool ->
  Bool ->
  DVST.VehicleServiceTier ->
  DFP.FullFarePolicy ->
  m DQuote.Quote
buildQuote merchantOpCityId searchRequest transporterId pickupTime isScheduled returnTime roundTrip mbDistance mbDuration specialLocationTag tollCharges tollNames _tollIds isCustomerPrefferedSearchRoute isBlockedRoute nightShiftOverlapChecking vehicleServiceTierItem fullFarePolicy = do
  let dist = fromMaybe 0 mbDistance
  fareParams <-
    FCV2.calculateFareParametersV2
      CalculateFareParametersParams
        { farePolicy = fullFarePolicy,
          actualDistance = Just dist,
          rideTime = pickupTime,
          returnTime,
          roundTrip,
          waitingTime = Nothing,
          stopWaitingTimes = [],
          actualRideDuration = Nothing,
          vehicleAge = Nothing,
          driverSelectedFare = Nothing,
          customerExtraFee = Nothing,
          petCharges = Nothing,
          nightShiftCharge = Nothing,
          estimatedCongestionCharge = Nothing,
          customerCancellationDues = searchRequest.customerCancellationDues,
          nightShiftOverlapChecking = nightShiftOverlapChecking,
          estimatedDistance = searchRequest.estimatedDistance,
          estimatedRideDuration = searchRequest.estimatedDuration,
          timeDiffFromUtc = Nothing,
          tollCharges = tollCharges,
          currency = searchRequest.currency,
          noOfStops = length searchRequest.stops,
          shouldApplyBusinessDiscount = False,
          shouldApplyPersonalDiscount = True,
          distanceUnit = searchRequest.distanceUnit,
          merchantOperatingCityId = Just merchantOpCityId,
          mbAdditonalChargeCategories = Nothing,
          numberOfLuggages = searchRequest.numberOfLuggages
        }
  quoteId <- Id <$> generateGUID
  void $ cacheFarePolicyByQuoteId quoteId.getId fullFarePolicy
  now <- getCurrentTime
  let estimatedFare = fareSum fareParams (Just [])
      estimatedFinishTime = (\duration -> fromIntegral duration `addUTCTime` now) <$> mbDuration
  -- Keeping quote expiry as search request expiry. Slack discussion: https://juspay.slack.com/archives/C0139KHBFU1/p1683349807003679
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill = searchRequestExpirationSeconds `addUTCTime` now
      isTollApplicable = isTollApplicableForTrip fullFarePolicy.vehicleServiceTier fullFarePolicy.tripCategory
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
        merchantOperatingCityId = Just merchantOpCityId,
        ..
      }

buildEstimate ::
  (EsqDBFlow m r, CacheFlow m r, EsqDBReplicaFlow m r) =>
  Id DM.Merchant ->
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
  Maybe [Text] ->
  Maybe Bool ->
  Maybe Bool ->
  Int ->
  Maybe Seconds ->
  Bool ->
  DVST.VehicleServiceTier ->
  DFP.FullFarePolicy ->
  m DEst.Estimate
buildEstimate merchantId merchantOperatingCityId currency distanceUnit mbSearchReq startTime isScheduled returnTime roundTrip mbDistance specialLocationTag tollCharges tollNames tollIds isCustomerPrefferedSearchRoute isBlockedRoute noOfStops mbEstimatedDuration nightShiftOverlapChecking vehicleServiceTierItem fullFarePolicy = do
  let dist = fromMaybe 0 mbDistance -- TODO: Fix Later
      isAmbulanceEstimate = isAmbulanceTrip fullFarePolicy.tripCategory
  (minFareParams, maxFareParams) <- do
    let params =
          CalculateFareParametersParams
            { farePolicy = fullFarePolicy,
              actualDistance = Just dist,
              rideTime = startTime,
              returnTime,
              roundTrip,
              waitingTime = Nothing,
              stopWaitingTimes = [],
              actualRideDuration = Nothing,
              vehicleAge = Nothing,
              driverSelectedFare = Nothing,
              customerExtraFee = Nothing,
              petCharges = Nothing,
              nightShiftCharge = Nothing,
              customerCancellationDues = mbSearchReq >>= (.customerCancellationDues),
              estimatedCongestionCharge = Nothing,
              nightShiftOverlapChecking = nightShiftOverlapChecking,
              estimatedDistance = Nothing,
              estimatedRideDuration = mbEstimatedDuration,
              timeDiffFromUtc = Nothing,
              tollCharges = tollCharges,
              noOfStops,
              currency,
              distanceUnit,
              shouldApplyBusinessDiscount = False,
              shouldApplyPersonalDiscount = False,
              merchantOperatingCityId = Just merchantOperatingCityId,
              mbAdditonalChargeCategories = Nothing,
              numberOfLuggages = mbSearchReq >>= (.numberOfLuggages)
            }
    fareParamsMax <- FCV2.calculateFareParametersV2 params
    fareParamsMin <-
      if isAmbulanceEstimate
        then FCV2.calculateFareParametersV2 params {vehicleAge = Just 100000} -- high value
        else return fareParamsMax
    return (fareParamsMin, fareParamsMax)
  let businessDiscount = if isJust fullFarePolicy.businessDiscountPercentage then calculateBusinessDiscount maxFareParams (fromMaybe 0.0 fullFarePolicy.businessDiscountPercentage) else Nothing
  let personalDiscount = if isJust fullFarePolicy.personalDiscountPercentage then calculateBusinessDiscount maxFareParams (fromMaybe 0.0 fullFarePolicy.personalDiscountPercentage) else Nothing
  estimateId <- Id <$> generateGUID
  now <- getCurrentTime
  void $ cacheFarePolicyByEstimateId estimateId.getId fullFarePolicy
  commissionCharges <- FCV2.calculateCommission minFareParams (Just fullFarePolicy)
  let pickupChargesMaxx = case fullFarePolicy.farePolicyDetails of
        DFP.ProgressiveDetails progressiveDetails ->
          if progressiveDetails.pickupCharges.pickupChargesMin == progressiveDetails.pickupCharges.pickupChargesMax then 0 else progressiveDetails.pickupCharges.pickupChargesMax - progressiveDetails.pickupCharges.pickupChargesMin
        _ -> 0
  let mbDriverExtraFeeBounds = DFP.findDriverExtraFeeBoundsByDistance dist <$> fullFarePolicy.driverExtraFeeBounds
      minFare = fareSum minFareParams (Just []) + maybe 0.0 (.minFee) mbDriverExtraFeeBounds
      maxFare = fareSum maxFareParams (Just []) + maybe 0.0 (.maxFee) mbDriverExtraFeeBounds + pickupChargesMaxx
  let isTollApplicable = isTollApplicableForTrip fullFarePolicy.vehicleServiceTier fullFarePolicy.tripCategory
  pure
    DEst.Estimate
      { id = estimateId,
        requestId = maybe (Id "") (.id) mbSearchReq,
        vehicleServiceTier = fullFarePolicy.vehicleServiceTier,
        vehicleServiceTierName = Just vehicleServiceTierItem.name,
        driverExtraFeeBounds = mbDriverExtraFeeBounds,
        tripCategory = fullFarePolicy.tripCategory,
        estimatedDistance = mbDistance,
        estimatedDuration = maybe Nothing (.estimatedDuration) mbSearchReq,
        fromLocGeohash = maybe Nothing (.fromLocGeohash) mbSearchReq,
        currency,
        fareParams = Just maxFareParams, -- Todo: fix it
        farePolicy = Just $ DFP.fullFarePolicyToFarePolicy fullFarePolicy,
        tipOptions = fullFarePolicy.tipOptions,
        specialLocationTag = specialLocationTag,
        isScheduled = isScheduled,
        tollNames = if isTollApplicable then tollNames else Nothing,
        tollIds = if isTollApplicable then tollIds else Nothing,
        dpVersion = fullFarePolicy.dpVersion,
        congestionMultiplier = DFP.congestionChargeMultiplierToCentesimal <$> fullFarePolicy.congestionChargeMultiplier,
        createdAt = now,
        updatedAt = now,
        eligibleForUpgrade = False,
        supplyDemandRatioToLoc = fullFarePolicy.mbSupplyDemandRatioToLoc,
        supplyDemandRatioFromLoc = fullFarePolicy.mbSupplyDemandRatioFromLoc,
        mbActualQARFromLocGeohash = fullFarePolicy.mbActualQARFromLocGeohash,
        mbActualQARCity = fullFarePolicy.mbActualQARCity,
        smartTipSuggestion = fullFarePolicy.smartTipSuggestion,
        smartTipReason = fullFarePolicy.smartTipReason,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        mbActualQARCityPast = (.mbActualQARCityPast) =<< fullFarePolicy.congestionChargeData,
        mbActualQARFromLocGeohashDistance = (.mbActualQARFromLocGeohashDistance) =<< fullFarePolicy.congestionChargeData,
        mbActualQARFromLocGeohashDistancePast = (.mbActualQARFromLocGeohashDistancePast) =<< fullFarePolicy.congestionChargeData,
        mbActualQARFromLocGeohashPast = (.mbActualQARFromLocGeohashPast) =<< fullFarePolicy.congestionChargeData,
        mbCongestionCity = (.mbCongestionCity) =<< fullFarePolicy.congestionChargeData,
        mbCongestionCityPast = (.mbCongestionCityPast) =<< fullFarePolicy.congestionChargeData,
        mbCongestionFromLocGeohash = (.mbCongestionFromLocGeohash) =<< fullFarePolicy.congestionChargeData,
        mbCongestionFromLocGeohashDistance = (.mbCongestionFromLocGeohashDistance) =<< fullFarePolicy.congestionChargeData,
        mbCongestionFromLocGeohashDistancePast = (.mbCongestionFromLocGeohashDistancePast) =<< fullFarePolicy.congestionChargeData,
        mbCongestionFromLocGeohashPast = (.mbCongestionFromLocGeohashPast) =<< fullFarePolicy.congestionChargeData,
        ..
      }

validateRequest :: DM.Merchant -> DSearchReq -> Flow ValidatedDSearchReq
validateRequest merchant sReq = do
  isValueAddNP <- CQVAN.isValueAddNP sReq.bapId
  -- This checks for origin serviceability too
  NearestOperatingAndSourceCity {nearestOperatingCity, sourceCity} <- getNearestOperatingAndSourceCity merchant sReq.pickupLocation
  let bapCity = nearestOperatingCity.city
  merchantOpCity <- CQMOC.getMerchantOpCity merchant (Just bapCity)
  let (cityDistanceUnit, merchantOpCityId) = (merchantOpCity.distanceUnit, merchantOpCity.id)
  transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId (Just (TransactionId (Id sReq.transactionId))) >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)
  (isInterCity, isCrossCity, destinationTravelCityName) <- checkForIntercityOrCrossCity transporterConfig sReq.dropLocation sReq.toSpecialLocationId sourceCity merchant
  now <- getCurrentTime
  let possibleTripOption = getPossibleTripOption now transporterConfig sReq isInterCity isCrossCity destinationTravelCityName
      isMeterRideSearch = sReq.isMeterRideSearch
      isReserveRide = sReq.isReserveRide
      reserveRideEstimate = sReq.reserveRideEstimate
      numberOfLuggages = sReq.numberOfLuggages
  driverIdForSearch <- mapM getDriverIdFromIdentifier $ bool Nothing sReq.driverIdentifier isValueAddNP
  return ValidatedDSearchReq {..}

data IsIntercityReq = IsIntercityReq
  { pickupLatLong :: LatLong,
    mbDropLatLong :: Maybe LatLong
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data IsIntercityResp = IsIntercityResp
  { isInterCity :: Bool,
    isCrossCity :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

getIsInterCity :: Id DM.Merchant -> Maybe Text -> IsIntercityReq -> Flow IsIntercityResp
getIsInterCity merchantId apiKey IsIntercityReq {..} = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  NearestOperatingAndSourceCity {nearestOperatingCity, sourceCity} <- getNearestOperatingAndSourceCity merchant pickupLatLong
  let bapCity = nearestOperatingCity.city
  merchantOpCity <- CQMOC.getMerchantOpCity merchant (Just bapCity)
  transporterConfig <- CCT.findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCity.id.getId)
  (isInterCity, isCrossCity, _) <- checkForIntercityOrCrossCity transporterConfig mbDropLatLong Nothing sourceCity merchant
  return $ IsIntercityResp {..}

checkForIntercityOrCrossCity :: DTMT.TransporterConfig -> Maybe LatLong -> Maybe (Id SL.SpecialLocation) -> CityState -> DM.Merchant -> Flow (Bool, Bool, Maybe Text)
checkForIntercityOrCrossCity transporterConfig mbDropLocation mbToSpecialLocationId sourceCity merchant = do
  case (mbDropLocation, mbToSpecialLocationId) of
    (Just dropLoc, Nothing) -> do
      (destinationCityState, mbDestinationTravelCityName) <- getDestinationCity merchant dropLoc -- This checks for destination serviceability too
      if destinationCityState.city == sourceCity.city && destinationCityState.city /= Context.City "AnyCity"
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
    _ -> pure (False, False, Nothing)

getPossibleTripOption :: UTCTime -> DTMT.TransporterConfig -> DSearchReq -> Bool -> Bool -> Maybe Text -> TripOption
getPossibleTripOption now tConf dsReq isInterCity isCrossCity destinationTravelCityName = do
  let (schedule, isScheduled) =
        if maybe True not dsReq.isMultimodalSearch && tConf.scheduleRideBufferTime `addUTCTime` now < dsReq.pickupTime
          then (dsReq.pickupTime, True)
          else (now, False)
      tripCategories =
        if checkIfMeterRideSearch dsReq.isMeterRideSearch
          then [OneWay MeterRide]
          else do
            case dsReq.dropLocation of
              Just _ -> do
                if isInterCity
                  then do
                    if isCrossCity
                      then do
                        [CrossCity OneWayOnDemandStaticOffer destinationTravelCityName]
                          <> (if not isScheduled then [CrossCity OneWayRideOtp destinationTravelCityName, CrossCity OneWayOnDemandDynamicOffer destinationTravelCityName] else [])
                      else do
                        [InterCity OneWayOnDemandStaticOffer destinationTravelCityName]
                          <> (if not isScheduled then [InterCity OneWayRideOtp destinationTravelCityName, InterCity OneWayOnDemandDynamicOffer destinationTravelCityName] else [])
                  else do
                    [Rental OnDemandStaticOffer]
                      <> (if not isScheduled then [OneWay OneWayRideOtp, OneWay OneWayOnDemandDynamicOffer, Ambulance OneWayOnDemandDynamicOffer, Rental RideOtp, Delivery OneWayOnDemandDynamicOffer] else [OneWay OneWayRideOtp, OneWay OneWayOnDemandStaticOffer])
              Nothing ->
                [Rental OnDemandStaticOffer]
                  <> [Rental RideOtp | not isScheduled]

  TripOption {..}
  where
    checkIfMeterRideSearch isMeterRideSearch = case isMeterRideSearch of
      Just isMeterRide -> isMeterRide
      Nothing -> False

getDriverIdFromIdentifier :: DRL.DriverIdentifier -> Flow (Id DP.Person)
getDriverIdFromIdentifier driverInfo =
  case driverInfo._type of
    DRL.REFERRAL_CODE -> do
      driverReferralLinkage <- QDR.findByRefferalCode (Id driverInfo.value) >>= fromMaybeM (DriverNotFoundForReferralCode driverInfo.value)
      return driverReferralLinkage.driverId
    DRL.VEHICLE_NUMBER -> do
      vehicle <- QVeh.findByRegistrationNo driverInfo.value >>= fromMaybeM (VehicleDoesNotExist $ "registration number:" +|| driverInfo.value ||+ ".")
      return vehicle.driverId

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
      case filter (\geom -> geom.city /= Context.City "AnyCity") geoms of
        [] ->
          find (\geom -> geom.city == Context.City "AnyCity") geoms & \case
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
      case filter (\geom -> geom.city /= Context.City "AnyCity") geoms of
        [] ->
          find (\geom -> geom.city == Context.City "AnyCity") geoms & \case
            Just anyCityGeom -> do
              interTravelCities <- CQITC.findByMerchantIdAndState merchant.id anyCityGeom.state >>= mapM (\m -> return (distanceBetweenInMeters dropLatLong (LatLong m.lat m.lng), m.cityName))
              mbNearestCity <-
                if null interTravelCities
                  then do
                    operatingCities <- CQMOC.findAllByMerchantIdAndState merchant.id anyCityGeom.state >>= mapM (\m -> return (distanceBetweenInMeters dropLatLong m.location, show m.city))
                    return $ snd <$> listToMaybe (sortBy (comparing fst) operatingCities)
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
              fullAddress = (address >>= decodeAddress) <|> updAddress.full_address,
              instructions = Nothing,
              extras = Nothing
            },
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOpCityId,
        ..
      }

getAddressByGetPlaceName :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> LatLong -> m Address
getAddressByGetPlaceName merchantId merchantOpCityId sessionToken latLong = do
  pickupRes <-
    DMaps.getPlaceName merchantId merchantOpCityId Nothing $
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

transformReserveRideEsttoEst :: (EsqDBFlow m r, CacheFlow m r, EsqDBReplicaFlow m r) => DBppEstimate.BppEstimate -> m DEst.Estimate
transformReserveRideEsttoEst DBppEstimate.BppEstimate {..} = do
  farePolicy <- QFPolicy.findById Nothing (fromMaybe "" farePolicyId)
  fareParams <- QFP.findById (fromMaybe "" fareParamsId)
  let businessDiscount = case (farePolicy, fareParams) of
        (Just farePolicy', Just params) -> if isJust farePolicy'.businessDiscountPercentage then calculateBusinessDiscount params (fromMaybe 0.0 farePolicy'.businessDiscountPercentage) else Nothing
        _ -> Nothing
  let personalDiscount = case (farePolicy, fareParams) of
        (Just farePolicy', Just params) -> if isJust farePolicy'.personalDiscountPercentage then calculateBusinessDiscount params (fromMaybe 0.0 farePolicy'.personalDiscountPercentage) else Nothing
        _ -> Nothing
  return
    DEst.Estimate
      { commissionCharges = Nothing,
        ..
      }
