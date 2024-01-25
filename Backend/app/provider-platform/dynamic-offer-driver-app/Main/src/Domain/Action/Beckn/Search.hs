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
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Domain.Action.UI.Maps as DMaps
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant.TransporterConfig as DTMT
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RideRoute
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverPool
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import SharedLogic.GoogleMaps
import SharedLogic.Ride
import qualified SharedLogic.RiderDetails as SRD
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import Storage.CachedQueries.Merchant.TransporterConfig as CTC
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics.ARDUBPPMetrics as Metrics

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Context.City,
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
    routePoints :: Maybe [LatLong]
  }

data DSearchRes = DSearchRes
  { specialLocationTag :: Maybe Text,
    searchMetricsMVar :: Metrics.SearchMetricsMVar,
    paymentMethodsInfo :: [DMPM.PaymentMethodInfo],
    provider :: DM.Merchant,
    fromLocation :: LatLong,
    toLocation :: Maybe LatLong,
    now :: UTCTime,
    quotes :: [(DQuote.Quote, Maybe NearestDriverInfo)],
    estimates :: [(DEst.Estimate, Maybe NearestDriverInfo)]
  }

data NearestDriverInfo = NearestDriverInfo
  { distanceToNearestDriver :: Meters,
    driverLatLongs :: NonEmpty LatLong
  }

getDistanceAndDuration :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> LatLong -> Maybe Meters -> Maybe Seconds -> Flow (Meters, Seconds)
getDistanceAndDuration _ _ _ _ (Just distance) (Just duration) = return (distance, duration)
getDistanceAndDuration merchantId merchantOpCityId fromLocation toLocation _ _ = do
  response <-
    Maps.getDistance merchantId merchantOpCityId $
      Maps.GetDistanceReq
        { origin = fromLocation,
          destination = toLocation,
          travelMode = Just Maps.CAR
        }
  return (response.distance, response.duration)

handler :: DM.Merchant -> DSearchReq -> Flow DSearchRes
handler merchant sReq = do
  searchMetricsMVar <- Metrics.startSearchMetrics merchant.name
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just sReq.bapCity)
  let merchantId = merchant.id
  sessiontoken <- generateGUIDText
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)

  fromLocation <- buildSearchReqLocation merchant.id merchantOpCityId sessiontoken sReq.pickupAddress sReq.customerLanguage sReq.pickupLocation
  now <- getCurrentTime
  cancellationDues <- getCancellationDues transporterConfig

  (mbSetRouteInfo, mbToLocation, mbDistance, mbDuration) <-
    case sReq.dropLocation of
      Just dropLoc -> do
        (estimatedDistance, estimatedDuration) <- getDistanceAndDuration merchant.id merchantOpCityId sReq.pickupLocation dropLoc sReq.routeDistance sReq.routeDuration
        logDebug $ "distance: " <> show estimatedDistance
        let routeInfo = RouteInfo {distance = Just estimatedDistance, duration = Just estimatedDuration, points = sReq.routePoints}
        toLocation <- buildSearchReqLocation merchant.id merchantOpCityId sessiontoken sReq.dropAddrress sReq.customerLanguage dropLoc

        let setRouteInfo srId = Redis.setExp (searchRequestKey $ getId srId) routeInfo 3600

        return $ (Just setRouteInfo, Just toLocation, Just estimatedDistance, Just estimatedDuration)
      _ -> return (Nothing, Nothing, sReq.routeDistance, sReq.routeDuration) -- estimate distance and durations by user
  let possibleTripOption = getPossibleTripOption now transporterConfig sReq
  allFarePoliciesProduct <- (pure . combineFarePoliciesProducts) =<< ((getAllFarePoliciesProduct merchant.id merchantOpCityId sReq.pickupLocation sReq.dropLocation) `mapM` possibleTripOption.tripCategories)
  let farePolicies = selectFarePolicy (fromMaybe 0 mbDistance) (fromMaybe 0 mbDuration) allFarePoliciesProduct.farePolicies

  (driverPool, selectedFarePolicies) <-
    if transporterConfig.considerDriversForSearch
      then do
        (pool, policies) <- selectDriversAndMatchFarePolicies merchantId merchantOpCityId mbDistance fromLocation transporterConfig farePolicies
        pure (nonEmpty pool, policies)
      else return (Nothing, catMaybes $ everyPossibleVariant <&> \var -> find ((== var) . (.vehicleVariant)) farePolicies)
  -- whenJustM
  --   (QSearchRequestSpecialZone.findByMsgIdAndBapIdAndBppId sReq.messageId sReq.bapId merchantId)
  --   (\_ -> throwError $ InvalidRequest "Duplicate Search request")
  searchReq <- buildSearchRequest sReq merchantId merchantOpCityId fromLocation mbToLocation mbDistance mbDuration allFarePoliciesProduct.specialLocationTag allFarePoliciesProduct.area cancellationDues
  whenJust mbSetRouteInfo $ \setRouteInfo -> setRouteInfo searchReq.id
  triggerSearchEvent SearchEventData {searchRequest = searchReq, merchantId = merchantId}
  void $ QSR.createDSReq searchReq

  let buildEstimateHelper = buildEstimate searchReq.id sReq.pickupTime mbDistance allFarePoliciesProduct.specialLocationTag cancellationDues
  let buildQuoteHelper = buildQuote searchReq merchantId sReq.pickupTime mbDistance mbDuration allFarePoliciesProduct.specialLocationTag cancellationDues

  (estimates, quotes) <- foldrM (processPolicy buildEstimateHelper buildQuoteHelper) ([], []) selectedFarePolicies
  QEst.createMany estimates
  for_ quotes QQuote.create

  forM_ estimates $ \est -> triggerEstimateEvent EstimateEventData {estimate = est, merchantId = merchantId}

  buildDSearchResp sReq.pickupLocation sReq.dropLocation merchantOpCityId allFarePoliciesProduct.specialLocationTag searchMetricsMVar (addNearestDriverInfo driverPool quotes) (addNearestDriverInfo driverPool estimates)
  where
    combineFarePoliciesProducts :: [FarePoliciesProduct] -> FarePoliciesProduct
    combineFarePoliciesProducts products =
      FarePoliciesProduct
        { farePolicies = concatMap farePolicies products,
          area = maybe DFareProduct.Default (.area) $ listToMaybe products,
          specialLocationTag = (listToMaybe products) >>= (.specialLocationTag)
        }

    processPolicy ::
      (DFP.FullFarePolicy -> Flow DEst.Estimate) ->
      (DFP.FullFarePolicy -> Flow DQuote.Quote) ->
      DFP.FullFarePolicy ->
      ([DEst.Estimate], [DQuote.Quote]) ->
      Flow ([DEst.Estimate], [DQuote.Quote])
    processPolicy buildEstimateHelper buildQuoteHelper fp (estimates, quotes) =
      case fp.tripCategory of
        DTC.OneWay DTC.OneWayOnDemandDynamicOffer -> buildEstimateHelper fp >>= \est -> pure (est : estimates, quotes)
        _ -> buildQuoteHelper fp >>= \quote -> pure (estimates, quote : quotes)

    buildDSearchResp fromLocation toLocation merchantOpCityId specialLocationTag searchMetricsMVar quotes estimates = do
      merchantPaymentMethods <- CQMPM.findAllByMerchantOpCityId merchantOpCityId
      let paymentMethodsInfo = DMPM.mkPaymentMethodInfo <$> merchantPaymentMethods
      now <- getCurrentTime
      return $
        DSearchRes
          { provider = merchant,
            ..
          }

    selectFarePolicy distance duration farePolicies =
      filter isValid farePolicies
      where
        isValid farePolicy = checkDistanceBounds farePolicy && checkExtendUpto farePolicy

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
                  maxAllowed = case (farePolicy.maxAdditionalKmsLimit, farePolicy.totalAdditionalKmsLimit) of
                    (Just maxAdditionalKmsLimit, Just totalAdditionalKmsLimit) -> min (min maxAdditionalKmsLimit.getKilometers (timeInHr * includedKm)) (totalAdditionalKmsLimit.getKilometers - (timeInHr * includedKm))
                    (Just maxAdditionalKmsLimit, Nothing) -> min maxAdditionalKmsLimit.getKilometers (timeInHr * includedKm)
                    (Nothing, Just totalAdditionalKmsLimit) -> min (timeInHr * includedKm) (totalAdditionalKmsLimit.getKilometers - (timeInHr * includedKm))
                    _ -> 0 -- or infinite?
               in distInKm - includedKm <= maxAllowed

    getCancellationDues transporterConfig =
      if transporterConfig.canAddCancellationFee
        then do
          case sReq.customerPhoneNum of
            Just number -> do
              now <- getCurrentTime
              (riderDetails, isNewRider) <- SRD.getRiderDetails merchant.id (fromMaybe "+91" merchant.mobileCountryCode) number now False
              when isNewRider $ QRD.create riderDetails
              return riderDetails.cancellationDues
            Nothing -> do
              logWarning "Failed to calculate Customer Cancellation Dues as BAP Phone Number is NULL"
              return 0
        else return 0

getPossibleTripOption :: UTCTime -> DTMT.TransporterConfig -> DSearchReq -> DTC.TripOption
getPossibleTripOption now tConf dsReq = do
  let schedule =
        if tConf.scheduleRideBufferTime `addUTCTime` now < dsReq.pickupTime
          then Just dsReq.pickupTime
          else Nothing
      tripCategories =
        case dsReq.dropLocation of
          Just _ ->
            [DTC.OneWay DTC.OneWayOnDemandStaticOffer, DTC.RoundTrip DTC.OnDemandStaticOffer, DTC.Rental DTC.OnDemandStaticOffer]
              <> (if isNothing schedule then [DTC.OneWay DTC.OneWayRideOtp, DTC.OneWay DTC.OneWayOnDemandDynamicOffer, DTC.RoundTrip DTC.RideOtp, DTC.Rental DTC.RideOtp] else [])
          Nothing ->
            [DTC.Rental DTC.OnDemandStaticOffer]
              <> [DTC.Rental DTC.RideOtp | isNothing schedule]

  DTC.TripOption {..}

addNearestDriverInfo ::
  (HasField "vehicleVariant" a DVeh.Variant) =>
  (Maybe (NonEmpty DriverPoolResult)) ->
  [a] ->
  [(a, Maybe NearestDriverInfo)]
addNearestDriverInfo Nothing estdOrQuotes = map (\a -> (a, Nothing)) estdOrQuotes
addNearestDriverInfo (Just driverPool) estdOrQuotes = do
  let mapOfDPRByVariant = foldl (\m dpr -> M.insertWith (<>) dpr.variant (pure dpr) m) mempty driverPool
  matchInputWithNearestDriver estdOrQuotes mapOfDPRByVariant
  where
    matchInputWithNearestDriver ::
      (HasField "vehicleVariant" a DVeh.Variant) =>
      [a] ->
      M.Map DVeh.Variant (NonEmpty DriverPoolResult) ->
      [(a, Maybe NearestDriverInfo)]
    matchInputWithNearestDriver inputs driverPools = do
      input <- inputs
      let driverPool' = M.lookup input.vehicleVariant driverPools
      case driverPool' of
        Nothing -> mempty
        Just dp -> do
          let driverLatLongs = fmap (\x -> LatLong x.lat x.lon) dp
              distanceToNearestDriver = NE.head dp & (.distanceToPickup)
              nearestDriverInfo = NearestDriverInfo {..}
          return (input, Just nearestDriverInfo)

selectDriversAndMatchFarePolicies :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Meters -> DLoc.Location -> DTMT.TransporterConfig -> [DFP.FullFarePolicy] -> Flow ([DriverPoolResult], [DFP.FullFarePolicy])
selectDriversAndMatchFarePolicies merchantId merchantOpCityId mbDistance fromLocation transporterConfig farePolicies = do
  driverPoolCfg <- getSearchDriverPoolConfig merchantOpCityId mbDistance
  driverPoolNotOnRide <- calculateDriverPool Estimate driverPoolCfg Nothing fromLocation merchantId True Nothing
  logDebug $ "Driver Pool not on ride " <> show driverPoolNotOnRide
  driverPoolCurrentlyOnRide <-
    if null driverPoolNotOnRide
      then do
        if transporterConfig.includeDriverCurrentlyOnRide
          then calculateDriverPoolCurrentlyOnRide Estimate driverPoolCfg Nothing fromLocation merchantId Nothing
          else pure []
      else pure []
  let driverPool =
        driverPoolNotOnRide
          <> map (\DriverPoolResultCurrentlyOnRide {..} -> DriverPoolResult {..}) driverPoolCurrentlyOnRide
  logDebug $ "Search handler: driver pool " <> show driverPool
  let onlyFPWithDrivers = filter (\fp -> (skipDriverPoolCheck fp.tripCategory) || (isJust (find (\dp -> dp.variant == fp.vehicleVariant) driverPool))) farePolicies
  return (driverPool, onlyFPWithDrivers)

skipDriverPoolCheck :: DTC.TripCategory -> Bool
skipDriverPoolCheck (DTC.OneWay DTC.OneWayOnDemandStaticOffer) = False
skipDriverPoolCheck (DTC.OneWay DTC.OneWayOnDemandDynamicOffer) = False
skipDriverPoolCheck _ = True

buildSearchRequest ::
  ( MonadFlow m
  ) =>
  DSearchReq ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DLoc.Location ->
  Maybe DLoc.Location ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Text ->
  DFareProduct.Area ->
  HighPrecMoney ->
  m DSR.SearchRequest
buildSearchRequest DSearchReq {..} providerId merchantOpCityId fromLocation mbToLocation mbDistance mbDuration specialLocationTag area customerCancellationDues = do
  uuid <- generateGUID
  now <- getCurrentTime
  pure
    DSR.SearchRequest
      { id = Id uuid,
        messageId = Nothing,
        startTime = pickupTime,
        area = Just area,
        bapCity = Just bapCity,
        bapCountry = Just bapCountry,
        autoAssignEnabled = Nothing,
        merchantOperatingCityId = merchantOpCityId,
        toLocation = mbToLocation,
        estimatedDistance = mbDistance,
        estimatedDuration = mbDuration,
        createdAt = now,
        ..
      }

buildQuote ::
  ( EsqDBFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DSR.SearchRequest ->
  Id DM.Merchant ->
  UTCTime ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Text ->
  HighPrecMoney ->
  DFP.FullFarePolicy ->
  m DQuote.Quote
buildQuote searchRequest transporterId pickupTime mbDistance mbDuration specialLocationTag customerCancellationDues fullFarePolicy = do
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
          customerCancellationDues = customerCancellationDues,
          nightShiftOverlapChecking = True, -- sending True in rental
          estimatedDistance = searchRequest.estimatedDistance,
          estimatedRideDuration = searchRequest.estimatedDuration,
          timeDiffFromUtc = Nothing
        }
  quoteId <- Id <$> generateGUID
  now <- getCurrentTime
  let estimatedFare = fareSum fareParams
      estimatedFinishTime = (\duration -> fromIntegral duration `addUTCTime` now) <$> mbDuration
  -- Keeping quote expiry as search request expiry. Slack discussion: https://juspay.slack.com/archives/C0139KHBFU1/p1683349807003679
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DQuote.Quote
      { id = quoteId,
        searchRequestId = searchRequest.id,
        providerId = transporterId,
        distance = mbDistance,
        vehicleVariant = fullFarePolicy.vehicleVariant,
        tripCategory = fullFarePolicy.tripCategory,
        farePolicy = Just $ DFP.fullFarePolicyToFarePolicy fullFarePolicy,
        createdAt = now,
        updatedAt = now,
        ..
      }

buildEstimate ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id DSR.SearchRequest ->
  UTCTime ->
  Maybe Meters ->
  Maybe Text ->
  HighPrecMoney ->
  DFP.FullFarePolicy ->
  m DEst.Estimate
buildEstimate searchReqId startTime mbDistance specialLocationTag customerCancellationDues fullFarePolicy = do
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
          customerCancellationDues = customerCancellationDues,
          nightShiftOverlapChecking = False,
          estimatedDistance = Nothing,
          estimatedRideDuration = Nothing,
          timeDiffFromUtc = Nothing
        }
  let baseFare = fareSum fareParams
  logDebug $ "baseFare: " <> show baseFare
  uuid <- generateGUID
  now <- getCurrentTime
  let mbDriverExtraFeeBounds = DFP.findDriverExtraFeeBoundsByDistance dist <$> fullFarePolicy.driverExtraFeeBounds
  pure
    DEst.Estimate
      { id = Id uuid,
        requestId = searchReqId,
        vehicleVariant = fullFarePolicy.vehicleVariant,
        tripCategory = fullFarePolicy.tripCategory,
        estimatedDistance = mbDistance,
        minFare = baseFare + maybe 0 (.minFee) mbDriverExtraFeeBounds,
        maxFare = baseFare + maybe 0 (.maxFee) mbDriverExtraFeeBounds,
        fareParams = Just fareParams,
        farePolicy = Just $ DFP.fullFarePolicyToFarePolicy fullFarePolicy,
        specialLocationTag = specialLocationTag,
        createdAt = now,
        updatedAt = now
      }

validateRequest :: Id DM.Merchant -> DSearchReq -> Flow DM.Merchant
validateRequest merchantId sReq = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless merchant.enabled $ throwError AgencyDisabled
  unlessM (rideServiceable' merchant.geofencingConfig QGeometry.someGeometriesContain sReq.pickupLocation sReq.dropLocation) $
    throwError RideNotServiceable
  return merchant
  where
    rideServiceable' ::
      MonadFlow m =>
      GeofencingConfig ->
      (LatLong -> [Text] -> m Bool) ->
      LatLong ->
      Maybe LatLong ->
      m Bool
    rideServiceable' geofencingConfig someGeometriesContain origin mbDestination = do
      originServiceable <-
        case geofencingConfig.origin of
          Unrestricted -> pure True
          Regions regions -> someGeometriesContain origin regions
      destinationServiceable <-
        case geofencingConfig.destination of
          Unrestricted -> pure True
          Regions regions -> do
            maybe (pure True) (`someGeometriesContain` regions) mbDestination
      pure $ originServiceable && destinationServiceable

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
