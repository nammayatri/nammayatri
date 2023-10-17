{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

module Domain.Action.Beckn.Search
  ( DSearchReq (..),
    DSearchReqOnDemand' (..),
    DSearchReqRental' (..),
    DSearchRes (..),
    EstimateInfo (..),
    RentalQuoteInfo (..),
    SpecialZoneQuoteInfo (..),
    handler,
    validateRequest,
    searchRequestKey,
  )
where

import qualified Beckn.Types.Core.Taxi.Search as BA
import Control.Applicative ((<|>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Domain.Action.UI.Maps as DMaps
import qualified Domain.Types.Estimate as DEst
import Domain.Types.FareParameters
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.DriverPoolConfig (DriverPoolConfig)
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
-- import qualified Storage.CachedQueries.FareProduct as QFareProduct
import qualified Domain.Types.QuoteRental as DQuoteRental
import qualified Domain.Types.QuoteSpecialZone as DQuoteSpecialZone
import Domain.Types.RideRoute
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestSpecialZone as DSRSZ
import qualified Domain.Types.Vehicle as DVeh
import Environment
import EulerHS.Prelude (Alternative (empty), whenJustM)
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
import qualified SharedLogic.Estimate as SHEst
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import SharedLogic.GoogleMaps
import qualified Storage.CachedQueries.FarePolicy as QFP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import Storage.CachedQueries.Merchant.TransporterConfig as CTC
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.FareProduct as QFareProduct
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.QuoteRental as QQuoteRental
import qualified Storage.Queries.QuoteSpecialZone as QQuoteSpecialZone
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestSpecialZone as QSearchRequestSpecialZone
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics.ARDUBPPMetrics as Metrics

-- import qualified Control.Monad as NE

data DSearchReqOnDemand' = DSearchReqOnDemand'
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Context.City,
    bapCountry :: Context.Country,
    pickupLocation :: LatLong,
    pickupTime :: UTCTime,
    dropLocation :: LatLong,
    pickupAddress :: Maybe BA.Address,
    dropAddrress :: Maybe BA.Address,
    routeDistance :: Maybe Meters,
    routeDuration :: Maybe Seconds,
    device :: Maybe Text,
    customerLanguage :: Maybe Maps.Language,
    disabilityTag :: Maybe Text,
    routePoints :: Maybe [LatLong]
  }

data DSearchReqRental' = DSearchReqRental'
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Context.City,
    bapCountry :: Context.Country,
    pickupLocation :: LatLong,
    pickupTime :: UTCTime,
    pickupAddress :: Maybe BA.Address,
    device :: Maybe Text,
    customerLanguage :: Maybe Maps.Language,
    disabilityTag :: Maybe Text
  }

data DSearchReq = DSearchReqOnDemand DSearchReqOnDemand' | DSearchReqRental DSearchReqRental'

data SpecialZoneQuoteInfo = SpecialZoneQuoteInfo
  { quoteId :: Id DQuoteSpecialZone.QuoteSpecialZone,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Money,
    fromLocation :: LatLong,
    toLocation :: LatLong,
    specialLocationTag :: Maybe Text,
    startTime :: UTCTime
  }

data RentalQuoteInfo = RentalQuoteInfo
  { quoteId :: Id DQuoteRental.QuoteRental,
    vehicleVariant :: DVeh.Variant,
    baseFare :: Money,
    baseDistance :: Kilometers,
    baseDuration :: Hours,
    fromLocation :: LatLong,
    rentalTag :: Maybe Text,
    startTime :: UTCTime
  }

data DSearchRes = DSearchRes
  { provider :: DM.Merchant,
    fromLocation :: LatLong,
    toLocation :: Maybe LatLong,
    now :: UTCTime,
    estimateList :: Maybe [EstimateInfo],
    specialQuoteList :: Maybe [SpecialZoneQuoteInfo],
    rentalQuoteList :: Maybe [RentalQuoteInfo],
    searchMetricsMVar :: Metrics.SearchMetricsMVar,
    paymentMethodsInfo :: [DMPM.PaymentMethodInfo]
  }

data EstimateInfo = EstimateInfo
  { estimate :: DEst.Estimate,
    distanceToNearestDriver :: Meters,
    driverLatLongs :: NonEmpty LatLong
  }

data DistanceAndDuration = DistanceAndDuration
  { distance :: Meters,
    duration :: Seconds
  }

getDistanceAndDuration :: Id DM.Merchant -> LatLong -> LatLong -> Maybe Meters -> Maybe Seconds -> Flow DistanceAndDuration
getDistanceAndDuration _ _ _ (Just distance) (Just duration) = return $ DistanceAndDuration {distance, duration}
getDistanceAndDuration merchantId fromLocation toLocation _ _ = do
  response <-
    Maps.getDistance merchantId $
      Maps.GetDistanceReq
        { origin = fromLocation,
          destination = toLocation,
          travelMode = Just Maps.CAR
        }
  return DistanceAndDuration {distance = response.distance, duration = response.duration}

handler :: DM.Merchant -> DSearchReq -> Flow DSearchRes
handler merchant sReq' =
  case sReq' of
    DSearchReqOnDemand sReq -> do
      searchMetricsMVar <- Metrics.startSearchMetrics merchant.name
      let fromLocationLatLong = sReq.pickupLocation
          toLocationLatLong = sReq.dropLocation
          merchantId = merchant.id
      result <- getDistanceAndDuration merchantId fromLocationLatLong toLocationLatLong sReq.routeDistance sReq.routeDuration
      logDebug $ "distance: " <> show result.distance
      sessiontoken <- generateGUIDText
      fromLocation <- buildSearchReqLocation merchantId sessiontoken sReq.pickupAddress sReq.customerLanguage sReq.pickupLocation
      toLocation <- buildSearchReqLocation merchantId sessiontoken sReq.dropAddrress sReq.customerLanguage sReq.dropLocation
      let routeInfo = RouteInfo {distance = sReq.routeDistance, duration = sReq.routeDuration, points = sReq.routePoints}

      allFarePoliciesProduct <- getAllFarePoliciesProduct merchantId fromLocationLatLong toLocationLatLong
      let farePolicies = selectFarePolicy result.distance allFarePoliciesProduct.farePolicies
      (quotes, mbEstimateInfos) <-
        case allFarePoliciesProduct.flow of
          DFareProduct.RIDE_OTP -> do
            whenJustM
              (QSearchRequestSpecialZone.findByMsgIdAndBapIdAndBppId sReq.messageId sReq.bapId merchantId)
              (\_ -> throwError $ InvalidRequest "Duplicate Search request")
            searchRequestSpecialZone <- buildSearchRequestSpecialZone sReq merchantId fromLocation toLocation result.distance result.duration allFarePoliciesProduct.area
            triggerSearchEvent SearchEventData {searchRequest = Right searchRequestSpecialZone, merchantId = merchantId}
            _ <- QSearchRequestSpecialZone.createSearchRequestSpecialZone searchRequestSpecialZone
            Redis.setExp (searchRequestKey $ getId searchRequestSpecialZone.id) routeInfo 3600
            now <- getCurrentTime
            let listOfVehicleVariants = listVehicleVariantHelper farePolicies
            listOfSpecialZoneQuotes <- do
              for listOfVehicleVariants $ \farePolicy -> do
                fareParams <-
                  calculateFareParameters
                    CalculateFareParametersParams
                      { farePolicy = farePolicy,
                        distance = result.distance,
                        rideTime = sReq.pickupTime,
                        waitingTime = Nothing,
                        driverSelectedFare = Nothing,
                        customerExtraFee = Nothing,
                        nightShiftCharge = Nothing,
                        rentalRideParams = Nothing
                      }
                buildSpecialZoneQuote
                  searchRequestSpecialZone
                  fareParams
                  merchant.id
                  result.distance
                  farePolicy.vehicleVariant
                  result.duration
                  allFarePoliciesProduct.specialLocationTag
            for_ listOfSpecialZoneQuotes QQuoteSpecialZone.create
            return (Just (mkQuoteInfo fromLocation toLocation now <$> listOfSpecialZoneQuotes), Nothing)
          DFareProduct.NORMAL -> buildEstimates sReq farePolicies result fromLocation toLocation allFarePoliciesProduct.specialLocationTag allFarePoliciesProduct.area routeInfo
          DFareProduct.RENTAL -> throwError $ InvalidRequest "RentalRequest is not allowed in onDemand"
      merchantPaymentMethods <- CQMPM.findAllByMerchantId merchantId
      let paymentMethodsInfo = DMPM.mkPaymentMethodInfo <$> merchantPaymentMethods
      buildSearchRes merchant fromLocationLatLong (Just toLocationLatLong) mbEstimateInfos quotes Nothing searchMetricsMVar paymentMethodsInfo
    DSearchReqRental sReq -> do
      searchMetricsMVar <- Metrics.startSearchMetrics merchant.name
      let fromLocationLatLong = sReq.pickupLocation
          merchantId = merchant.id
      sessiontoken <- generateGUIDText
      fromLocation <- buildSearchReqLocation merchantId sessiontoken sReq.pickupAddress sReq.customerLanguage sReq.pickupLocation
      fareProducts <- QFareProduct.findAllFareProductForFlow merchantId DFareProduct.RENTAL
      rentalfarePolicies <-
        mapM
          ( \fareProduct -> do
              farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
              return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant farePolicy
          )
          fareProducts
      rentalSearchReq <- buildRentalSearchRequest sReq merchantId fromLocation
      _ <- QSR.create rentalSearchReq
      triggerSearchEvent SearchEventData {searchRequest = Left rentalSearchReq, merchantId = merchantId}
      let allRentalSlabandFarePolicy = listAllTheRentalSlabs rentalfarePolicies
      (quotes, mbEstimateInfos) <- do
        now <- getCurrentTime
        -- let listOfBuildRentalQuote = []
        listOfRentalQuotes <- do
          for allRentalSlabandFarePolicy $ \(rentalslab :: FarePolicyD.FPSlabsDetailsSlab, farePolicy) -> do
            fareParams <-
              calculateFareParameters
                CalculateFareParametersParams
                  { farePolicy = farePolicy,
                    distance = rentalslab.startDistance,
                    rideTime = sReq.pickupTime,
                    waitingTime = Nothing,
                    driverSelectedFare = Nothing,
                    customerExtraFee = Nothing,
                    nightShiftCharge = Nothing,
                    rentalRideParams = Nothing
                  }
            buildRentalQuote
              rentalSearchReq
              fareParams
              merchant.id
              rentalslab.startDistance
              farePolicy.vehicleVariant
              (Seconds 1000) -- FIX ME: use correct table
              -- let z = concat listOfRentalQuotes
        for_ listOfRentalQuotes QQuoteRental.create
        return (Just (mkRentalQuoteInfo fromLocation now <$> listOfRentalQuotes), Nothing)
      merchantPaymentMethods <- CQMPM.findAllByMerchantId merchantId
      let paymentMethodsInfo = DMPM.mkPaymentMethodInfo <$> merchantPaymentMethods
      buildSearchRes merchant fromLocationLatLong Nothing mbEstimateInfos Nothing quotes searchMetricsMVar paymentMethodsInfo
  where
    listAllTheRentalSlabs =
      concatMap
        ( \farepolicy -> case farepolicy.farePolicyDetails of
            FarePolicyD.SlabsDetails det -> map (,farepolicy) (toList det.slabs)
            _ -> []
        )
    listVehicleVariantHelper farePolicy = catMaybes $ everyPossibleVariant <&> \var -> find ((== var) . (.vehicleVariant)) farePolicy

    buildEstimates onDemandSearchRequest farePolicies result fromLocation toLocation specialLocationTag area routeInfo = do
      driverPoolCfg <- getDriverPoolConfig merchant.id result.distance
      estimateInfos <- buildEstimatesInfos onDemandSearchRequest fromLocation toLocation driverPoolCfg result farePolicies specialLocationTag area routeInfo
      return (Nothing, Just estimateInfos)

    selectFarePolicy distance farePolicies = do
      farePolicy <- farePolicies
      let check =
            farePolicy.allowedTripDistanceBounds <&> \allowedTripDistanceBounds ->
              allowedTripDistanceBounds.minAllowedTripDistance <= distance
                && allowedTripDistanceBounds.maxAllowedTripDistance >= distance
      case check of
        Just False -> empty
        _ -> return farePolicy

    zipMatched estimates driverPools = do
      estimate <- estimates
      let driverPool = M.lookup estimate.vehicleVariant driverPools
      case driverPool of
        Nothing -> mempty
        Just dp -> return (estimate, dp)

    buildEstimatesInfos ::
      DSearchReqOnDemand' ->
      DLoc.Location ->
      DLoc.Location ->
      DriverPoolConfig ->
      DistanceAndDuration ->
      [DFP.FullFarePolicy] ->
      Maybe Text ->
      DFareProduct.Area ->
      RouteInfo ->
      Flow [EstimateInfo]
    buildEstimatesInfos onDemandSearchRequest fromLocation toLocation driverPoolCfg result farePolicies specialLocationTag area routeInfo = do
      let merchantId = merchant.id
      if null farePolicies
        then do
          logDebug "Trip doesnot match any fare policy constraints."
          return []
        else do
          driverPoolNotOnRide <- calculateDriverPool Estimate driverPoolCfg Nothing fromLocation merchantId True Nothing
          logDebug $ "Driver Pool not on ride " <> show driverPoolNotOnRide
          driverPoolCurrentlyOnRide <-
            if null driverPoolNotOnRide
              then do
                transporter <- CTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigDoesNotExist merchantId.getId)
                if transporter.includeDriverCurrentlyOnRide
                  then calculateDriverPoolCurrentlyOnRide Estimate driverPoolCfg Nothing fromLocation merchantId Nothing
                  else pure []
              else pure []
          let driverPool =
                driverPoolNotOnRide
                  <> map (\DriverPoolResultCurrentlyOnRide {..} -> DriverPoolResult {..}) driverPoolCurrentlyOnRide
          logDebug $ "Search handler: driver pool " <> show driverPool

          let onlyFPWithDrivers = filter (\fp -> isJust (find (\dp -> dp.variant == fp.vehicleVariant) driverPool)) farePolicies
          searchReq <- buildSearchRequest onDemandSearchRequest merchantId fromLocation toLocation result.distance result.duration specialLocationTag area
          Redis.setExp (searchRequestKey $ getId searchReq.id) routeInfo 3600
          estimates <- mapM (SHEst.buildEstimate searchReq.id onDemandSearchRequest.pickupTime result.distance specialLocationTag) onlyFPWithDrivers
          triggerSearchEvent SearchEventData {searchRequest = Left searchReq, merchantId = merchantId}

          forM_ estimates $ \est -> do
            triggerEstimateEvent EstimateEventData {estimate = est, merchantId = merchantId}
          _ <- QSR.createDSReq searchReq
          QEst.createMany estimates

          let mapOfDPRByVariant = foldl (\m dpr -> M.insertWith (<>) dpr.variant (pure dpr) m) mempty driverPool
              tuplesOfEstAndDBR :: [(DEst.Estimate, NonEmpty DriverPoolResult)] = zipMatched estimates mapOfDPRByVariant
          logDebug $ "bap uri: " <> show onDemandSearchRequest.bapUri
          return $ makeEstimateInfo <$> tuplesOfEstAndDBR

    makeEstimateInfo :: (DEst.Estimate, NonEmpty DriverPoolResult) -> EstimateInfo
    makeEstimateInfo (estimate, driverPool) = do
      let driverLatLongs = fmap (\x -> LatLong x.lat x.lon) driverPool
          distanceToNearestDriver = NE.head driverPool & (.distanceToPickup)
      EstimateInfo
        { ..
        }

buildSearchRes ::
  (MonadTime m) =>
  DM.Merchant ->
  LatLong ->
  Maybe LatLong ->
  Maybe [EstimateInfo] ->
  Maybe [SpecialZoneQuoteInfo] ->
  Maybe [RentalQuoteInfo] ->
  Metrics.SearchMetricsMVar ->
  [DMPM.PaymentMethodInfo] ->
  m DSearchRes
buildSearchRes org fromLocation toLocation estimateList specialQuoteList rentalQuoteList searchMetricsMVar paymentMethodsInfo = do
  now <- getCurrentTime
  pure $
    DSearchRes
      { provider = org,
        now,
        fromLocation,
        toLocation,
        estimateList,
        specialQuoteList,
        rentalQuoteList,
        searchMetricsMVar,
        paymentMethodsInfo
      }

buildSearchRequest ::
  ( MonadFlow m
  ) =>
  DSearchReqOnDemand' ->
  Id DM.Merchant ->
  DLoc.Location ->
  DLoc.Location ->
  Meters ->
  Seconds ->
  Maybe Text ->
  DFareProduct.Area ->
  m DSR.SearchRequest
buildSearchRequest DSearchReqOnDemand' {..} providerId fromLocation toLocation estimatedDistance estimatedDuration specialLocationTag area = do
  uuid <- generateGUID
  now <- getCurrentTime
  let searchDetails =
        DSR.SearchRequestDetailsOnDemand
          { fromLocation = fromLocation,
            toLocation = toLocation,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            specialLocationTag = specialLocationTag,
            autoAssignEnabled = Nothing
          }
  pure
    DSR.SearchRequest
      { id = Id uuid,
        createdAt = now,
        area = Just area,
        bapCity = Just bapCity,
        bapCountry = Just bapCountry,
        searchRequestDetails = searchDetails,
        tag = DSR.ON_DEMAND,
        ..
      }

buildRentalSearchRequest ::
  ( MonadFlow m
  ) =>
  DSearchReqRental' ->
  Id DM.Merchant ->
  DLoc.Location ->
  m DSR.SearchRequest
buildRentalSearchRequest DSearchReqRental' {..} providerId fromLocation = do
  uuid <- generateGUID
  now <- getCurrentTime
  let searchDetails =
        DSR.SearchRequestDetailsRental
          { rentalFromLocation = fromLocation
          }
  pure
    DSR.SearchRequest
      { id = Id uuid,
        createdAt = now,
        area = Nothing,
        bapCity = Just bapCity,
        bapCountry = Just bapCountry,
        searchRequestDetails = searchDetails,
        tag = DSR.RENTAL,
        ..
      }

buildSearchRequestSpecialZone ::
  ( MonadGuid m,
    MonadTime m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DSearchReqOnDemand' ->
  Id DM.Merchant ->
  DLoc.Location ->
  DLoc.Location ->
  Meters ->
  Seconds ->
  DFareProduct.Area ->
  m DSRSZ.SearchRequestSpecialZone
buildSearchRequestSpecialZone DSearchReqOnDemand' {..} providerId fromLocation toLocation estimatedDistance estimatedDuration area = do
  uuid <- generateGUID
  now <- getCurrentTime
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DSRSZ.SearchRequestSpecialZone
      { id = Id uuid,
        startTime = pickupTime,
        createdAt = now,
        updatedAt = now,
        area = Just area,
        ..
      }

buildSpecialZoneQuote ::
  ( EsqDBFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DSRSZ.SearchRequestSpecialZone ->
  FareParameters ->
  Id DM.Merchant ->
  Meters ->
  DVeh.Variant ->
  Seconds ->
  Maybe Text ->
  m DQuoteSpecialZone.QuoteSpecialZone
buildSpecialZoneQuote productSearchRequest fareParams transporterId distance vehicleVariant duration specialLocationTag = do
  quoteId <- Id <$> generateGUID
  now <- getCurrentTime
  let estimatedFare = fareSum fareParams
      estimatedFinishTime = fromIntegral duration `addUTCTime` now
  -- Keeping quote expiry as search request expiry. Slack discussion: https://juspay.slack.com/archives/C0139KHBFU1/p1683349807003679
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DQuoteSpecialZone.QuoteSpecialZone
      { id = quoteId,
        searchRequestId = productSearchRequest.id,
        providerId = transporterId,
        createdAt = now,
        updatedAt = now,
        ..
      }

buildRentalQuote ::
  ( EsqDBFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DSR.SearchRequest ->
  FareParameters ->
  Id DM.Merchant ->
  Meters ->
  DVeh.Variant ->
  Seconds ->
  m DQuoteRental.QuoteRental
buildRentalQuote productSearchRequest fareParams transporterId distance vehicleVariant duration = do
  quoteId <- Id <$> generateGUID
  now <- getCurrentTime
  let estimatedFare = fareSum fareParams
      estimatedFinishTime = fromIntegral duration `addUTCTime` now
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DQuoteRental.QuoteRental
      { id = quoteId,
        searchRequestId = productSearchRequest.id,
        providerId = transporterId,
        createdAt = now,
        updatedAt = now,
        baseDistance = metersToKilometers distance,
        baseDuration = Hours $ div (getSeconds duration) 3600,
        baseFare = estimatedFare,
        ..
      }

mkQuoteInfo :: DLoc.Location -> DLoc.Location -> UTCTime -> DQuoteSpecialZone.QuoteSpecialZone -> SpecialZoneQuoteInfo
mkQuoteInfo fromLoc toLoc startTime DQuoteSpecialZone.QuoteSpecialZone {..} = do
  let fromLocation = Maps.getCoordinates fromLoc
      toLocation = Maps.getCoordinates toLoc
  SpecialZoneQuoteInfo
    { quoteId = id,
      ..
    }

mkRentalQuoteInfo :: DLoc.Location -> UTCTime -> DQuoteRental.QuoteRental -> RentalQuoteInfo
mkRentalQuoteInfo fromLoc startTime DQuoteRental.QuoteRental {..} = do
  let fromLocation = Maps.getCoordinates fromLoc
  RentalQuoteInfo
    { quoteId = id,
      rentalTag = Nothing,
      ..
    }

validateRequest :: Id DM.Merchant -> DSearchReq -> Flow DM.Merchant
validateRequest merchantId sReq' = do
  let (fromLocationLatLong, toLocationLatLong) = case sReq' of
        DSearchReqOnDemand sReq -> (sReq.pickupLocation, Just sReq.dropLocation)
        DSearchReqRental sReq -> (sReq.pickupLocation, Nothing)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless merchant.enabled $ throwError AgencyDisabled
  unlessM (rideServiceable' merchant.geofencingConfig QGeometry.someGeometriesContain fromLocationLatLong toLocationLatLong) $
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

buildSearchReqLocation :: ServiceFlow m r => Id DM.Merchant -> Text -> Maybe BA.Address -> Maybe Maps.Language -> LatLong -> m DLoc.Location
buildSearchReqLocation merchantId sessionToken address customerLanguage latLong@Maps.LatLong {..} = do
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
    _ -> getAddressByGetPlaceName merchantId sessionToken latLong
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

getAddressByGetPlaceName :: ServiceFlow m r => Id DM.Merchant -> Text -> LatLong -> m Address
getAddressByGetPlaceName merchantId sessionToken latLong = do
  pickupRes <-
    DMaps.getPlaceName merchantId $
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

searchRequestKey :: Text -> Text
searchRequestKey sId = "Driver:Search:Request:" <> sId
