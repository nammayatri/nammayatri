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
    SpecialZoneQuoteInfo (..),
    handler,
  )
where

import qualified Data.Map as M
import Domain.Types.FareParameters
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.QuoteSpecialZone as DQuoteSpecialZone
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequestSpecialZone as DSRSZ
import qualified Domain.Types.Vehicle as DVeh
import Environment
import EulerHS.Prelude (whenJustM)
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.Prelude
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.DriverPool hiding (lat, lon)
import SharedLogic.Estimate (EstimateItem, buildEstimate, buildEstimateFromSlabFarePolicy)
import SharedLogic.FareCalculator
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.SlabFarePolicy as SFarePolicyS
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.QuoteSpecialZone as QQuoteSpecialZone
import qualified Storage.Queries.SearchRequestSpecialZone as QSearchRequestSpecialZone
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics.ARDUBPPMetrics as Metrics

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: LatLong,
    pickupTime :: UTCTime,
    dropLocation :: LatLong,
    routeInfo :: Maybe Maps.RouteInfo
  }

data SpecialZoneQuoteInfo = SpecialZoneQuoteInfo
  { quoteId :: Id DQuoteSpecialZone.QuoteSpecialZone,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Money,
    fromLocation :: LatLong,
    toLocation :: LatLong,
    startTime :: UTCTime
  }

data DSearchRes = DSearchRes
  { provider :: DM.Merchant,
    fromLocation :: LatLong,
    toLocation :: LatLong,
    now :: UTCTime,
    estimateList :: Maybe [EstimateItem],
    specialQuoteList :: Maybe [SpecialZoneQuoteInfo],
    searchMetricsMVar :: Metrics.SearchMetricsMVar
  }

data DistanceAndDuration = DistanceAndDuration
  { distance :: Meters,
    duration :: Seconds
  }

getDistanceAndDuration :: Id DM.Merchant -> LatLong -> LatLong -> Maybe Maps.RouteInfo -> Flow DistanceAndDuration
getDistanceAndDuration merchantId fromLocation toLocation routeInfo = case routeInfo of
  Just (Maps.RouteInfo (Just duration) (Just distance) _ _ _) -> return $ DistanceAndDuration {distance, duration}
  _ -> getMapsDistance
  where
    getMapsDistance = do
      response <-
        Maps.getDistance merchantId $
          Maps.GetDistanceReq
            { origin = fromLocation,
              destination = toLocation,
              travelMode = Just Maps.CAR
            }
      return DistanceAndDuration {distance = response.distance, duration = response.duration}

handler :: Id DM.Merchant -> DSearchReq -> Flow DSearchRes
handler merchantId sReq = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless org.enabled $ throwError AgencyDisabled
  searchMetricsMVar <- Metrics.startSearchMetrics org.name
  let fromLocationLatLong = sReq.pickupLocation
      toLocationLatLong = sReq.dropLocation
  fromLocation <- buildSearchReqLocation fromLocationLatLong
  toLocation <- buildSearchReqLocation toLocationLatLong
  unlessM (rideServiceable org.geofencingConfig QGeometry.someGeometriesContain fromLocationLatLong (Just toLocationLatLong)) $
    throwError RideNotServiceable
  result <- getDistanceAndDuration merchantId fromLocationLatLong toLocationLatLong sReq.routeInfo
  CD.cacheDistance sReq.transactionId (result.distance, result.duration)
  logDebug $ "distance: " <> show result.distance
  mbSpecialLocation <- QSpecialLocation.findSpecialLocationByLatLong fromLocationLatLong

  (quotes :: Maybe [SpecialZoneQuoteInfo], estimates' :: Maybe [EstimateItem]) <-
    if isJust mbSpecialLocation
      then do
        whenJustM
          (QSearchRequestSpecialZone.findByMsgIdAndBapIdAndBppId sReq.messageId sReq.bapId merchantId)
          (\_ -> throwError $ InvalidRequest "Duplicate Search request")
        searchRequestSpecialZone <- buildSearchRequestSpecialZone sReq merchantId fromLocation toLocation result.distance result.duration
        Esq.runTransaction $ do
          QSearchRequestSpecialZone.create searchRequestSpecialZone
        now <- getCurrentTime
        listOfSpecialZoneQuotes <- case org.farePolicyType of
          SLAB -> do
            allFarePolicies <- SFarePolicyS.findAllByMerchantId org.id
            let slabFarePolicies = selectFarePolicy result.distance allFarePolicies
            let listOfVehicleVariants = listVehicleVariantHelper slabFarePolicies
            for listOfVehicleVariants $ \slabFarePolicy -> do
              fareParams <- calculateFare org.id (Right slabFarePolicy) result.distance sReq.pickupTime Nothing
              buildSpecialZoneQuote
                searchRequestSpecialZone
                fareParams
                org.id
                result.distance
                slabFarePolicy.vehicleVariant
                result.duration
          -- return listOfSpecialZoneQuotes
          NORMAL -> do
            allFarePolicies <- FarePolicyS.findAllByMerchantId org.id (Just result.distance)
            let farePolicies = selectFarePolicy result.distance allFarePolicies
            let listOfVehicleVariants = listVehicleVariantHelper farePolicies
            for listOfVehicleVariants $ \farePolicy -> do
              fareParams <- calculateFare org.id (Left farePolicy) result.distance sReq.pickupTime Nothing
              buildSpecialZoneQuote
                searchRequestSpecialZone
                fareParams
                org.id
                result.distance
                farePolicy.vehicleVariant
                result.duration
        Esq.runTransaction $
          for_ listOfSpecialZoneQuotes QQuoteSpecialZone.create
        return (Just (mkQuoteInfo fromLocation toLocation now <$> listOfSpecialZoneQuotes), Nothing)
      else do
        driverPoolCfg <- getDriverPoolConfig merchantId result.distance
        estimates <-
          case org.farePolicyType of
            SLAB -> do
              allFarePolicies <- SFarePolicyS.findAllByMerchantId org.id
              let slabFarePolicies = selectFarePolicy result.distance allFarePolicies
              buildEstimates sReq fromLocation driverPoolCfg org result buildEstimateFromSlabFarePolicy slabFarePolicies
            NORMAL -> do
              allFarePolicies <- FarePolicyS.findAllByMerchantId org.id (Just result.distance)
              let farePolicies = selectFarePolicy result.distance allFarePolicies
              buildEstimates sReq fromLocation driverPoolCfg org result buildEstimate farePolicies
        return (Nothing, Just estimates)
  buildSearchRes org fromLocationLatLong toLocationLatLong estimates' quotes searchMetricsMVar
  where
    listVehicleVariantHelper farePolicy = catMaybes $ everyPossibleVariant <&> \var -> find ((== var) . (.vehicleVariant)) farePolicy

    selectFarePolicy distance = filter (\fp -> checkTripConstraints distance fp.minAllowedTripDistance fp.maxAllowedTripDistance)
      where
        checkTripConstraints tripDistance minAllowedTripDistance maxAllowedTripDistance =
          let cond1 = (<= tripDistance) <$> minAllowedTripDistance
              cond2 = (>= tripDistance) <$> maxAllowedTripDistance
           in and $ catMaybes [cond1, cond2]

    getfilteredProtoQuotes driverPool farePolicies = do
      let listOfProtoQuotes = foldl (\m dpr -> M.insertWith (<>) dpr.variant (pure dpr) m) mempty driverPool
      let filteredProtoQuotes = zipMatched farePolicies listOfProtoQuotes
      filteredProtoQuotes
      where
        zipMatched fare driverPools = do
          farePolicy <- fare
          let driverPoolRes = M.lookup farePolicy.vehicleVariant driverPools
          case driverPoolRes of
            Nothing -> mempty
            Just dp -> return (farePolicy, dp)

    buildSearchReqLocation :: (MonadGuid m, MonadTime m) => LatLong -> m DLoc.SearchReqLocation
    buildSearchReqLocation LatLong {..} = do
      id <- Id <$> generateGUID
      now <- getCurrentTime
      let createdAt = now
          updatedAt = now
      pure
        DLoc.SearchReqLocation
          { street = Nothing,
            city = Nothing,
            state = Nothing,
            country = Nothing,
            building = Nothing,
            areaCode = Nothing,
            area = Nothing,
            full_address = Nothing,
            ..
          }

    buildEstimates sReqest fromLocation driverPoolCfg org result buildEstimateFn farePolicies =
      if null farePolicies
        then do
          logDebug "Trip doesnot match any fare policy constraints."
          return []
        else do
          driverPool <- calculateDriverPool Estimate driverPoolCfg Nothing fromLocation org.id True Nothing

          logDebug $ "Search handler: driver pool " <> show driverPool

          let filteredProtoQuotes = getfilteredProtoQuotes driverPool farePolicies
          estimates <- mapM (buildEstimateFn org sReqest.pickupTime result.distance) filteredProtoQuotes
          logDebug $ "bap uri: " <> show sReqest.bapUri
          return estimates

buildSearchRes ::
  (MonadTime m) =>
  DM.Merchant ->
  LatLong ->
  LatLong ->
  Maybe [EstimateItem] ->
  Maybe [SpecialZoneQuoteInfo] ->
  Metrics.SearchMetricsMVar ->
  m DSearchRes
buildSearchRes org fromLocation toLocation estimateList specialQuoteList searchMetricsMVar = do
  now <- getCurrentTime
  pure $
    DSearchRes
      { provider = org,
        now,
        fromLocation,
        toLocation,
        estimateList,
        specialQuoteList,
        searchMetricsMVar
      }

buildSearchRequestSpecialZone ::
  ( MonadGuid m,
    MonadTime m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DSearchReq ->
  Id DM.Merchant ->
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Meters ->
  Seconds ->
  m DSRSZ.SearchRequestSpecialZone
buildSearchRequestSpecialZone DSearchReq {..} providerId fromLocation toLocation estimatedDistance estimatedDuration = do
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
        ..
      }

buildSpecialZoneQuote ::
  ( MonadGuid m,
    MonadTime m,
    MonadReader r m,
    EsqDBFlow m r,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime
  ) =>
  DSRSZ.SearchRequestSpecialZone ->
  FareParameters ->
  Id DM.Merchant ->
  Meters ->
  -- Meters ->
  DVeh.Variant ->
  Seconds ->
  m DQuoteSpecialZone.QuoteSpecialZone
buildSpecialZoneQuote productSearchRequest fareParams transporterId distance vehicleVariant duration = do
  quoteId <- Id <$> generateGUID
  now <- getCurrentTime
  let estimatedFare = fareSum fareParams
      estimatedFinishTime = fromIntegral duration `addUTCTime` now
  driverQuoteExpirationSeconds <- asks (.driverQuoteExpirationSeconds)
  let validTill = driverQuoteExpirationSeconds `addUTCTime` now
  pure
    DQuoteSpecialZone.QuoteSpecialZone
      { id = quoteId,
        searchRequestId = productSearchRequest.id,
        providerId = transporterId,
        createdAt = now,
        updatedAt = now,
        ..
      }

mkQuoteInfo :: DLoc.SearchReqLocation -> DLoc.SearchReqLocation -> UTCTime -> DQuoteSpecialZone.QuoteSpecialZone -> SpecialZoneQuoteInfo
mkQuoteInfo fromLoc toLoc startTime DQuoteSpecialZone.QuoteSpecialZone {..} = do
  let fromLocation = Maps.getCoordinates fromLoc
      toLocation = Maps.getCoordinates toLoc
  SpecialZoneQuoteInfo
    { quoteId = id,
      ..
    }
