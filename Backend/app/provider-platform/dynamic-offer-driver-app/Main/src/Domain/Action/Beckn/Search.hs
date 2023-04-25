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
    EstimateInfo (..),
    SpecialZoneQuoteInfo (..),
    handler,
    validateRequest,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Domain.Types.Estimate as DEst
import Domain.Types.FareParameters
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.DriverPoolConfig (DriverPoolConfig)
import qualified Domain.Types.QuoteSpecialZone as DQuoteSpecialZone
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequestSpecialZone as DSRSZ
import qualified Domain.Types.Vehicle as DVeh
import Environment
import EulerHS.Prelude (Alternative (empty), whenJustM)
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.Prelude
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.DriverPool hiding (lat, lon)
import qualified SharedLogic.Estimate as SHEst
import SharedLogic.FareCalculator
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.CachedQueries.Merchant.TransporterConfig as CTC
import qualified Storage.Queries.Estimate as QEst
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
    routeDistance :: Maybe Meters,
    routeDuration :: Maybe Seconds,
    device :: Maybe Text
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
    estimateList :: Maybe [EstimateInfo],
    specialQuoteList :: Maybe [SpecialZoneQuoteInfo],
    searchMetricsMVar :: Metrics.RequestMetricsMVar
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
handler org sReq = do
  searchMetricsMVar <- Metrics.startSearchMetrics org.name
  let fromLocationLatLong = sReq.pickupLocation
      toLocationLatLong = sReq.dropLocation
      merchantId = org.id
  result <- getDistanceAndDuration merchantId fromLocationLatLong toLocationLatLong sReq.routeDistance sReq.routeDuration
  CD.cacheDistance sReq.transactionId (result.distance, result.duration)
  Redis.setExp (CD.deviceKey sReq.transactionId) sReq.device 120
  logDebug $ "distance: " <> show result.distance
  mbSpecialLocation <- QSpecialLocation.findSpecialLocationByLatLong fromLocationLatLong

  (quotes, mbEstimateInfos) <-
    if isJust mbSpecialLocation
      then do
        whenJustM
          (QSearchRequestSpecialZone.findByMsgIdAndBapIdAndBppId sReq.messageId sReq.bapId merchantId)
          (\_ -> throwError $ InvalidRequest "Duplicate Search request")
        fromLocation <- buildSearchReqLocation fromLocationLatLong
        toLocation <- buildSearchReqLocation toLocationLatLong
        searchRequestSpecialZone <- buildSearchRequestSpecialZone sReq merchantId fromLocation toLocation result.distance result.duration
        Esq.runTransaction $ do
          QSearchRequestSpecialZone.create searchRequestSpecialZone
        now <- getCurrentTime
        listOfSpecialZoneQuotes <- do
          allFarePolicies <- FarePolicyS.findAllByMerchantId org.id
          let farePolicies = selectFarePolicy result.distance allFarePolicies
          let listOfVehicleVariants = listVehicleVariantHelper farePolicies
          for listOfVehicleVariants $ \farePolicy -> do
            fareParams <-
              calculateFareParameters
                CalculateFareParametersParams
                  { farePolicy = farePolicy,
                    distance = result.distance,
                    rideTime = sReq.pickupTime,
                    waitingTime = Nothing,
                    driverSelectedFare = Nothing,
                    customerExtraFee = Nothing
                  }
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
        estimateInfos <- do
          allFarePolicies <- FarePolicyS.findAllByMerchantId org.id
          let farePolicies = selectFarePolicy result.distance allFarePolicies
          buildEstimatesInfos fromLocationLatLong driverPoolCfg result farePolicies
        return (Nothing, Just estimateInfos)
  buildSearchRes org fromLocationLatLong toLocationLatLong mbEstimateInfos quotes searchMetricsMVar
  where
    listVehicleVariantHelper farePolicy = catMaybes $ everyPossibleVariant <&> \var -> find ((== var) . (.vehicleVariant)) farePolicy

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

    buildSearchReqLocation :: (MonadGuid m, MonadTime m) => LatLong -> m DLoc.SearchReqLocation
    buildSearchReqLocation LatLong {..} = do
      id <- Id <$> generateGUID
      now <- getCurrentTime
      let createdAt = now
          updatedAt = now
      pure
        DLoc.SearchReqLocation
          { street = Nothing,
            door = Nothing,
            city = Nothing,
            state = Nothing,
            country = Nothing,
            building = Nothing,
            areaCode = Nothing,
            area = Nothing,
            full_address = Nothing,
            ..
          }

    buildEstimatesInfos ::
      LatLong ->
      DriverPoolConfig ->
      DistanceAndDuration ->
      [DFP.FarePolicy] ->
      Flow [EstimateInfo]
    buildEstimatesInfos fromLocation driverPoolCfg result farePolicies = do
      let merchantId = org.id
      if null farePolicies
        then do
          logDebug "Trip doesnot match any fare policy constraints."
          return []
        else do
          driverPoolNotOnRide <- calculateDriverPool Estimate driverPoolCfg Nothing fromLocation merchantId True Nothing
          driverPoolCurrentlyOnRide <-
            if null driverPoolNotOnRide
              then do
                let reducedRadiusValue = driverPoolCfg.radiusShrinkValueForDriversOnRide
                transporter <- CTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigDoesNotExist merchantId.getId)
                if transporter.includeDriverCurrentlyOnRide
                  then calculateDriverPoolCurrentlyOnRide Estimate driverPoolCfg Nothing fromLocation merchantId Nothing reducedRadiusValue
                  else pure []
              else pure []
          let driverPool = driverPoolNotOnRide ++ map changeIntoDriverPoolResult driverPoolCurrentlyOnRide
          logDebug $ "Search handler: driver pool " <> show driverPool

          let onlyFPWithDrivers = filter (\fp -> isJust (find (\dp -> dp.variant == fp.vehicleVariant) driverPool)) farePolicies
          estimates <- mapM (SHEst.buildEstimate sReq.transactionId sReq.pickupTime result.distance) onlyFPWithDrivers
          Esq.runTransaction $ do
            QEst.createMany estimates

          let mapOfDPRByVariant = foldl (\m dpr -> M.insertWith (<>) dpr.variant (pure dpr) m) mempty driverPool
              tuplesOfEstAndDBR :: [(DEst.Estimate, NonEmpty DriverPoolResult)] = zipMatched estimates mapOfDPRByVariant
          logDebug $ "bap uri: " <> show sReq.bapUri
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
  LatLong ->
  Maybe [EstimateInfo] ->
  Maybe [SpecialZoneQuoteInfo] ->
  Metrics.RequestMetricsMVar ->
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
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
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

mkQuoteInfo :: DLoc.SearchReqLocation -> DLoc.SearchReqLocation -> UTCTime -> DQuoteSpecialZone.QuoteSpecialZone -> SpecialZoneQuoteInfo
mkQuoteInfo fromLoc toLoc startTime DQuoteSpecialZone.QuoteSpecialZone {..} = do
  let fromLocation = Maps.getCoordinates fromLoc
      toLocation = Maps.getCoordinates toLoc
  SpecialZoneQuoteInfo
    { quoteId = id,
      ..
    }

validateRequest :: Id DM.Merchant -> DSearchReq -> Flow DM.Merchant
validateRequest merchantId sReq = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless org.enabled $ throwError AgencyDisabled
  let fromLocationLatLong = sReq.pickupLocation
      toLocationLatLong = sReq.dropLocation
  unlessM (rideServiceable org.geofencingConfig QGeometry.someGeometriesContain fromLocationLatLong (Just toLocationLatLong)) $
    throwError RideNotServiceable
  return org
