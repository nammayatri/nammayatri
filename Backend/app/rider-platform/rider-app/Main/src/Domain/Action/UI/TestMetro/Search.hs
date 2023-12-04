{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.TestMetro.Search where

import Control.Monad
import qualified Data.List.NonEmpty as NE
import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Action.UI.Serviceability as Serviceability
import qualified Domain.Types.Location as Location
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.SearchRequest as DSearchReq
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import qualified Tools.Maps as Maps
import Tools.Metrics
import qualified Tools.Search as Search

data MetroSearchReq = MetroSearchReq
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data MetroSearchRes = MetroSearchRes
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation,
    searchId :: Id DSearchReq.SearchRequest,
    now :: UTCTime,
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime,
    merchant :: DM.Merchant,
    city :: City,
    customerLanguage :: Maybe Maps.Language,
    disabilityTag :: Maybe Text,
    device :: Maybe Text,
    shortestRouteInfo :: Maybe Maps.RouteInfo
  }

metroSearch ::
  ( MonadFlow m,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]
  ) =>
  Id Person.Person ->
  MetroSearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  m MetroSearchRes
metroSearch personId req bundleVersion clientVersion device = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  let tag = Nothing
  merchant <- QMerc.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  -- mbFavourite <- CSavedLocation.findByLatLonAndRiderId personId req.origin.gps
  -- HotSpotConfig {..} <- QHotSpotConfig.findConfigByMerchantId merchant.id >>= fromMaybeM (InternalError "config not found for merchant")

  -- when shouldTakeHotSpot do
  --   _ <- hotSpotUpdate person.merchantId mbFavourite req
  --   updateForSpecialLocation person.merchantId req

  let sourceLatLong = req.origin.gps
  let destinationLatLong = req.destination.gps

  originCity <- validateServiceability sourceLatLong destinationLatLong person <&> fromMaybe merchant.defaultCity <$> (.city)
  -- merchant operating city of search-request-origin-location
  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id originCity
      >>= fromMaybeM
        ( MerchantOperatingCityNotFound $
            "merchantId: " <> merchant.id.getId <> " ,city: " <> show originCity
        )
  let request =
        Maps.GetRoutesReq
          { waypoints = NE.fromList [sourceLatLong, destinationLatLong],
            calcPoints = True,
            mode = Just Maps.CAR
          }
  routeResponse <- Maps.getRoutes person.id person.merchantId (Just merchantOperatingCity.id) request
  let durationWeightage = 100 - merchant.distanceWeightage
  let shortestRouteInfo = getEfficientRouteInfo routeResponse merchant.distanceWeightage durationWeightage
  let longestRouteDistance = (.distance) =<< getLongestRouteDistance routeResponse
  let shortestRouteDistance = (.distance) =<< shortestRouteInfo
  let shortestRouteDuration = (.duration) =<< shortestRouteInfo

  now <- getCurrentTime
  fromLocation <- buildMetroSearchReqLoc now dummyPickupLocAddress req.origin
  toLocation <- buildMetroSearchReqLoc now dummyDropLocAddress req.destination
  searchRequest <- buildMetroSearchRequest person merchantOperatingCity fromLocation (Just toLocation) (metersToHighPrecMeters <$> longestRouteDistance) (metersToHighPrecMeters <$> shortestRouteDistance) now bundleVersion clientVersion device tag shortestRouteDuration
  -- Metrics.incrementSearchRequestCount merchant.name
  -- let txnId = getId (searchRequest.id)
  -- Metrics.startSearchMetrics merchant.name txnId
  -- triggerSearchEvent SearchEventData {searchRequest = searchRequest}
  _ <- QSearchRequest.createDSReq searchRequest
  _ <- QPFS.updateStatus person.id DPFS.SEARCHING {requestId = searchRequest.id, validTill = searchRequest.validTill}
  QPFS.clearCache person.id
  let dSearchRes =
        MetroSearchRes
          { origin = req.origin,
            destination = req.destination,
            searchId = searchRequest.id,
            now = now,
            gatewayUrl = merchant.gatewayUrl,
            searchRequestExpiry = searchRequest.validTill,
            customerLanguage = searchRequest.language,
            city = originCity,
            disabilityTag = tag,
            device,
            shortestRouteInfo,
            ..
          }
  -- fork "updating search counters" $ do
  --   merchantConfigs <- QMC.findAllByMerchantOperatingCityId person.merchantOperatingCityId
  --   SMC.updateSearchFraudCounters personId merchantConfigs
  --   mFraudDetected <- SMC.anyFraudDetected personId merchantOperatingCity.id merchantConfigs
  --   whenJust mFraudDetected $ \mc -> SMC.blockCustomer personId (Just mc.id)
  return dSearchRes
  where
    validateServiceability origin destination person' = do
      originServiceability <- Serviceability.checkServiceabilityAndGetCity (.origin) (person'.id, person'.merchantId) origin False
      destinationServiceability <- Serviceability.checkServiceabilityAndGetCity (.destination) (person'.id, person'.merchantId) destination False
      if originServiceability.serviceable && destinationServiceability.serviceable && originServiceability.city == destinationServiceability.city
        then pure originServiceability
        else throwError RideNotServiceable

buildMetroSearchRequest ::
  ( (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]),
    EsqDBFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  Person.Person ->
  DMOC.MerchantOperatingCity ->
  Location.Location ->
  Maybe Location.Location ->
  Maybe HighPrecMeters ->
  Maybe HighPrecMeters ->
  UTCTime ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Seconds ->
  m DSearchReq.SearchRequest
buildMetroSearchRequest person merchantOperatingCity pickup mbDrop mbMaxDistance mbDistance now bundleVersion clientVersion device disabilityTag duration = do
  searchRequestId <- generateGUID
  validTill <- getSearchRequestExpiry now
  return
    DSearchReq.SearchRequest
      { id = searchRequestId,
        startTime = now,
        validTill = validTill,
        riderId = person.id,
        fromLocation = pickup,
        toLocation = mbDrop,
        distance = mbDistance,
        maxDistance = mbMaxDistance,
        merchantId = person.merchantId,
        merchantOperatingCityId = merchantOperatingCity.id,
        createdAt = now,
        estimatedRideDuration = duration,
        device = device,
        bundleVersion = bundleVersion,
        clientVersion = clientVersion,
        language = person.language,
        disabilityTag = disabilityTag,
        customerExtraFee = Nothing,
        autoAssignEnabled = Nothing,
        autoAssignEnabledV2 = Nothing,
        availablePaymentMethods = [],
        selectedPaymentMethodId = Nothing
      }
  where
    getSearchRequestExpiry :: (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]) => UTCTime -> m UTCTime
    getSearchRequestExpiry _ = do
      searchRequestExpiry <- maybe 1800 fromIntegral <$> asks (.searchRequestExpiry)
      pure $ addUTCTime (fromInteger searchRequestExpiry) now

buildMetroSearchReqLoc :: MonadFlow m => UTCTime -> LocationAddress -> DSearch.SearchReqLocation -> m Location.Location
buildMetroSearchReqLoc now address' DSearch.SearchReqLocation {..} = do
  locId <- generateGUID
  return
    Location.Location
      { id = locId,
        lat = gps.lat,
        lon = gps.lon,
        address = address',
        createdAt = now,
        updatedAt = now
      }

dummyPickupLocAddress :: LocationAddress
dummyPickupLocAddress =
  LocationAddress
    { door = Just "817",
      building = Just "20th Main Rd",
      street = Just "Koramangala 8th Block",
      area = Just "Koramangala, Koramangala 8th Block, 20th Main Rd",
      areaCode = Just "560095",
      city = Just "Bengaluru",
      state = Just "Karnataka 560095",
      country = Just "India",
      ward = Nothing,
      placeId = Nothing
    }

dummyDropLocAddress :: LocationAddress
dummyDropLocAddress =
  LocationAddress
    { door = Just "#444",
      building = Just "Juspay Apartments",
      street = Just "18th Main",
      area = Just "6th Block Koramangala",
      areaCode = Just "560047",
      city = Just "Bangalore",
      state = Just "Karnataka",
      country = Just "India",
      ward = Nothing,
      placeId = Nothing
    }

getLongestRouteDistance :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getLongestRouteDistance [] = Nothing
getLongestRouteDistance (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteresult <- getLongestRouteDistance routeInfoArray
      Just $ comparator' routeInfo restRouteresult
  where
    comparator' route1 route2 =
      if route1.distance > route2.distance
        then route1
        else route2

getEfficientRouteInfo :: [Maps.RouteInfo] -> Int -> Int -> Maybe Maps.RouteInfo
getEfficientRouteInfo [] _ _ = Nothing
getEfficientRouteInfo routeInfos distanceWeight durationWeight = do
  let minD = Search.minDistance routeInfos
      minDur = Search.minDuration routeInfos
      normalizedInfos = Search.normalizeArr (Just minD) (Just minDur) routeInfos
      resultInfoIdx = Search.findMaxWeightedInfoIdx (fromIntegral distanceWeight) (fromIntegral durationWeight) normalizedInfos
  if resultInfoIdx < length routeInfos
    then Just (routeInfos !! resultInfoIdx)
    else Nothing
