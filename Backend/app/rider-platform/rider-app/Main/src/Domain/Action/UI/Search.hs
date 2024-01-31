{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Search where

import Control.Monad
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import Data.OpenApi hiding (Header)
import qualified Data.OpenApi as OpenApi hiding (Header)
import Domain.Action.UI.HotSpot
import qualified Domain.Action.UI.Serviceability as Serviceability
import Domain.Types.HotSpot hiding (address, updatedAt)
import Domain.Types.HotSpotConfig
import qualified Domain.Types.Location as Location
import Domain.Types.LocationAddress
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.NextBillionData as DNB
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Domain.Types.SavedReqLocation
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps
import qualified Kernel.External.Maps as MapsK
import qualified Kernel.External.Maps.Interface.NextBillion as NextBillion
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.HotSpotConfig as QHotSpotConfig
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.MerchantConfig as QMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.SavedLocation as CSavedLocation
import qualified Storage.Queries.NextBillionData as QNB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person.PersonDisability as PD
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.Event
import qualified Tools.JSON as J
import qualified Tools.Maps as Maps
import Tools.Metrics
import qualified Tools.Metrics as Metrics
import qualified Tools.Search as Search

data SearchReq = OneWaySearch OneWaySearchReq | RentalSearch RentalSearchReq
  deriving (Generic, Show)

instance ToJSON SearchReq where
  toJSON = genericToJSON fareProductOptions

instance FromJSON SearchReq where
  parseJSON = genericParseJSON fareProductOptions

instance ToSchema SearchReq where
  declareNamedSchema = genericDeclareNamedSchema fareProductSchemaOptions

fareProductOptions :: Options
fareProductOptions =
  defaultOptions
    { sumEncoding = J.fareProductTaggedObject,
      constructorTagModifier = fareProductConstructorModifier
    }

fareProductSchemaOptions :: OpenApi.SchemaOptions
fareProductSchemaOptions =
  OpenApi.defaultSchemaOptions
    { OpenApi.sumEncoding = J.fareProductTaggedObject,
      OpenApi.constructorTagModifier = fareProductConstructorModifier
    }

data SearchResp = SearchResp
  { searchId :: Id SearchRequest.SearchRequest,
    searchExpiry :: UTCTime,
    routeInfo :: Maybe Maps.RouteInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWaySearch" -> "ONE_WAY"
  "RentalSearch" -> "RENTAL"
  x -> x

data OneWaySearchReq = OneWaySearchReq
  { origin :: SearchReqLocation,
    destination :: SearchReqLocation,
    isSourceManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    isReallocationEnabled :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalSearchReq = RentalSearchReq
  { origin :: SearchReqLocation,
    stops :: Maybe [SearchReqLocation],
    isSourceManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: UTCTime,
    estimatedRentalDistance :: Meters,
    estimatedRentalDuration :: Seconds
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data SearchRes = SearchRes
  { origin :: SearchReqLocation,
    stops :: [SearchReqLocation],
    startTime :: UTCTime,
    searchId :: Id DSearchReq.SearchRequest,
    now :: UTCTime,
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime,
    merchant :: DM.Merchant,
    city :: City,
    customerLanguage :: Maybe Maps.Language,
    disabilityTag :: Maybe Text,
    device :: Maybe Text,
    distance :: Maybe Meters,
    duration :: Maybe Seconds,
    shortestRouteInfo :: Maybe Maps.RouteInfo,
    phoneNumber :: Maybe Text,
    isReallocationEnabled :: Maybe Bool
  }

hotSpotUpdate ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  Id Merchant ->
  Maybe SavedReqLocation ->
  SearchReqLocation ->
  Maybe Bool ->
  m ()
hotSpotUpdate merchantId mbFavourite origin isSourceManuallyMoved = case mbFavourite of
  Just SavedReqLocation {..} ->
    frequencyUpdator merchantId origin.gps (Just origin.address) (bool NonManualSaved ManualSaved (isMoved == Just True))
  Nothing ->
    frequencyUpdator merchantId origin.gps (Just origin.address) (bool NonManualPickup ManualPickup (isSourceManuallyMoved == Just True))

updateForSpecialLocation ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  Id Merchant ->
  SearchReqLocation ->
  Maybe Bool ->
  m ()
updateForSpecialLocation merchantId origin mbIsSpecialLocation = do
  case mbIsSpecialLocation of
    Just isSpecialLocation -> do
      when isSpecialLocation $ frequencyUpdator merchantId origin.gps (Just origin.address) SpecialLocation
    Nothing -> do
      specialLocationBody <- QSpecialLocation.findSpecialLocationByLatLong origin.gps
      case specialLocationBody of
        Just _ -> frequencyUpdator merchantId origin.gps (Just origin.address) SpecialLocation
        Nothing -> return ()

search ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    EsqDBFlow m r,
    HasBAPMetrics m r,
    MonadFlow m,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  Id Person.Person ->
  SearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  m SearchRes
search personId req bundleVersion clientVersion device = do
  now <- getCurrentTime
  let (origin, stops, isSourceManuallyMoved, isSpecialLocation, startTime, isReallocationEnabled) =
        case req of
          OneWaySearch oneWayReq ->
            (oneWayReq.origin, [oneWayReq.destination], oneWayReq.isSourceManuallyMoved, oneWayReq.isSpecialLocation, now, oneWayReq.isReallocationEnabled)
          RentalSearch rentalReq ->
            (rentalReq.origin, fromMaybe [] rentalReq.stops, rentalReq.isSourceManuallyMoved, rentalReq.isSpecialLocation, rentalReq.startTime, Nothing)

  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  phoneNumber <- mapM decrypt person.mobileNumber

  tag <- case person.hasDisability of
    Just True -> B.runInReplica $ fmap (.tag) <$> PD.findByPersonId personId
    _ -> return Nothing

  merchant <- QMerc.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  mbFavourite <- CSavedLocation.findByLatLonAndRiderId personId origin.gps
  HotSpotConfig {..} <- QHotSpotConfig.findConfigByMerchantId merchant.id >>= fromMaybeM (InternalError "config not found for merchant")

  let sourceLatLong = origin.gps
  let stopsLatLong = map (.gps) stops
  originCity <- validateServiceability sourceLatLong stopsLatLong person <&> fromMaybe merchant.defaultCity <$> (.city)
  -- merchant operating city of search-request-origin-location

  when (shouldSaveSearchHotSpot && shouldTakeHotSpot) do
    fork "ride search geohash frequencyUpdater" $ do
      _ <- hotSpotUpdate person.merchantId mbFavourite origin isSourceManuallyMoved
      updateForSpecialLocation person.merchantId origin isSpecialLocation

  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id originCity
      >>= fromMaybeM
        ( MerchantOperatingCityNotFound $
            "merchantId: " <> merchant.id.getId <> " ,city: " <> show originCity
        )
  searchRequestId <- generateGUID
  (longestRouteDistance, shortestRouteDistance, shortestRouteDuration, shortestRouteInfo) <-
    case req of
      OneWaySearch oneWayReq -> do
        let destinationLatLong = oneWayReq.destination.gps
        let request =
              Maps.GetRoutesReq
                { waypoints = NE.fromList [sourceLatLong, destinationLatLong],
                  calcPoints = True,
                  mode = Just Maps.CAR
                }
        routeResponse <- Maps.getRoutes person.id person.merchantId (Just merchantOperatingCity.id) (Just searchRequestId) request
        fork "calling next billion directions api" $ do
          nextBillionConfigs <- QMSC.findByMerchantIdAndService person.merchantId (DMSC.MapsService MapsK.NextBillion) >>= fromMaybeM (MerchantServiceConfigNotFound person.merchantId.getId "Maps" "NextBillion")
          case nextBillionConfigs.serviceConfig of
            DMSC.MapsServiceConfig mapsCfg -> do
              case mapsCfg of
                MapsK.NextBillionConfig msc -> do
                  nextBillionRouteResponse <- NextBillion.getRoutes msc request
                  logInfo $ "NextBillion route response: " <> show nextBillionRouteResponse
                  let routeData =
                        DNB.NextBillionData
                          { routes = map show nextBillionRouteResponse,
                            searchRequestId = searchRequestId,
                            merchantId = Just merchant.id,
                            merchantOperatingCityId = Just merchantOperatingCity.id,
                            createdAt = now,
                            updatedAt = now
                          }
                  QNB.create routeData
                _ -> logInfo "No NextBillion config"
            _ -> logInfo "NextBillion route not found"

        let durationWeightage = 100 - merchant.distanceWeightage
        let shortestRouteInfo = getEfficientRouteInfo routeResponse merchant.distanceWeightage durationWeightage
        let longestRouteDistance = (.distance) =<< getLongestRouteDistance routeResponse
        let shortestRouteDistance = (.distance) =<< shortestRouteInfo
        let shortestRouteDuration = (.duration) =<< shortestRouteInfo
        return (longestRouteDistance, shortestRouteDistance, shortestRouteDuration, shortestRouteInfo)
      RentalSearch rentalReq -> return (Nothing, Just rentalReq.estimatedRentalDistance, Just rentalReq.estimatedRentalDuration, Nothing)

  fromLocation <- buildSearchReqLoc origin
  stopLocations <- buildSearchReqLoc `mapM` stops
  searchRequest <-
    buildSearchRequest
      searchRequestId
      person
      fromLocation
      merchantOperatingCity
      (listToMaybe stopLocations) --- Take first stop, handle multiple stops later
      (metersToHighPrecMeters <$> longestRouteDistance)
      (metersToHighPrecMeters <$> shortestRouteDistance)
      startTime
      bundleVersion
      clientVersion
      device
      tag
      shortestRouteDuration
  Metrics.incrementSearchRequestCount merchant.name

  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  triggerSearchEvent SearchEventData {searchRequest = searchRequest}
  _ <- QSearchRequest.createDSReq searchRequest
  _ <- QPFS.updateStatus person.id DPFS.SEARCHING {requestId = searchRequest.id, validTill = searchRequest.validTill}
  QPFS.clearCache person.id
  let dSearchRes =
        SearchRes
          { searchId = searchRequest.id,
            gatewayUrl = merchant.gatewayUrl,
            searchRequestExpiry = searchRequest.validTill,
            customerLanguage = searchRequest.language,
            city = originCity,
            distance = shortestRouteDistance,
            duration = shortestRouteDuration,
            disabilityTag = tag,
            ..
          }
  fork "updating search counters" $ do
    merchantConfigs <- QMC.findAllByMerchantOperatingCityId person.merchantOperatingCityId
    SMC.updateSearchFraudCounters personId merchantConfigs
    mFraudDetected <- SMC.anyFraudDetected personId merchantOperatingCity.id merchantConfigs
    whenJust mFraudDetected $ \mc -> SMC.blockCustomer personId (Just mc.id)
  return dSearchRes
  where
    validateServiceability origin stops person' = do
      originServiceability <- Serviceability.checkServiceabilityAndGetCity (.origin) (person'.id, person'.merchantId) False origin
      stopsServiceabilities <- traverse (Serviceability.checkServiceabilityAndGetCity (.origin) (person'.id, person'.merchantId) False) stops
      if originServiceability.serviceable && (all (.serviceable) stopsServiceabilities)
        then
          if all (\s -> s.city == originServiceability.city) stopsServiceabilities
            then pure originServiceability
            else throwError $ InvalidRequest "All stops should be inside the source city"
        else throwError RideNotServiceable

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

buildSearchRequest ::
  ( (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]),
    EsqDBFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  Id SearchRequest.SearchRequest ->
  DPerson.Person ->
  Location.Location ->
  DMOC.MerchantOperatingCity ->
  Maybe Location.Location ->
  Maybe HighPrecMeters ->
  Maybe HighPrecMeters ->
  UTCTime ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Seconds ->
  m SearchRequest.SearchRequest
buildSearchRequest searchRequestId person pickup merchantOperatingCity mbDrop mbMaxDistance mbDistance startTime bundleVersion clientVersion device disabilityTag duration = do
  now <- getCurrentTime
  validTill <- getSearchRequestExpiry startTime
  return
    SearchRequest.SearchRequest
      { id = searchRequestId,
        startTime,
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
    getSearchRequestExpiry time = do
      searchRequestExpiry <- maybe 1800 fromIntegral <$> asks (.searchRequestExpiry)
      pure $ addUTCTime (fromInteger searchRequestExpiry) time

data SearchReqLocation = SearchReqLocation
  { gps :: LatLong,
    address :: LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

buildSearchReqLoc :: MonadFlow m => SearchReqLocation -> m Location.Location
buildSearchReqLoc SearchReqLocation {..} = do
  now <- getCurrentTime
  locId <- generateGUID
  return
    Location.Location
      { id = locId,
        lat = gps.lat,
        lon = gps.lon,
        address = address,
        createdAt = now,
        updatedAt = now
      }

makeSearchReqLoc' :: Location.Location -> SearchReqLocation
makeSearchReqLoc' Location.Location {..} =
  SearchReqLocation
    { gps = LatLong lat lon,
      ..
    }
