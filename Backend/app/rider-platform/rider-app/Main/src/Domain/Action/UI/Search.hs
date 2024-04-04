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
import Domain.Action.UI.Maps (makeAutoCompleteKey)
import qualified Domain.Action.UI.Maps as DMaps
import qualified Domain.Action.UI.Serviceability as Serviceability
import qualified Domain.Types.Client as DC
import Domain.Types.HotSpot hiding (address, updatedAt)
import Domain.Types.HotSpotConfig
import qualified Domain.Types.Location as Location
import Domain.Types.LocationAddress
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonFlowStatus as DPFS
import Domain.Types.SavedReqLocation
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps
import qualified Kernel.External.Maps as MapsK
import qualified Kernel.External.Maps.Interface as MapsRoutes
import qualified Kernel.External.Maps.Interface.NextBillion as NextBillion
import qualified Kernel.External.Maps.NextBillion.Types as NBT
import qualified Kernel.External.Maps.Utils as Search
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.HotSpotConfig as QHotSpotConfig
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantState as QMMS
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.CachedQueries.MerchantConfig as QMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.SavedLocation as CSavedLocation
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonDisability as PD
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.Event
import qualified Tools.JSON as J
import qualified Tools.Maps as Maps
import Tools.Metrics
import qualified Tools.Metrics as Metrics

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
    isDestinationManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: Maybe UTCTime,
    isReallocationEnabled :: Maybe Bool,
    sessionToken :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalSearchReq = RentalSearchReq
  { origin :: SearchReqLocation,
    stops :: Maybe [SearchReqLocation],
    isSourceManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: UTCTime,
    estimatedRentalDistance :: Meters,
    estimatedRentalDuration :: Seconds,
    isReallocationEnabled :: Maybe Bool
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
    isReallocationEnabled :: Maybe Bool,
    multipleRoutes :: Maybe [Maps.RouteInfo]
  }

hotSpotUpdate ::
  ( KvDbFlow m r,
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
  ( KvDbFlow m r,
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
  ( KvDbFlow m r,
    EncFlow m r,
    ClickhouseFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds,
    HasFlowEnv m r '["collectRouteData" ::: Bool]
  ) =>
  Id Person.Person ->
  SearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe (Id DC.Client) ->
  Maybe Text ->
  m SearchRes
search personId req bundleVersion clientVersion clientConfigVersion clientId device = do
  now <- getCurrentTime
  let (riderPreferredOption, origin, stops, isSourceManuallyMoved, isSpecialLocation, startTime, isReallocationEnabled) =
        case req of
          OneWaySearch oneWayReq ->
            (SearchRequest.OneWay, oneWayReq.origin, [oneWayReq.destination], oneWayReq.isSourceManuallyMoved, oneWayReq.isSpecialLocation, fromMaybe now oneWayReq.startTime, oneWayReq.isReallocationEnabled)
          RentalSearch rentalReq ->
            (SearchRequest.Rental, rentalReq.origin, fromMaybe [] rentalReq.stops, rentalReq.isSourceManuallyMoved, rentalReq.isSpecialLocation, rentalReq.startTime, rentalReq.isReallocationEnabled)
  unless ((120 `addUTCTime` startTime) >= now) $ throwError (InvalidRequest "Ride time should only be future time") -- 2 mins buffer
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
  originCity <- validateServiceability sourceLatLong stopsLatLong person
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
  (longestRouteDistance, shortestRouteDistance, shortestRouteDuration, shortestRouteInfo, multipleRoutes) <-
    case req of
      OneWaySearch oneWayReq -> do
        riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCity.id >>= fromMaybeM (RiderConfigNotFound merchantOperatingCity.id.getId)

        let destinationLatLong = oneWayReq.destination.gps
        let request =
              Maps.GetRoutesReq
                { waypoints = NE.fromList [sourceLatLong, destinationLatLong],
                  calcPoints = True,
                  mode = Just Maps.CAR
                }
        routeResponse <- Maps.getRoutes (Just riderConfig.isAvoidToll) person.id person.merchantId (Just merchantOperatingCity.id) request

        fork "calling mmi directions api" $ do
          let collectMMIData = fromMaybe False riderConfig.collectMMIRouteData
          when collectMMIData $ do
            mmiConfigs <- QMSC.findByMerchantOpCityIdAndService person.merchantId merchantOperatingCity.id (DMSC.MapsService MapsK.MMI) >>= fromMaybeM (MerchantServiceConfigNotFound person.merchantId.getId "Maps" "MMI")
            case mmiConfigs.serviceConfig of
              DMSC.MapsServiceConfig mapsCfg -> do
                routeResp <- MapsRoutes.getRoutes True mapsCfg request
                logInfo $ "MMI route response: " <> show routeResp
                let routeData = RouteDataEvent (Just $ show MapsK.MMI) (map show routeResp) (Just searchRequestId) merchant.id merchantOperatingCity.id now now
                triggerRouteDataEvent routeData
              _ -> logInfo "MapsServiceConfig config not found for MMI"

        fork "calling next billion directions api" $ do
          shouldCollectRouteData <- asks (.collectRouteData)
          when shouldCollectRouteData $ do
            nextBillionConfigs <- QMSC.findByMerchantOpCityIdAndService person.merchantId merchantOperatingCity.id (DMSC.MapsService MapsK.NextBillion) >>= fromMaybeM (MerchantServiceConfigNotFound person.merchantId.getId "Maps" "NextBillion")
            case nextBillionConfigs.serviceConfig of
              DMSC.MapsServiceConfig mapsCfg -> do
                case mapsCfg of
                  MapsK.NextBillionConfig msc -> do
                    let nbFastestReq = NBT.GetRoutesRequest request.waypoints (Just True) (Just 3) (Just "fastest") Nothing
                    let nbShortestReq = NBT.GetRoutesRequest request.waypoints (Just True) (Just 3) (Just "shortest") (Just "flexible")
                    nbFastestRouteResponse <- NextBillion.getRoutesWithExtraParameters msc nbFastestReq
                    nbShortestRouteResponse <- NextBillion.getRoutesWithExtraParameters msc nbShortestReq
                    logInfo $ "NextBillion route responses: " <> show nbFastestRouteResponse <> "\n" <> show nbShortestRouteResponse
                    let fastRouteData = RouteDataEvent (Just "NB_Fastest") (map show nbFastestRouteResponse) (Just searchRequestId) (merchant.id) (merchantOperatingCity.id) now now
                    let shortRouteData = RouteDataEvent (Just "NB_Shortest") (map show nbShortestRouteResponse) (Just searchRequestId) (merchant.id) (merchantOperatingCity.id) now now
                    triggerRouteDataEvent fastRouteData
                    triggerRouteDataEvent shortRouteData
                  _ -> logInfo "No NextBillion config"
              _ -> logInfo "NextBillion route not found"

        fork "Updating autocomplete data in search" $ do
          whenJust oneWayReq.sessionToken $ \token -> do
            let toCollectData = fromMaybe False riderConfig.collectAutoCompleteData
            when toCollectData $ do
              let pickUpKey = makeAutoCompleteKey token (show DMaps.PICKUP)
              let dropKey = makeAutoCompleteKey token (show DMaps.DROP)
              pickupRecord :: Maybe AutoCompleteEventData <- Redis.safeGet pickUpKey
              dropRecord :: Maybe AutoCompleteEventData <- Redis.safeGet dropKey
              whenJust pickupRecord $ \record -> do
                let updatedRecord = AutoCompleteEventData record.autocompleteInputs record.customerId record.id (oneWayReq.isSourceManuallyMoved) (Just searchRequestId) record.searchType record.sessionToken record.merchantId record.merchantOperatingCityId record.originLat record.originLon record.createdAt now
                -- let updatedRecord = record {DTA.searchRequestId = Just searchRequestId, DTA.isLocationSelectedOnMap = oneWayReq.isSourceManuallyMoved, DTA.updatedAt = now}
                triggerAutoCompleteEvent updatedRecord
              whenJust dropRecord $ \record -> do
                let updatedRecord = AutoCompleteEventData record.autocompleteInputs record.customerId record.id (oneWayReq.isDestinationManuallyMoved) (Just searchRequestId) record.searchType record.sessionToken record.merchantId record.merchantOperatingCityId record.originLat record.originLon record.createdAt now
                -- let updatedRecord = record {DTA.searchRequestId = Just searchRequestId, DTA.isLocationSelectedOnMap = oneWayReq.isDestinationManuallyMoved, DTA.updatedAt = now}
                triggerAutoCompleteEvent updatedRecord

        let distanceWeightage = riderConfig.distanceWeightage
            durationWeightage = 100 - distanceWeightage
            (shortestRouteInfo, shortestRouteIndex) = Search.getEfficientRouteInfo routeResponse distanceWeightage durationWeightage
            longestRouteDistance = (.distance) =<< Search.getLongestRouteDistance routeResponse
            shortestRouteDistance = (.distance) =<< shortestRouteInfo
            shortestRouteDuration = (.duration) =<< shortestRouteInfo
        return (longestRouteDistance, shortestRouteDistance, shortestRouteDuration, shortestRouteInfo, Just $ Search.updateEfficientRoutePosition routeResponse shortestRouteIndex)
      RentalSearch rentalReq -> return (Nothing, Just rentalReq.estimatedRentalDistance, Just rentalReq.estimatedRentalDuration, Just (RouteInfo (Just rentalReq.estimatedRentalDuration) (Just rentalReq.estimatedRentalDistance) Nothing Nothing [] []), Nothing)

  fromLocation <- buildSearchReqLoc origin
  stopLocations <- buildSearchReqLoc `mapM` stops
  searchRequest <-
    buildSearchRequest
      searchRequestId
      clientId
      person
      fromLocation
      merchantOperatingCity
      (listToMaybe stopLocations) --- Take first stop, handle multiple stops later
      (metersToDistance <$> longestRouteDistance)
      (metersToDistance <$> shortestRouteDistance)
      startTime
      bundleVersion
      clientVersion
      clientConfigVersion
      device
      tag
      shortestRouteDuration
      riderPreferredOption
  Metrics.incrementSearchRequestCount merchant.name merchantOperatingCity.id.getId

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
      Serviceability.NearestOperatingAndCurrentCity {nearestOperatingCity, currentCity} <- Serviceability.getNearestOperatingAndCurrentCity (.origin) (person'.id, person'.merchantId) False origin
      stopCitiesAndStates <- traverse (Serviceability.getNearestOperatingAndCurrentCity (.destination) (person'.id, person'.merchantId) False) stops
      mbMerchantState <- QMMS.findByMerchantIdAndState person'.merchantId currentCity.state
      let allowedStates = maybe [currentCity.state] (.allowedDestinationStates) mbMerchantState
      if all (\d -> d.currentCity.state `elem` allowedStates) stopCitiesAndStates
        then return nearestOperatingCity.city
        else throwError RideNotServiceable

buildSearchRequest ::
  ( (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds, "version" ::: DeploymentVersion]),
    EsqDBFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  Id SearchRequest.SearchRequest ->
  Maybe (Id DC.Client) ->
  DPerson.Person ->
  Location.Location ->
  DMOC.MerchantOperatingCity ->
  Maybe Location.Location ->
  Maybe Distance ->
  Maybe Distance ->
  UTCTime ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Seconds ->
  SearchRequest.RiderPreferredOption ->
  m SearchRequest.SearchRequest
buildSearchRequest searchRequestId mbClientId person pickup merchantOperatingCity mbDrop mbMaxDistance mbDistance startTime bundleVersion clientVersion clientConfigVersion device disabilityTag duration riderPreferredOption = do
  now <- getCurrentTime
  validTill <- getSearchRequestExpiry startTime
  deploymentVersion <- asks (.version)
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
        clientId = mbClientId,
        createdAt = now,
        estimatedRideDuration = duration,
        device = device,
        clientBundleVersion = bundleVersion,
        clientSdkVersion = clientVersion,
        clientDevice = getDeviceFromText device,
        clientConfigVersion = clientConfigVersion,
        backendConfigVersion = Nothing,
        backendAppVersion = Just deploymentVersion.getDeploymentVersion,
        language = person.language,
        disabilityTag = disabilityTag,
        customerExtraFee = Nothing,
        autoAssignEnabled = Nothing,
        autoAssignEnabledV2 = Nothing,
        availablePaymentMethods = [],
        selectedPaymentMethodId = Nothing,
        riderPreferredOption -- this is just to store the rider preference for the ride type to handle backward compatibility
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
