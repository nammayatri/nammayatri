{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Search where

import qualified BecknV2.OnDemand.Tags as Beckn
import Control.Monad
import Data.Aeson
import Data.Default.Class
import qualified Data.List.NonEmpty as NE
import Data.OpenApi hiding (Header)
import qualified Data.OpenApi as OpenApi hiding (Header)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE
import Domain.Action.UI.HotSpot
import Domain.Action.UI.Maps (makeAutoCompleteKey)
import qualified Domain.Action.UI.Maps as DMaps
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
import Domain.Types.RiderConfig
import Domain.Types.SavedReqLocation
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest as SearchRequest
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps
import qualified Kernel.External.Maps as MapsK
import qualified Kernel.External.Maps.Interface as MapsRoutes
import qualified Kernel.External.Maps.Interface.NextBillion as NextBillion
import qualified Kernel.External.Maps.NextBillion.Types as NBT
import qualified Kernel.External.Maps.Utils as Search
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.MerchantConfig as SMC
import qualified SharedLogic.Serviceability as Serviceability
import qualified Storage.CachedQueries.HotSpotConfig as QHotSpotConfig
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.CachedQueries.MerchantConfig as QMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.SavedReqLocation as CSavedLocation
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonDisability as PD
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.Event
import qualified Tools.JSON as J
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics

data SearchReq = OneWaySearch OneWaySearchReq | RentalSearch RentalSearchReq | InterCitySearch InterCitySearchReq | AmbulanceSearch OneWaySearchReq
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
  "InterCitySearch" -> "INTER_CITY"
  "AmbulanceSearch" -> "AMBULANCE"
  x -> x

data OneWaySearchReq = OneWaySearchReq
  { origin :: SearchReqLocation,
    destination :: SearchReqLocation,
    isSourceManuallyMoved :: Maybe Bool,
    isDestinationManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: Maybe UTCTime,
    isReallocationEnabled :: Maybe Bool,
    quotesUnifiedFlow :: Maybe Bool,
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
    quotesUnifiedFlow :: Maybe Bool,
    isReallocationEnabled :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data InterCitySearchReq = InterCitySearchReq
  { origin :: SearchReqLocation,
    stops :: Maybe [SearchReqLocation],
    roundTrip :: Bool,
    isSourceManuallyMoved :: Maybe Bool,
    isDestinationManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    sessionToken :: Maybe Text,
    quotesUnifiedFlow :: Maybe Bool,
    isReallocationEnabled :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data SearchRes = SearchRes
  { origin :: SearchReqLocation,
    stops :: [SearchReqLocation],
    startTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    riderPreferredOption :: SearchRequest.RiderPreferredOption,
    roundTrip :: Bool,
    isDashboardRequest :: Bool,
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
    isReallocationEnabled :: Maybe Bool,
    multipleRoutes :: Maybe [Maps.RouteInfo],
    taggings :: Maybe Beckn.Taggings
  }

data SearchDetails = SearchDetails
  { riderPreferredOption :: SearchRequest.RiderPreferredOption,
    origin :: SearchReqLocation,
    roundTrip :: Bool,
    stops :: [SearchReqLocation],
    isSourceManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    isReallocationEnabled :: Maybe Bool,
    quotesUnifiedFlow :: Maybe Bool
  }
  deriving (Generic, Show)

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
    EsqDBReplicaFlow m r,
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
      specialLocationBody <- Esq.runInReplica $ QSpecialLocation.findSpecialLocationByLatLong origin.gps
      case specialLocationBody of
        Just _ -> frequencyUpdator merchantId origin.gps (Just origin.address) SpecialLocation
        Nothing -> return ()

{-
# /search
#  -> Make one normal search to BAP (no change in this)
#     => create entry in search table
#     => initiate search to BPP
#  -> check if metro route possible (we have to add this logic in BAP itself as BPP always expect metro stations)
#     => if yes, then make a search to metro BPP with metro stations
#         => create entry in frfs search table with along with parent search id (multi-modal parent search id)
#     => also make a first and last miles search to BPP with parent search id as above search id (multi-modal parent search id)
-}
search ::
  Id Person.Person ->
  SearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe (Id DC.Client) ->
  Maybe Text ->
  Bool ->
  Bool ->
  Flow SearchRes
search personId req bundleVersion clientVersion clientConfigVersion clientId device isDashboardRequest_ makeMultiModalSearch = do
  now <- getCurrentTime
  let SearchDetails {..} = extractSearchDetails now req
  validateStartAndReturnTime now startTime returnTime

  let isDashboardRequest = isDashboardRequest_ || isNothing quotesUnifiedFlow -- Don't get confused with this, it is done to handle backward compatibility so that in both dashboard request or mobile app request without quotesUnifiedFlow can be consider same
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  tag <- case person.hasDisability of
    Just True -> B.runInReplica $ fmap (.tag) <$> PD.findByPersonId personId
    _ -> return Nothing

  merchant <- QMerc.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  let sourceLatLong = origin.gps
  let stopsLatLong = map (.gps) stops
  originCity <- Serviceability.validateServiceability sourceLatLong stopsLatLong person

  updateRideSearchHotSpot person origin merchant isSourceManuallyMoved isSpecialLocation

  -- merchant operating city of search-request-origin-location
  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchant.id originCity
      >>= fromMaybeM
        ( MerchantOperatingCityNotFound $
            "merchantId: " <> merchant.id.getId <> " ,city: " <> show originCity
        )
  searchRequestId <- generateGUID
  (longestRouteDistance, shortestRouteDistance, shortestRouteDuration, shortestRouteInfo, multipleRoutes) <-
    case req of
      OneWaySearch oneWayReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId oneWayReq stopsLatLong now sourceLatLong roundTrip
      AmbulanceSearch ambulanceReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId ambulanceReq stopsLatLong now sourceLatLong roundTrip
      InterCitySearch interCityReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId interCityReq stopsLatLong now sourceLatLong roundTrip
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
      (convertMetersToDistance merchantOperatingCity.distanceUnit <$> longestRouteDistance)
      (convertMetersToDistance merchantOperatingCity.distanceUnit <$> shortestRouteDistance)
      startTime
      returnTime
      roundTrip
      bundleVersion
      clientVersion
      clientConfigVersion
      device
      tag
      shortestRouteDuration
      riderPreferredOption
      merchantOperatingCity.distanceUnit
      person.totalRidesCount
  Metrics.incrementSearchRequestCount merchant.name merchantOperatingCity.id.getId

  Metrics.startSearchMetrics merchant.name searchRequest.id.getId
  -- triggerSearchEvent SearchEventData {searchRequest = searchRequest}
  QSearchRequest.createDSReq searchRequest
  QPFS.clearCache person.id

  fork "updating search counters" $ fraudCheck person merchantOperatingCity searchRequest

  when makeMultiModalSearch $ do
    fork "multi-modal search" $ multiModalSearch searchRequest
  return $
    SearchRes -- TODO: cleanup this reponse field based on what is not required for beckn type conversions
      { searchId = searchRequest.id,
        gatewayUrl = merchant.gatewayUrl,
        searchRequestExpiry = searchRequest.validTill,
        customerLanguage = searchRequest.language,
        city = originCity,
        distance = shortestRouteDistance,
        duration = shortestRouteDuration,
        disabilityTag = tag,
        taggings = getTags shortestRouteDistance shortestRouteDuration returnTime roundTrip ((.points) <$> shortestRouteInfo) multipleRoutes isReallocationEnabled,
        ..
      }
  where
    getTags distance duration returnTime roundTrip mbPoints mbMultipleRoutes mbIsReallocationEnabled = do
      let isReallocationEnabled = fromMaybe False mbIsReallocationEnabled
      Just $
        def{Beckn.fulfillmentTags =
              [ (Beckn.DISTANCE_INFO_IN_M, show . (.getMeters) <$> distance),
                (Beckn.DURATION_INFO_IN_S, show . (.getSeconds) <$> duration),
                (Beckn.RETURN_TIME, show <$> returnTime),
                (Beckn.ROUND_TRIP, Just $ show roundTrip),
                (Beckn.WAYPOINTS, LT.toStrict . TE.decodeUtf8 . encode <$> mbPoints),
                (Beckn.MULTIPLE_ROUTES, LT.toStrict . TE.decodeUtf8 . encode <$> mbMultipleRoutes),
                (Beckn.IS_REALLOCATION_ENABLED, Just $ show isReallocationEnabled)
              ]
           }

    validateStartAndReturnTime :: UTCTime -> UTCTime -> Maybe UTCTime -> Flow ()
    validateStartAndReturnTime now startTime returnTime = do
      whenJust returnTime $ \rt -> do
        when (rt <= startTime) $ throwError (InvalidRequest "Return time should be greater than start time")
      unless ((120 `addUTCTime` startTime) >= now) $ throwError (InvalidRequest "Ride time should only be future time") -- 2 mins buffer
    extractSearchDetails :: UTCTime -> SearchReq -> SearchDetails
    extractSearchDetails now = \case
      OneWaySearch OneWaySearchReq {..} ->
        SearchDetails
          { riderPreferredOption = SearchRequest.OneWay,
            roundTrip = False,
            stops = [destination],
            startTime = fromMaybe now startTime,
            returnTime = Nothing,
            ..
          }
      RentalSearch RentalSearchReq {..} ->
        SearchDetails
          { riderPreferredOption = SearchRequest.Rental,
            roundTrip = False,
            stops = fromMaybe [] stops,
            returnTime = Nothing,
            ..
          }
      InterCitySearch InterCitySearchReq {..} ->
        SearchDetails
          { riderPreferredOption = SearchRequest.InterCity,
            stops = fromMaybe [] stops,
            ..
          }
      AmbulanceSearch OneWaySearchReq {..} ->
        SearchDetails
          { riderPreferredOption = SearchRequest.Ambulance,
            roundTrip = False,
            stops = [destination],
            startTime = fromMaybe now startTime,
            returnTime = Nothing,
            ..
          }

    processOneWaySearch person merchant merchantOperatingCity searchRequestId sReq stopsLatLong now sourceLatLong roundTrip = do
      riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCity.id >>= fromMaybeM (RiderConfigNotFound merchantOperatingCity.id.getId)
      autoCompleteEvent riderConfig searchRequestId sReq.sessionToken sReq.isSourceManuallyMoved sReq.isDestinationManuallyMoved now
      destinationLatLong <- listToMaybe stopsLatLong & fromMaybeM (InternalError "Destination is required for OneWay Search")
      let latLongs = if roundTrip then [sourceLatLong, destinationLatLong, sourceLatLong] else [sourceLatLong, destinationLatLong]
      calculateDistanceAndRoutes riderConfig merchant merchantOperatingCity person searchRequestId latLongs now

    updateRideSearchHotSpot :: DPerson.Person -> SearchReqLocation -> Merchant -> Maybe Bool -> Maybe Bool -> Flow ()
    updateRideSearchHotSpot person origin merchant isSourceManuallyMoved isSpecialLocation = do
      HotSpotConfig {..} <- QHotSpotConfig.findConfigByMerchantId merchant.id >>= fromMaybeM (InternalError "config not found for merchant")
      when (shouldSaveSearchHotSpot && shouldTakeHotSpot) do
        fork "ride search geohash frequencyUpdater" $ do
          mbFavourite <- CSavedLocation.findByLatLonAndRiderId person.id origin.gps
          hotSpotUpdate person.merchantId mbFavourite origin isSourceManuallyMoved
          updateForSpecialLocation person.merchantId origin isSpecialLocation

    fraudCheck :: DPerson.Person -> DMOC.MerchantOperatingCity -> SearchRequest.SearchRequest -> Flow ()
    fraudCheck person merchantOperatingCity searchRequest = do
      merchantConfigs <- QMC.findAllByMerchantOperatingCityId person.merchantOperatingCityId
      SMC.updateSearchFraudCounters person.id merchantConfigs
      mFraudDetected <- SMC.anyFraudDetected person.id merchantOperatingCity.id merchantConfigs (Just searchRequest)
      whenJust mFraudDetected $ \mc -> SMC.blockCustomer person.id (Just mc.id)

-- TODO(MultiModal): Move all multimodal related code to a separate module
data MultiModalRoute = Metro [LatLong] | None

multiModalSearch :: SearchRequest.SearchRequest -> Flow ()
multiModalSearch searchRequest = do
  multiModalRoute <- checkIfMultModalRoutePossible searchRequest
  case multiModalRoute of
    Metro _metroRoute -> do
      -- TODO(MultiModal):
      -- make a search to metro BAP (call FRFS function, also pass parent searchId to aggregate information) with metro stations
      -- if firstMile and lastMile is not walk then call
      -- search personId req bundleVersion clientVersion clientConfigVersion clientId device isDashboardRequest_ False parentSearchId
      -- check on_status information for more details
      throwError (InternalError "Not implemented")
    None -> return ()

-- TODO(MultiModal)
checkIfMultModalRoutePossible :: SearchRequest.SearchRequest -> Flow MultiModalRoute
checkIfMultModalRoutePossible _ = throwError (InternalError "Not implemented")

buildSearchRequest ::
  Id SearchRequest.SearchRequest ->
  Maybe (Id DC.Client) ->
  DPerson.Person ->
  Location.Location ->
  DMOC.MerchantOperatingCity ->
  Maybe Location.Location ->
  Maybe Distance ->
  Maybe Distance ->
  UTCTime ->
  Maybe UTCTime ->
  Bool ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Seconds ->
  SearchRequest.RiderPreferredOption ->
  DistanceUnit ->
  Maybe Int ->
  Flow SearchRequest.SearchRequest
buildSearchRequest searchRequestId mbClientId person pickup merchantOperatingCity mbDrop mbMaxDistance mbDistance startTime returnTime roundTrip bundleVersion clientVersion clientConfigVersion device disabilityTag duration riderPreferredOption distanceUnit totalRidesCount = do
  now <- getCurrentTime
  validTill <- getSearchRequestExpiry startTime
  deploymentVersion <- asks (.version)
  return
    SearchRequest.SearchRequest
      { id = searchRequestId,
        startTime,
        returnTime,
        roundTrip = Just roundTrip,
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
        selectedPaymentMethodId = Nothing,
        isAdvanceBookingEnabled = Nothing,
        riderPreferredOption, -- this is just to store the rider preference for the ride type to handle backward compatibility
        distanceUnit,
        totalRidesCount
      }
  where
    getSearchRequestExpiry :: (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]) => UTCTime -> m UTCTime
    getSearchRequestExpiry time = do
      searchRequestExpiry <- maybe 1800 fromIntegral <$> asks (.searchRequestExpiry)
      pure $ addUTCTime (fromInteger searchRequestExpiry) time

calculateDistanceAndRoutes ::
  RiderConfig ->
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  DPerson.Person ->
  Id SearchRequest.SearchRequest ->
  [LatLong] ->
  UTCTime ->
  Flow (Maybe Meters, Maybe Meters, Maybe Seconds, Maybe Maps.RouteInfo, Maybe [Maps.RouteInfo])
calculateDistanceAndRoutes riderConfig merchant merchantOperatingCity person searchRequestId latLongs now = do
  let request =
        Maps.GetRoutesReq
          { waypoints = NE.fromList latLongs,
            calcPoints = True,
            mode = Just Maps.CAR
          }
  routeResponse <- Maps.getRoutes (Just riderConfig.isAvoidToll) person.id person.merchantId (Just merchantOperatingCity.id) request

  let collectMMIData = fromMaybe False riderConfig.collectMMIRouteData
  when collectMMIData $ do
    fork "calling mmi directions api" $ do
      mmiConfigs <- QMSC.findByMerchantOpCityIdAndService person.merchantId merchantOperatingCity.id (DMSC.MapsService MapsK.MMI) >>= fromMaybeM (MerchantServiceConfigNotFound person.merchantId.getId "Maps" "MMI")
      case mmiConfigs.serviceConfig of
        DMSC.MapsServiceConfig mapsCfg -> do
          routeResp <- MapsRoutes.getRoutes True mapsCfg request
          logInfo $ "MMI route response: " <> show routeResp
          let routeData = RouteDataEvent (Just $ show MapsK.MMI) (map show routeResp) (Just searchRequestId) merchant.id merchantOperatingCity.id now now
          triggerRouteDataEvent routeData
        _ -> logInfo "MapsServiceConfig config not found for MMI"

  shouldCollectRouteData <- asks (.collectRouteData)
  when shouldCollectRouteData $ do
    fork "calling next billion directions api" $ do
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

  let distanceWeightage = riderConfig.distanceWeightage
      durationWeightage = 100 - distanceWeightage
      (shortestRouteInfo, shortestRouteIndex) = Search.getEfficientRouteInfo routeResponse distanceWeightage durationWeightage
      longestRouteDistance = (.distance) =<< Search.getLongestRouteDistance routeResponse
      shortestRouteDistance = (.distance) =<< shortestRouteInfo
      shortestRouteDuration = (.duration) =<< shortestRouteInfo
  return (longestRouteDistance, shortestRouteDistance, shortestRouteDuration, shortestRouteInfo, Just $ Search.updateEfficientRoutePosition routeResponse shortestRouteIndex)

autoCompleteEvent ::
  RiderConfig ->
  Id SearchRequest.SearchRequest ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  UTCTime ->
  Flow ()
autoCompleteEvent riderConfig searchRequestId sessionToken isSourceManuallyMoved isDestinationManuallyMoved now = do
  whenJust sessionToken $ \token -> do
    let toCollectData = fromMaybe False riderConfig.collectAutoCompleteData
    when toCollectData $ do
      fork "Updating autocomplete data in search" $ do
        let pickUpKey = makeAutoCompleteKey token (show DMaps.PICKUP)
        let dropKey = makeAutoCompleteKey token (show DMaps.DROP)
        pickupRecord :: Maybe AutoCompleteEventData <- Redis.safeGet pickUpKey
        dropRecord :: Maybe AutoCompleteEventData <- Redis.safeGet dropKey
        whenJust pickupRecord $ \record -> do
          let updatedRecord = AutoCompleteEventData record.autocompleteInputs record.customerId record.id isSourceManuallyMoved (Just searchRequestId) record.searchType record.sessionToken record.merchantId record.merchantOperatingCityId record.originLat record.originLon record.createdAt now
          -- let updatedRecord = record {DTA.searchRequestId = Just searchRequestId, DTA.isLocationSelectedOnMap = isSourceManuallyMoved, DTA.updatedAt = now}
          triggerAutoCompleteEvent updatedRecord
        whenJust dropRecord $ \record -> do
          let updatedRecord = AutoCompleteEventData record.autocompleteInputs record.customerId record.id isDestinationManuallyMoved (Just searchRequestId) record.searchType record.sessionToken record.merchantId record.merchantOperatingCityId record.originLat record.originLon record.createdAt now
          -- let updatedRecord = record {DTA.searchRequestId = Just searchRequestId, DTA.isLocationSelectedOnMap = isDestinationManuallyMoved, DTA.updatedAt = now}
          triggerAutoCompleteEvent updatedRecord

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
