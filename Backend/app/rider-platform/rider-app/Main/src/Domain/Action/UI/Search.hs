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
import Control.Applicative ((<|>))
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Text as AT
import Data.Default.Class
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE
import Domain.Action.UI.HotSpot
import Domain.Action.UI.Maps (makeAutoCompleteKey)
import qualified Domain.Action.UI.Maps as DMaps
import Domain.Types (GatewayAndRegistryService (..))
import qualified Domain.Types.Client as DC
import Domain.Types.HotSpot hiding (address, updatedAt)
import Domain.Types.HotSpotConfig
import qualified Domain.Types.Location as Location
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RefereeLink as DRL
import Domain.Types.RiderConfig
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
import Kernel.Prelude hiding (drop)
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Lib.JourneyLeg.Types as JPT
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import Lib.SessionizerMetrics.Types.Event
import Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.MerchantConfig as SMC
import qualified SharedLogic.Referral as Referral
import SharedLogic.Search
import qualified SharedLogic.Serviceability as Serviceability
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.HotSpotConfig as QHotSpotConfig
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.MerchantConfig as QMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.SavedReqLocation as CSavedLocation
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonDisability as PD
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.DynamicLogic (getConfigVersionMapForStickiness)
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import Tools.Metrics.BAPMetrics.Types

type SearchRequestFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    EventStreamFlow m r,
    ClickhouseFlow m r,
    HasBAPMetrics m r,
    HasFlowEnv m r '["nyGatewayUrl" ::: BaseUrl],
    HasFlowEnv m r '["ondcGatewayUrl" ::: BaseUrl],
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["collectRouteData" ::: Bool],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasField "hotSpotExpiry" r Seconds
  )

hotSpotUpdate ::
  SearchRequestFlow m r =>
  Id Merchant ->
  Maybe SavedReqLocation ->
  SearchReqLocation ->
  Maybe Bool ->
  Maybe HotSpotConfig ->
  m ()
hotSpotUpdate merchantId mbFavourite origin isSourceManuallyMoved mbHotSpotConfig = case mbFavourite of
  Just SavedReqLocation {..} ->
    frequencyUpdator merchantId origin.gps (Just origin.address) (bool NonManualSaved ManualSaved (isMoved == Just True)) mbHotSpotConfig
  Nothing ->
    frequencyUpdator merchantId origin.gps (Just origin.address) (bool NonManualPickup ManualPickup (isSourceManuallyMoved == Just True)) mbHotSpotConfig

updateForSpecialLocation ::
  SearchRequestFlow m r =>
  Id Merchant ->
  SearchReqLocation ->
  Maybe Bool ->
  Maybe HotSpotConfig ->
  m ()
updateForSpecialLocation merchantId origin mbIsSpecialLocation mbHotSpotConfig = do
  case mbIsSpecialLocation of
    Just isSpecialLocation -> do
      when isSpecialLocation $ frequencyUpdator merchantId origin.gps (Just origin.address) SpecialLocation mbHotSpotConfig
    Nothing -> do
      specialLocationBody <- Esq.runInReplica $ QSpecialLocation.findSpecialLocationByLatLong origin.gps
      case specialLocationBody of
        Just _ -> frequencyUpdator merchantId origin.gps (Just origin.address) SpecialLocation mbHotSpotConfig
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
  SearchRequestFlow m r =>
  Id Person.Person ->
  SearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe (Id DC.Client) ->
  Maybe Text ->
  Bool ->
  Maybe JPT.JourneySearchData ->
  m SearchRes
search personId req bundleVersion clientVersion clientConfigVersion_ mbRnVersion clientId device isDashboardRequest_ journeySearchData = do
  now <- getCurrentTime
  let SearchDetails {..} = extractSearchDetails now req
  validateStartAndReturnTime now startTime returnTime

  let isDashboardRequest = isDashboardRequest_ || isNothing quotesUnifiedFlow -- Don't get confused with this, it is done to handle backward compatibility so that in both dashboard request or mobile app request without quotesUnifiedFlow can be consider same
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  tag <- case person.hasDisability of
    Just True -> B.runInReplica $ fmap (.tag) <$> PD.findByPersonId personId
    _ -> return Nothing

  merchant <- QMerc.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  let txnCity = show merchant.defaultCity

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
  let merchantOperatingCityId = merchantOperatingCity.id
  configVersionMap <- getConfigVersionMapForStickiness (cast merchantOperatingCityId)
  riderCfg <- QRC.findByMerchantOperatingCityIdInRideFlow merchantOperatingCityId configVersionMap >>= fromMaybeM (RiderConfigNotFound merchantOperatingCityId.getId)
  searchRequestId <- generateGUID
  RouteDetails {..} <- getRouteDetails person merchant merchantOperatingCity searchRequestId stopsLatLong now sourceLatLong roundTrip originCity riderCfg req
  fromLocation <- buildSearchReqLoc merchant.id merchantOperatingCityId origin
  stopLocations <- buildSearchReqLoc merchant.id merchantOperatingCityId `mapM` stops
  let driverIdentifier' = driverIdentifier_ <|> (person.referralCode >>= \refCode -> bool Nothing (mkDriverIdentifier refCode) $ shouldPriortiseDriver person riderCfg refCode)
  searchRequest <-
    buildSearchRequest
      searchRequestId
      clientId
      person
      fromLocation
      merchantOperatingCity
      (lastMaybe stopLocations) ---(Now - As it's drop which would be last element of stops list) --------------- (Earlier - Take first stop, handle multiple stops later)
      (convertMetersToDistance merchantOperatingCity.distanceUnit <$> longestRouteDistance)
      (convertMetersToDistance merchantOperatingCity.distanceUnit <$> shortestRouteDistance)
      startTime
      returnTime
      roundTrip
      bundleVersion
      clientVersion
      clientConfigVersion_
      mbRnVersion
      device
      tag
      shortestRouteDuration
      shortestRouteStaticDuration
      riderPreferredOption
      merchantOperatingCity.distanceUnit
      person.totalRidesCount
      isDashboardRequest_
      placeNameSource
      hasStops
      (safeInit stopLocations)
      journeySearchData
      driverIdentifier'
      configVersionMap
  Metrics.incrementSearchRequestCount merchant.name merchantOperatingCity.id.getId

  Metrics.startSearchMetrics merchant.name searchRequest.id.getId
  -- triggerSearchEvent SearchEventData {searchRequest = searchRequest}
  QSearchRequest.createDSReq searchRequest
  QPFS.clearCache person.id

  fork "updating search counters" $ fraudCheck person merchantOperatingCity searchRequest
  let updatedPerson = backfillCustomerNammaTags person
  gatewayUrl <-
    case merchant.gatewayAndRegistryPriorityList of
      (NY : _) -> asks (.nyGatewayUrl)
      _ -> asks (.ondcGatewayUrl)
  return $
    SearchRes -- TODO: cleanup this reponse field based on what is not required for beckn type conversions
      { searchRequest = searchRequest,
        gatewayUrl = gatewayUrl,
        searchRequestExpiry = searchRequest.validTill,
        city = originCity,
        distance = shortestRouteDistance,
        duration = shortestRouteDuration,
        taggings = getTags tag searchRequest updatedPerson shortestRouteDistance shortestRouteDuration returnTime roundTrip ((.points) <$> shortestRouteInfo) multipleRoutes txnCity isReallocationEnabled isDashboardRequest fareParametersInRateCard,
        ..
      }
  where
    mkDriverIdentifier :: Text -> Maybe DRL.DriverIdentifier
    mkDriverIdentifier refCode = DRL.mkDriverIdentifier (Just DRL.REFERRAL_CODE) (Just refCode)

    shouldPriortiseDriver :: Person.Person -> RiderConfig -> Text -> Bool
    shouldPriortiseDriver person riderCfg refCode = riderCfg.isFirstReferredRideEnabled && isFirstRideFor person && not (Referral.isCustomerReferralCode refCode)

    isFirstRideFor :: Person.Person -> Bool
    isFirstRideFor person = person.totalRidesCount == Just 0

    backfillCustomerNammaTags :: Person.Person -> Person.Person
    backfillCustomerNammaTags Person.Person {..} =
      if isNothing customerNammaTags
        then do
          let genderTag = LYT.TagNameValueExpiry $ "Gender#" <> show gender -- handle it properly later
          Person.Person {customerNammaTags = Just [genderTag], ..}
        else Person.Person {..}

    getTags tag searchRequest person distance duration returnTime roundTrip mbPoints mbMultipleRoutes txnCity mbIsReallocationEnabled isDashboardRequest mbfareParametersInRateCard = do
      let isReallocationEnabled = fromMaybe False mbIsReallocationEnabled
      let fareParametersInRateCard = fromMaybe False mbfareParametersInRateCard
      Just $
        def{Beckn.fulfillmentTags =
              [ (Beckn.DISTANCE_INFO_IN_M, show . (.getMeters) <$> distance),
                (Beckn.DURATION_INFO_IN_S, show . (.getSeconds) <$> duration),
                (Beckn.RETURN_TIME, show <$> returnTime),
                (Beckn.ROUND_TRIP, Just $ show roundTrip),
                (Beckn.WAYPOINTS, LT.toStrict . TE.decodeUtf8 . encode <$> mbPoints),
                (Beckn.MULTIPLE_ROUTES, LT.toStrict . TE.decodeUtf8 . encode <$> mbMultipleRoutes),
                (Beckn.IS_REALLOCATION_ENABLED, Just $ show isReallocationEnabled),
                (Beckn.FARE_PARAMETERS_IN_RATECARD, Just $ show fareParametersInRateCard),
                (Beckn.DRIVER_IDENTITY, searchRequest.driverIdentifier <&> LT.toStrict . AT.encodeToLazyText)
              ],
            Beckn.paymentTags =
              [ (Beckn.SETTLEMENT_AMOUNT, Nothing),
                (Beckn.DELAY_INTEREST, Just "0"),
                (Beckn.SETTLEMENT_BASIS, Just "INVOICE_RECIEPT"),
                (Beckn.MANDATORY_ARBITRATION, Just "TRUE"),
                (Beckn.COURT_JURISDICTION, Just txnCity)
              ],
            Beckn.personTags =
              [ (Beckn.CUSTOMER_LANGUAGE, (Just . show) searchRequest.language),
                (Beckn.DASHBOARD_USER, (Just . show) isDashboardRequest),
                (Beckn.CUSTOMER_DISABILITY, (decode . encode) tag),
                (Beckn.CUSTOMER_NAMMA_TAGS, show @Text @[Text] . fmap ((.getTagNameValue) . Yudhishthira.removeTagExpiry) <$> person.customerNammaTags)
              ]
           }

    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs

    validateStartAndReturnTime :: SearchRequestFlow m r => UTCTime -> UTCTime -> Maybe UTCTime -> m ()
    validateStartAndReturnTime now startTime returnTime = do
      whenJust returnTime $ \rt -> do
        when (rt <= startTime) $ throwError (InvalidRequest "Return time should be greater than start time")
      unless ((120 `addUTCTime` startTime) >= now) $ throwError (InvalidRequest "Ride time should only be future time") -- 2 mins buffer
    getRouteDetails ::
      SearchRequestFlow m r =>
      DPerson.Person ->
      Merchant ->
      DMOC.MerchantOperatingCity ->
      Id SearchRequest.SearchRequest ->
      [LatLong] ->
      UTCTime ->
      LatLong ->
      Bool ->
      Context.City ->
      RiderConfig ->
      SearchReq ->
      m RouteDetails
    getRouteDetails person merchant merchantOperatingCity searchRequestId stopsLatLong now sourceLatLong roundTrip originCity riderCfg = \case
      OneWaySearch oneWayReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId oneWayReq.sessionToken oneWayReq.isSourceManuallyMoved oneWayReq.isDestinationManuallyMoved stopsLatLong now sourceLatLong roundTrip riderCfg
      AmbulanceSearch ambulanceReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId ambulanceReq.sessionToken ambulanceReq.isSourceManuallyMoved ambulanceReq.isDestinationManuallyMoved stopsLatLong now sourceLatLong roundTrip riderCfg
      InterCitySearch interCityReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId interCityReq.sessionToken interCityReq.isSourceManuallyMoved interCityReq.isDestinationManuallyMoved stopsLatLong now sourceLatLong roundTrip riderCfg
      RentalSearch rentalReq -> processRentalSearch person rentalReq stopsLatLong originCity
      DeliverySearch deliveryReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId deliveryReq.sessionToken deliveryReq.isSourceManuallyMoved deliveryReq.isDestinationManuallyMoved stopsLatLong now sourceLatLong roundTrip riderCfg

    extractSearchDetails :: UTCTime -> SearchReq -> SearchDetails
    extractSearchDetails now = \case
      OneWaySearch reqDetails@OneWaySearchReq {..} ->
        SearchDetails
          { riderPreferredOption = SearchRequest.OneWay,
            roundTrip = False,
            stops = fromMaybe [] stops <> [destination],
            startTime = fromMaybe now startTime,
            returnTime = Nothing,
            hasStops = reqDetails.stops >>= \s -> Just $ length s > 0,
            driverIdentifier_ = driverIdentifier,
            ..
          }
      RentalSearch RentalSearchReq {..} ->
        SearchDetails
          { riderPreferredOption = SearchRequest.Rental,
            roundTrip = False,
            stops = fromMaybe [] stops,
            hasStops = Nothing,
            returnTime = Nothing,
            driverIdentifier_ = Nothing,
            ..
          }
      InterCitySearch InterCitySearchReq {..} ->
        SearchDetails
          { riderPreferredOption = SearchRequest.InterCity,
            stops = fromMaybe [] stops,
            hasStops = Nothing,
            driverIdentifier_ = Nothing,
            ..
          }
      AmbulanceSearch OneWaySearchReq {..} ->
        SearchDetails
          { riderPreferredOption = SearchRequest.Ambulance,
            roundTrip = False,
            stops = [destination],
            startTime = fromMaybe now startTime,
            returnTime = Nothing,
            hasStops = Nothing,
            driverIdentifier_ = driverIdentifier,
            ..
          }
      DeliverySearch OneWaySearchReq {..} ->
        SearchDetails
          { riderPreferredOption = SearchRequest.Delivery,
            roundTrip = False,
            stops = [destination],
            startTime = fromMaybe now startTime,
            returnTime = Nothing,
            hasStops = Nothing,
            driverIdentifier_ = driverIdentifier,
            ..
          }

    processOneWaySearch ::
      SearchRequestFlow m r =>
      DPerson.Person ->
      Merchant ->
      DMOC.MerchantOperatingCity ->
      Id SearchRequest.SearchRequest ->
      Maybe Text ->
      Maybe Bool ->
      Maybe Bool ->
      [LatLong] ->
      UTCTime ->
      LatLong ->
      Bool ->
      RiderConfig ->
      m RouteDetails
    processOneWaySearch person merchant merchantOperatingCity searchRequestId sessionToken isSourceManuallyMoved isDestinationManuallyMoved stopsLatLong now sourceLatLong roundTrip riderConfig = do
      autoCompleteEvent riderConfig searchRequestId sessionToken isSourceManuallyMoved isDestinationManuallyMoved now
      destinationLatLong <- lastMaybe stopsLatLong & fromMaybeM (InternalError "Destination is required for OneWay Search")
      let latLongs = if roundTrip then [sourceLatLong, destinationLatLong, sourceLatLong] else sourceLatLong : stopsLatLong
      calculateDistanceAndRoutes riderConfig merchant merchantOperatingCity person searchRequestId latLongs now

    processRentalSearch :: SearchRequestFlow m r => DPerson.Person -> RentalSearchReq -> [LatLong] -> Context.City -> m RouteDetails
    processRentalSearch person rentalReq stopsLatLong originCity = do
      case stopsLatLong of
        [] -> return $ RouteDetails Nothing (Just rentalReq.estimatedRentalDistance) (Just rentalReq.estimatedRentalDuration) Nothing (Just (RouteInfo (Just rentalReq.estimatedRentalDuration) Nothing (Just rentalReq.estimatedRentalDistance) Nothing Nothing [] [])) Nothing
        (stop : _) -> do
          stopCity <- Serviceability.validateServiceability stop [] person
          unless (stopCity == originCity) $ throwError RideNotServiceable
          return $ RouteDetails Nothing (Just rentalReq.estimatedRentalDistance) (Just rentalReq.estimatedRentalDuration) Nothing (Just (RouteInfo (Just rentalReq.estimatedRentalDuration) Nothing (Just rentalReq.estimatedRentalDistance) Nothing Nothing [] [])) Nothing

    updateRideSearchHotSpot :: SearchRequestFlow m r => DPerson.Person -> SearchReqLocation -> Merchant -> Maybe Bool -> Maybe Bool -> m ()
    updateRideSearchHotSpot person origin merchant isSourceManuallyMoved isSpecialLocation = do
      HotSpotConfig {..} <- QHotSpotConfig.findConfigByMerchantId merchant.id >>= fromMaybeM (InternalError "config not found for merchant")
      when (shouldSaveSearchHotSpot && shouldTakeHotSpot) do
        fork "ride search geohash frequencyUpdater" $ do
          let mbHotSpotConfig = Just $ HotSpotConfig {..}
          mbFavourite <- CSavedLocation.findByLatLonAndRiderId person.id origin.gps
          hotSpotUpdate person.merchantId mbFavourite origin isSourceManuallyMoved mbHotSpotConfig
          updateForSpecialLocation person.merchantId origin isSpecialLocation mbHotSpotConfig

    fraudCheck :: SearchRequestFlow m r => DPerson.Person -> DMOC.MerchantOperatingCity -> SearchRequest.SearchRequest -> m ()
    fraudCheck person merchantOperatingCity searchRequest = do
      merchantConfigs <- QMC.findAllByMerchantOperatingCityIdInRideFlow person.merchantOperatingCityId searchRequest.configInExperimentVersions
      SMC.updateSearchFraudCounters person.id merchantConfigs
      mFraudDetected <- SMC.anyFraudDetected person.id merchantOperatingCity.id merchantConfigs (Just searchRequest)
      whenJust mFraudDetected $ \mc -> SMC.blockCustomer person.id (Just mc.id)

buildSearchRequest ::
  SearchRequestFlow m r =>
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
  Maybe Text ->
  Maybe Seconds ->
  Maybe Seconds ->
  SearchRequest.RiderPreferredOption ->
  DistanceUnit ->
  Maybe Int ->
  Bool ->
  Maybe Text ->
  Maybe Bool ->
  [Location.Location] ->
  Maybe JPT.JourneySearchData ->
  Maybe DRL.DriverIdentifier ->
  [LYT.ConfigVersionMap] ->
  m SearchRequest.SearchRequest
buildSearchRequest searchRequestId mbClientId person pickup merchantOperatingCity mbDrop mbMaxDistance mbDistance startTime returnTime roundTrip bundleVersion clientVersion clientConfigVersion clientRnVersion device disabilityTag duration staticDuration riderPreferredOption distanceUnit totalRidesCount isDashboardRequest mbPlaceNameSource hasStops stops journeySearchData mbDriverReferredInfo configVersionMap = do
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
        estimatedRideStaticDuration = staticDuration,
        device = device,
        hasStops,
        clientBundleVersion = bundleVersion,
        clientSdkVersion = clientVersion,
        clientReactNativeVersion = clientRnVersion,
        clientDevice = getDeviceFromText device,
        clientConfigVersion = clientConfigVersion,
        backendConfigVersion = Nothing,
        backendAppVersion = Just deploymentVersion.getDeploymentVersion,
        language = person.language,
        customerExtraFee = Nothing,
        autoAssignEnabled = Nothing,
        autoAssignEnabledV2 = Nothing,
        selectedPaymentMethodId = Nothing,
        isAdvanceBookingEnabled = Nothing,
        availablePaymentMethods = [],
        riderPreferredOption, -- this is just to store the rider preference for the ride type to handle backward compatibility
        distanceUnit,
        stops,
        totalRidesCount,
        isDashboardRequest = Just isDashboardRequest,
        placeNameSource = mbPlaceNameSource,
        initiatedBy = Nothing,
        journeyLegInfo = journeySearchData,
        driverIdentifier = mbDriverReferredInfo,
        hasMultimodalSearch = Just False,
        configInExperimentVersions = configVersionMap,
        ..
      }
  where
    getSearchRequestExpiry :: SearchRequestFlow m r => UTCTime -> m UTCTime
    getSearchRequestExpiry time = do
      searchRequestExpiry <- maybe 1800 fromIntegral <$> asks (.searchRequestExpiry)
      pure $ addUTCTime (fromInteger searchRequestExpiry) time

calculateDistanceAndRoutes ::
  SearchRequestFlow m r =>
  RiderConfig ->
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  DPerson.Person ->
  Id SearchRequest.SearchRequest ->
  [LatLong] ->
  UTCTime ->
  m RouteDetails
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
      shortestRouteStaticDuration = (.staticDuration) =<< shortestRouteInfo
  return $ RouteDetails {multipleRoutes = Just $ Search.updateEfficientRoutePosition routeResponse shortestRouteIndex, ..}

autoCompleteEvent ::
  SearchRequestFlow m r =>
  RiderConfig ->
  Id SearchRequest.SearchRequest ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  UTCTime ->
  m ()
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
