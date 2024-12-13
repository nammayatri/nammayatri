{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Search where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified Beckn.ACL.Search as TaxiACL
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Tags as Beckn
import Control.Applicative (liftA2)
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Text as AT
import Data.Default.Class
import qualified Data.List.NonEmpty as NE
import Data.OpenApi hiding (Header, description, email)
import qualified Data.OpenApi as OpenApi hiding (Header)
import Data.Text (breakOn, drop)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Domain.Action.UI.HotSpot
import Domain.Action.UI.Maps (makeAutoCompleteKey)
import qualified Domain.Action.UI.Maps as DMaps
import Domain.Types (GatewayAndRegistryService (..))
import qualified Domain.Types.Client as DC
import Domain.Types.HotSpot hiding (address, updatedAt)
import Domain.Types.HotSpotConfig
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.Location as Location
import Domain.Types.LocationAddress
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
import qualified Domain.Types.Trip as DTrip
import qualified Domain.Types.WalkLegMultimodal as DWalkLeg
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps
import qualified Kernel.External.Maps as MapsK
import Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.Maps.Interface as MapsRoutes
import qualified Kernel.External.Maps.Interface.NextBillion as NextBillion
import qualified Kernel.External.Maps.NextBillion.Types as NBT
import qualified Kernel.External.Maps.Utils as Search
import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import Kernel.Prelude hiding (drop)
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Lib.JourneyPlannerTypes as JPT
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.MerchantConfig as SMC
import SharedLogic.Search
import qualified SharedLogic.Serviceability as Serviceability
import qualified Storage.CachedQueries.HotSpotConfig as QHotSpotConfig
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.CachedQueries.MerchantConfig as QMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.SavedReqLocation as CSavedLocation
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonDisability as PD
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.WalkLegMultimodal as QWalkLeg
import Tools.Error
import Tools.Event
import qualified Tools.JSON as J
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import qualified Tools.MultiModal as TMultiModal

data SearchReq = OneWaySearch OneWaySearchReq | RentalSearch RentalSearchReq | InterCitySearch InterCitySearchReq | AmbulanceSearch OneWaySearchReq | DeliverySearch OneWaySearchReq
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
  "DeliverySearch" -> "DELIVERY"
  x -> x

data OneWaySearchReq = OneWaySearchReq
  { origin :: SearchReqLocation,
    destination :: SearchReqLocation,
    stops :: Maybe [SearchReqLocation],
    isSourceManuallyMoved :: Maybe Bool,
    isDestinationManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: Maybe UTCTime,
    isReallocationEnabled :: Maybe Bool,
    quotesUnifiedFlow :: Maybe Bool,
    sessionToken :: Maybe Text,
    placeNameSource :: Maybe Text,
    driverIdentifier :: Maybe DRL.DriverIdentifier
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
    isReallocationEnabled :: Maybe Bool,
    placeNameSource :: Maybe Text
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
    isReallocationEnabled :: Maybe Bool,
    placeNameSource :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RouteDetails = RouteDetails
  { longestRouteDistance :: Maybe Meters,
    shortestRouteDistance :: Maybe Meters,
    shortestRouteDuration :: Maybe Seconds,
    shortestRouteStaticDuration :: Maybe Seconds,
    shortestRouteInfo :: Maybe Maps.RouteInfo,
    multipleRoutes :: Maybe [Maps.RouteInfo]
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
    hasStops :: Maybe Bool,
    isReallocationEnabled :: Maybe Bool,
    quotesUnifiedFlow :: Maybe Bool,
    placeNameSource :: Maybe Text,
    driverIdentifier_ :: Maybe DRL.DriverIdentifier
  }
  deriving (Generic, Show)

data JourneyPlannerLeg = JourneyPlannerLeg
  { mode :: DTrip.TravelMode,
    agency :: Text,
    originGps :: LatLong,
    destinationGps :: LatLong,
    legOrder :: Int,
    estimatedDistance :: Distance,
    estimatedDuration :: Seconds,
    originAddress :: LocationAddress,
    destinationAddress :: LocationAddress
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
  Maybe Text ->
  Maybe (Id DC.Client) ->
  Maybe Text ->
  Bool ->
  Maybe JPT.JourneySearchData ->
  Flow SearchRes
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
  riderCfg <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (RiderConfigNotFound merchantOperatingCityId.getId)
  searchRequestId <- generateGUID
  RouteDetails {..} <- getRouteDetails person merchant merchantOperatingCity searchRequestId stopsLatLong now sourceLatLong roundTrip originCity riderCfg req
  fromLocation <- buildSearchReqLoc origin
  stopLocations <- buildSearchReqLoc `mapM` stops
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
      (liftA2 (,) driverIdentifier_ riderCfg.driverReferredSearchReqExpiry)
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

  riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCity.id >>= fromMaybeM (RiderConfigNotFound merchantOperatingCity.id.getId)
  when (riderConfig.makeMultiModalSearch && isNothing journeySearchData) $ do
    case req of
      OneWaySearch searchReq -> fork "multi-modal search" $ multiModalSearch personId person.merchantId searchReq bundleVersion clientVersion clientConfigVersion_ mbRnVersion clientId device isDashboardRequest_ searchRequest merchantOperatingCityId riderConfig.maximumWalkDistance originCity
      _ -> pure ()

  return $
    SearchRes -- TODO: cleanup this reponse field based on what is not required for beckn type conversions
      { searchId = searchRequest.id,
        gatewayUrl = gatewayUrl,
        searchRequestExpiry = searchRequest.validTill,
        city = originCity,
        distance = shortestRouteDistance,
        duration = shortestRouteDuration,
        taggings = getTags tag searchRequest updatedPerson shortestRouteDistance shortestRouteDuration returnTime roundTrip ((.points) <$> shortestRouteInfo) multipleRoutes txnCity isReallocationEnabled isDashboardRequest,
        ..
      }
  where
    backfillCustomerNammaTags :: Person.Person -> Person.Person
    backfillCustomerNammaTags Person.Person {..} =
      if isNothing customerNammaTags
        then do
          let genderTag = "Gender#" <> show gender -- handle it properly later
          Person.Person {customerNammaTags = Just [genderTag], ..}
        else Person.Person {..}

    getTags tag searchRequest person distance duration returnTime roundTrip mbPoints mbMultipleRoutes txnCity mbIsReallocationEnabled isDashboardRequest = do
      let isReallocationEnabled = fromMaybe False mbIsReallocationEnabled && isNothing searchRequest.driverIdentifier
      Just $
        def{Beckn.fulfillmentTags =
              [ (Beckn.DISTANCE_INFO_IN_M, show . (.getMeters) <$> distance),
                (Beckn.DURATION_INFO_IN_S, show . (.getSeconds) <$> duration),
                (Beckn.RETURN_TIME, show <$> returnTime),
                (Beckn.ROUND_TRIP, Just $ show roundTrip),
                (Beckn.WAYPOINTS, LT.toStrict . TE.decodeUtf8 . encode <$> mbPoints),
                (Beckn.MULTIPLE_ROUTES, LT.toStrict . TE.decodeUtf8 . encode <$> mbMultipleRoutes),
                (Beckn.IS_REALLOCATION_ENABLED, Just $ show isReallocationEnabled),
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
                (Beckn.CUSTOMER_NAMMA_TAGS, show <$> person.customerNammaTags)
              ]
           }

    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs

    validateStartAndReturnTime :: UTCTime -> UTCTime -> Maybe UTCTime -> Flow ()
    validateStartAndReturnTime now startTime returnTime = do
      whenJust returnTime $ \rt -> do
        when (rt <= startTime) $ throwError (InvalidRequest "Return time should be greater than start time")
      unless ((120 `addUTCTime` startTime) >= now) $ throwError (InvalidRequest "Ride time should only be future time") -- 2 mins buffer
    getRouteDetails person merchant merchantOperatingCity searchRequestId stopsLatLong now sourceLatLong roundTrip originCity riderCfg = \case
      OneWaySearch oneWayReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId oneWayReq stopsLatLong now sourceLatLong roundTrip riderCfg
      AmbulanceSearch ambulanceReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId ambulanceReq stopsLatLong now sourceLatLong roundTrip riderCfg
      InterCitySearch interCityReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId interCityReq stopsLatLong now sourceLatLong roundTrip riderCfg
      RentalSearch rentalReq -> processRentalSearch person rentalReq stopsLatLong originCity
      DeliverySearch deliveryReq -> processOneWaySearch person merchant merchantOperatingCity searchRequestId deliveryReq stopsLatLong now sourceLatLong roundTrip riderCfg

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

    processOneWaySearch person merchant merchantOperatingCity searchRequestId sReq stopsLatLong now sourceLatLong roundTrip riderConfig = do
      autoCompleteEvent riderConfig searchRequestId sReq.sessionToken sReq.isSourceManuallyMoved sReq.isDestinationManuallyMoved now
      destinationLatLong <- lastMaybe stopsLatLong & fromMaybeM (InternalError "Destination is required for OneWay Search")
      let latLongs = if roundTrip then [sourceLatLong, destinationLatLong, sourceLatLong] else sourceLatLong : stopsLatLong
      calculateDistanceAndRoutes riderConfig merchant merchantOperatingCity person searchRequestId latLongs now

    processRentalSearch person rentalReq stopsLatLong originCity = do
      case stopsLatLong of
        [] -> return $ RouteDetails Nothing (Just rentalReq.estimatedRentalDistance) (Just rentalReq.estimatedRentalDuration) Nothing (Just (RouteInfo (Just rentalReq.estimatedRentalDuration) Nothing (Just rentalReq.estimatedRentalDistance) Nothing Nothing [] [])) Nothing
        (stop : _) -> do
          stopCity <- Serviceability.validateServiceability stop [] person
          unless (stopCity == originCity) $ throwError RideNotServiceable
          return $ RouteDetails Nothing (Just rentalReq.estimatedRentalDistance) (Just rentalReq.estimatedRentalDuration) Nothing (Just (RouteInfo (Just rentalReq.estimatedRentalDuration) Nothing (Just rentalReq.estimatedRentalDistance) Nothing Nothing [] [])) Nothing

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
-- data MultiModalRoute = Metro [LatLong] | None

multiModalSearch ::
  Id Person.Person ->
  Id DM.Merchant ->
  OneWaySearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe (Id DC.Client) ->
  Maybe Text ->
  Bool ->
  SearchRequest.SearchRequest ->
  Id DMOC.MerchantOperatingCity ->
  Meters ->
  Context.City ->
  Flow ()
multiModalSearch personId merchantId searchReq bundleVersion clientVersion clientConfigVersion_ clientRnVersion clientId device isDashboardRequest_ searchRequest merchantOperatingCityId maximumWalkDistance originCity = do
  now <- getCurrentTime

  let transitRoutesReq =
        GetTransitRoutesReq
          { origin = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = searchReq.origin.gps.lat, longitude = searchReq.origin.gps.lon}}},
            destination = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = searchReq.destination.gps.lat, longitude = searchReq.destination.gps.lon}}},
            arrivalTime = Nothing,
            departureTime = searchReq.startTime,
            mode = Nothing,
            transitPreferences = Nothing,
            transportModes = Nothing
          }

  transitServiceReq <- TMultiModal.getTransitServiceReq merchantId merchantOperatingCityId

  allRoutes <- MultiModal.getTransitRoutes transitServiceReq transitRoutesReq >>= fromMaybeM (InternalError "routes dont exist")

  void $
    mapWithIndex -- to see: only take topmost route
      ( \idx route -> do
          when (idx == 0) $ do
            journeyId <- generateGUID

            let journeyPlannerLegs = route.legs
                journeyLegsCount = length journeyPlannerLegs
                modes = map (\x -> convertMultiModalModeToTripMode x.mode (distanceToMeters x.distance) maximumWalkDistance) journeyPlannerLegs

            let journey =
                  DJourney.Journey
                    { convenienceCost = 0,
                      estimatedDistance = route.distance,
                      estimatedDuration = Just route.duration,
                      estimatedFare = Nothing,
                      fare = Nothing,
                      id = Id journeyId,
                      legsDone = 0,
                      totalLegs = journeyLegsCount,
                      modes = modes,
                      searchRequestId = searchRequest.id,
                      merchantId = Just merchantId,
                      merchantOperatingCityId = Just searchRequest.merchantOperatingCityId,
                      createdAt = now,
                      updatedAt = now
                    }
            QJourney.create journey

            logDebug $ "journey for multi-modal: " <> show journey

            -- when (idx == 0) $ do
            fork "child searches for multi-modal journey" $ makeChildSearchReqs personId merchantId journeyPlannerLegs searchReq searchRequest (Id journeyId) journeyLegsCount bundleVersion clientVersion clientConfigVersion_ clientRnVersion clientId device isDashboardRequest_ now maximumWalkDistance originCity journey
      )
      allRoutes.routes

makeChildSearchReqs ::
  Id Person.Person ->
  Id DM.Merchant ->
  [MultiModal.MultiModalLeg] ->
  OneWaySearchReq ->
  SearchRequest.SearchRequest ->
  Id DJourney.Journey ->
  Int ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe (Id DC.Client) ->
  Maybe Text ->
  Bool ->
  UTCTime ->
  Meters ->
  Context.City ->
  DJourney.Journey ->
  Flow ()
makeChildSearchReqs personId merchantId journeyPlannerLegs searchReq searchRequest journeyId journeyLegsCount bundleVersion clientVersion clientConfigVersion_ clientRnVersion clientId device isDashboardRequest_ _now maximumWalkDistance originCity journey = do
  now <- getCurrentTime
  void $
    mapWithIndex
      ( \idx journeyPlannerLeg -> do
          let mode = convertMultiModalModeToTripMode journeyPlannerLeg.mode (distanceToMeters journeyPlannerLeg.distance) maximumWalkDistance
              journeySearchData =
                JPT.JourneySearchData
                  { journeyId = journeyId.getId,
                    journeyLegOrder = idx,
                    agency = journeyPlannerLeg.agency <&> (.name),
                    skipBooking = False,
                    convenienceCost = 0,
                    pricingId = Nothing
                  }
          case mode of
            DTrip.Taxi -> do
              let legSearchReq =
                    OneWaySearch
                      OneWaySearchReq
                        { origin =
                            SearchReqLocation
                              { gps = LatLong {lat = journeyPlannerLeg.startLocation.latLng.latitude, lon = journeyPlannerLeg.startLocation.latLng.longitude},
                                address = dummyAddress
                              },
                          destination =
                            SearchReqLocation
                              { gps = LatLong {lat = journeyPlannerLeg.endLocation.latLng.latitude, lon = journeyPlannerLeg.endLocation.latLng.longitude},
                                address = dummyAddress
                              },
                          isSourceManuallyMoved = if idx == 0 then searchReq.isSourceManuallyMoved else Nothing,
                          isDestinationManuallyMoved = if idx == (journeyLegsCount - 1) then searchReq.isDestinationManuallyMoved else Nothing,
                          isSpecialLocation = searchReq.isSpecialLocation,
                          startTime = Just now, -- journeyPlannerLeg.fromArrivalTime,
                          isReallocationEnabled = searchReq.isReallocationEnabled,
                          quotesUnifiedFlow = searchReq.quotesUnifiedFlow,
                          sessionToken = searchReq.sessionToken,
                          placeNameSource = Nothing,
                          stops = Nothing,
                          driverIdentifier = Nothing
                        }
              fork "child searchReq for multi-modal" . withShortRetry $ do
                dSearchRes <- search personId legSearchReq bundleVersion clientVersion clientConfigVersion_ clientRnVersion clientId device isDashboardRequest_ (Just journeySearchData)
                becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
                let generatedJson = encode becknTaxiReqV2
                logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
                void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 merchantId
            DTrip.Metro -> do
              frfsSearchReq <- convertToFRFSStations journeyPlannerLeg (Just journeySearchData)
              fork "child FRFS searchReq for multi-modal" $ void $ FRFSTicketService.postFrfsSearch (Just personId, merchantId) (Just originCity) Spec.METRO frfsSearchReq
            DTrip.Bus -> do
              frfsSearchReq <- convertToFRFSStations journeyPlannerLeg (Just journeySearchData)
              fork "child FRFS searchReq for multi-modal" $ void $ FRFSTicketService.postFrfsSearch (Just personId, merchantId) (Just originCity) Spec.BUS frfsSearchReq
            DTrip.Walk -> do
              fromLocation_ <- buildSearchReqLoc $ SearchReqLocation {gps = LatLong {lat = journeyPlannerLeg.startLocation.latLng.latitude, lon = journeyPlannerLeg.startLocation.latLng.longitude}, address = dummyAddress}
              toLocation_ <- buildSearchReqLoc $ SearchReqLocation {gps = LatLong {lat = journeyPlannerLeg.endLocation.latLng.latitude, lon = journeyPlannerLeg.endLocation.latLng.longitude}, address = dummyAddress}
              walkeLegid <- generateGUID
              let walkLeg =
                    DWalkLeg.WalkLegMultimodal
                      { id = walkeLegid,
                        estimatedDistance = journeyPlannerLeg.distance,
                        estimatedDuration = Just journeyPlannerLeg.duration,
                        fromLocation = fromLocation_,
                        toLocation = Just toLocation_,
                        journeyLegInfo = Just journeySearchData,
                        riderId = personId,
                        startTime = fromMaybe now journeyPlannerLeg.fromArrivalTime,
                        merchantId = Just merchantId,
                        merchantOperatingCityId = Just searchRequest.merchantOperatingCityId,
                        createdAt = now,
                        updatedAt = now
                      }
              QWalkLeg.create walkLeg
              let legsDoneInitially = journey.legsDone
              QJourney.updateNumberOfLegs (legsDoneInitially + 1) (journey.id)
      )
      journeyPlannerLegs

convertToFRFSStations :: MultiModal.MultiModalLeg -> Maybe JPT.JourneySearchData -> Flow FRFSTicketService.FRFSSearchAPIReq
convertToFRFSStations journeyPlannerLeg journeySearchData_ = do
  fromStopDetails <- journeyPlannerLeg.fromStopDetails & fromMaybeM (InternalError "fromStopDetails dont exist")
  toStopDetails <- journeyPlannerLeg.toStopDetails & fromMaybeM (InternalError "toStopDetails dont exist")
  routeDetails <- journeyPlannerLeg.routeDetails & fromMaybeM (InternalError "routeDetails dont exist")
  _fromStopCode <- fromStopDetails.gtfsId & fromMaybeM (InternalError "fromStopCode doesnt exist")
  _toStopCode <- toStopDetails.gtfsId & fromMaybeM (InternalError "toStopCode doesnt exist")
  let _routeCode = routeDetails.gtfsId
  return $
    FRFSTicketService.FRFSSearchAPIReq
      { fromStationCode = cutAfterColon _fromStopCode,
        toStationCode = cutAfterColon _toStopCode,
        quantity = 1,
        journeySearchData = journeySearchData_,
        routeCode = fmap cutAfterColon _routeCode
      }
  where
    cutAfterColon :: Text -> Text
    cutAfterColon t =
      case breakOn ":" t of
        (_, "") -> t
        (_, after) -> drop 1 after

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
  Maybe (DRL.DriverIdentifier, Seconds) ->
  Flow SearchRequest.SearchRequest
buildSearchRequest searchRequestId mbClientId person pickup merchantOperatingCity mbDrop mbMaxDistance mbDistance startTime returnTime roundTrip bundleVersion clientVersion clientConfigVersion clientRnVersion device disabilityTag duration staticDuration riderPreferredOption distanceUnit totalRidesCount isDashboardRequest mbPlaceNameSource hasStops stops journeySearchData mbDriverReferredInfo = do
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
        driverIdentifier = fst <$> mbDriverReferredInfo,
        ..
      }
  where
    getSearchRequestExpiry :: (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]) => UTCTime -> m UTCTime
    getSearchRequestExpiry time = do
      let mbExpiryTime :: Maybe Seconds = snd <$> mbDriverReferredInfo
      searchRequestExpiry <- maybe 1800 (fromIntegral . orElse mbExpiryTime) <$> asks (.searchRequestExpiry)
      pure $ addUTCTime (fromInteger searchRequestExpiry) time

    -- TODO: Move to shared-kernel
    orElse :: Maybe a -> a -> a
    orElse = flip fromMaybe

calculateDistanceAndRoutes ::
  RiderConfig ->
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  DPerson.Person ->
  Id SearchRequest.SearchRequest ->
  [LatLong] ->
  UTCTime ->
  Flow RouteDetails
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

dummyAddress :: LocationAddress
dummyAddress =
  LocationAddress
    { street = Nothing,
      door = Nothing,
      city = Nothing,
      state = Nothing,
      country = Nothing,
      building = Nothing,
      areaCode = Nothing,
      area = Nothing,
      ward = Nothing,
      placeId = Nothing,
      instructions = Nothing,
      title = Nothing,
      extras = Nothing
    }

convertMultiModalModeToTripMode :: MultiModal.GeneralVehicleType -> Meters -> Meters -> DTrip.TravelMode
convertMultiModalModeToTripMode input distance maximumWalkDistance = case input of
  MultiModal.MetroRail -> DTrip.Metro
  MultiModal.Walk -> if (distance > maximumWalkDistance) then DTrip.Taxi else DTrip.Walk
  MultiModal.Bus -> DTrip.Bus
  MultiModal.Unspecified -> DTrip.Taxi

mapWithIndex :: (MonadFlow m) => (Int -> a -> m b) -> [a] -> m [b]
mapWithIndex f xs = go 0 xs
  where
    go _ [] = return []
    go idx (x : xs') = do
      y <- f idx x
      ys <- go (idx + 1) xs'
      return (y : ys)
