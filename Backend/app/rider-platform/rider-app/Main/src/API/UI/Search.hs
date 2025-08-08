{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Search
  ( DSearch.SearchReq (..),
    DSearch.SearchRes (..),
    DSearch.SearchResp (..),
    DSearch.OneWaySearchReq (..),
    DSearch.PublicTransportSearchReq (..),
    DSearch.RentalSearchReq (..),
    DSearch.SearchReqLocation (..),
    API,
    SearchAPI,
    search',
    search,
    handler,
    searchTrigger',
  )
where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified API.UI.CancelSearch as CancelSearch
import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.ACL.Search as TaxiACL
import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.MultimodalConfirm as DMC
import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Client as DC
import qualified Domain.Types.EstimateStatus as Estimate
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey as Journey
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RideStatus as DRide
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.StationType as Station
import qualified Domain.Types.Trip as DTrip
import Environment
import ExternalBPP.ExternalAPI.CallAPI as CallAPI
import ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare as CRISRouteFare
import Kernel.External.Encryption
import Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.Maps.Types as MapsTypes
import qualified Kernel.External.MultiModal.Interface as MInterface
import qualified Kernel.External.MultiModal.Interface as MultiModal
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import Kernel.Randomizer
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Version
import qualified Lib.JourneyLeg.Taxi as JLT
import qualified Lib.JourneyModule.Base as JM
import qualified Lib.JourneyModule.Types as JMTypes
import qualified Lib.JourneyModule.Utils as JMU
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.Search as DSearch
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Auth
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.MultiModal as TMultiModal
import TransactionLogs.Types

-------- Search Flow --------

data MultimodalWarning
  = NoSingleModeRoutes
  | NoUserPreferredFirstJourney
  | NoPublicTransportRoutes
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data MultimodalSearchResp = MultimodalSearchResp
  { searchId :: Id SearchRequest.SearchRequest,
    searchExpiry :: UTCTime,
    journeys :: [DQuote.JourneyData],
    firstJourney :: Maybe DQuote.JourneyData,
    firstJourneyInfo :: Maybe ApiTypes.JourneyInfoResp,
    showMultimodalWarning :: Bool,
    multimodalWarning :: Maybe MultimodalWarning,
    crisSdkToken :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

type API =
  SearchAPI
    :<|> "multimodalSearch"
      :> TokenAuth
      :> ReqBody '[JSON] DSearch.SearchReq
      :> Header "initateJourney" Bool
      :> Header "x-bundle-version" Version
      :> Header "x-client-version" Version
      :> Header "x-config-version" Version
      :> Header "x-rn-version" Text
      :> Header "client-id" (Id DC.Client)
      :> Header "x-device" Text
      :> Header "is-dashboard-request" Bool
      :> Header "imei-number" Text
      :> Post '[JSON] MultimodalSearchResp

type SearchAPI =
  "rideSearch"
    :> TokenAuth
    :> ReqBody '[JSON] DSearch.SearchReq
    :> Header "x-bundle-version" Version
    :> Header "x-client-version" Version
    :> Header "x-config-version" Version
    :> Header "x-rn-version" Text
    :> Header "client-id" (Id DC.Client)
    :> Header "x-device" Text
    :> Header "is-dashboard-request" Bool
    :> Post '[JSON] DSearch.SearchResp

handler :: FlowServer API
handler = search :<|> multimodalSearchHandler

search :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe (Id DC.Client) -> Maybe Text -> Maybe Bool -> FlowHandler DSearch.SearchResp
search (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice = withFlowHandlerAPI . search' (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice

search' :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe (Id DC.Client) -> Maybe Text -> Maybe Bool -> Flow DSearch.SearchResp
search' (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice mbIsDashboardRequest = withPersonIdLogTag personId $ do
  checkSearchRateLimit personId
  fork "updating person versions" $ updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice
  merchant <- CQM.findById (cast merchantId) >>= fromMaybeM (MerchantNotFound merchantId.getId)
  -- TODO : remove this code after multiple search req issue get fixed from frontend
  --BEGIN
  whenJust merchant.stuckRideAutoCancellationBuffer $ \stuckRideAutoCancellationBuffer -> do
    mbSReq <- QSearchRequest.findLastSearchRequestInKV personId
    shouldCancelPrevSearch <- maybe (return False) (checkValidSearchReq merchant.scheduleRideBufferTime) mbSReq
    when shouldCancelPrevSearch $ do
      fork "handle multiple search request issue" $ do
        case mbSReq of
          Just sReq -> do
            mbEstimate <- QEstimate.findBySRIdAndStatusesInKV sReq.id [Estimate.DRIVER_QUOTE_REQUESTED, Estimate.GOT_DRIVER_QUOTE]
            case mbEstimate of
              Just estimate -> do
                resp <- try @_ @SomeException $ CancelSearch.cancelSearch' (personId, merchantId) estimate.id
                case resp of
                  Left _ -> void $ handleBookingCancellation merchantId personId stuckRideAutoCancellationBuffer sReq.id req
                  Right _ -> pure ()
              Nothing -> void $ handleBookingCancellation merchantId personId stuckRideAutoCancellationBuffer sReq.id req
          _ -> pure ()
  -- TODO : remove this code after multiple search req issue get fixed from frontend
  --END
  dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) Nothing Nothing Nothing False
  fork "search cabs" . withShortRetry $ do
    becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
    let generatedJson = encode becknTaxiReqV2
    logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
    void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 merchantId
  fork "Multimodal Search" $ do
    riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow dSearchRes.searchRequest.merchantOperatingCityId dSearchRes.searchRequest.configInExperimentVersions >>= fromMaybeM (RiderConfigNotFound dSearchRes.searchRequest.merchantOperatingCityId.getId)
    when riderConfig.makeMultiModalSearch $ do
      void (multiModalSearch dSearchRes.searchRequest riderConfig riderConfig.initiateFirstMultimodalJourney True req personId)
  return $ DSearch.SearchResp dSearchRes.searchRequest.id dSearchRes.searchRequestExpiry dSearchRes.shortestRouteInfo
  where
    -- TODO : remove this code after multiple search req issue get fixed from frontend
    --BEGIN
    checkValidSearchReq scheduleRideBufferTime sReq = do
      now <- getCurrentTime
      let isNonScheduled = diffUTCTime sReq.startTime sReq.createdAt < scheduleRideBufferTime
          isValid = sReq.validTill > now
      return $ isNonScheduled && isValid

handleBookingCancellation :: Id Merchant.Merchant -> Id Person.Person -> Seconds -> Id SearchRequest.SearchRequest -> DSearch.SearchReq -> Flow ()
handleBookingCancellation merchantId _personId stuckRideAutoCancellationBuffer sReqId req = do
  mbBooking <- QBooking.findByTransactionIdAndStatus sReqId.getId Booking.activeBookingStatus
  case mbBooking of
    Just booking -> do
      let reasonCode = SCR.CancellationReasonCode "multiple search request issue"
          reasonStage = SCR.OnSearch
      let cancelReq =
            DCancel.CancelReq
              { additionalInfo = Nothing,
                reallocate = Just False,
                blockOnCancellationRate = Nothing,
                ..
              }
      mRide <- QR.findActiveByRBId booking.id
      whenJust mRide $ \ride -> do
        isCancellingAllowed <- checkIfCancellingAllowed ride
        when (ride.status `elem` [DRide.NEW, DRide.UPCOMING] && isCancellingAllowed) $ do
          dCancelRes <- DCancel.cancel booking mRide cancelReq SBCR.ByUser
          void $ withShortRetry $ CallBPP.cancelV2 merchantId dCancelRes.bppUrl =<< ACL.buildCancelReqV2 dCancelRes cancelReq.reallocate
    _ -> pure ()
  where
    checkIfCancellingAllowed ride =
      case req of
        DSearch.OneWaySearch DSearch.OneWaySearchReq {verifyBeforeCancellingOldBooking} -> do
          let verifyBeforeCancelling = fromMaybe False verifyBeforeCancellingOldBooking -- defaulting to old behaviour when flag is not sent from frontend
          if verifyBeforeCancelling
            then do
              now <- getCurrentTime
              if addUTCTime (fromIntegral stuckRideAutoCancellationBuffer) ride.createdAt < now
                then return True
                else throwError (InvalidRequest "ACTIVE_BOOKING_PRESENT") -- 2 mins buffer
            else do
              return True -- this is the old behaviour, to cancel automatically
        _ -> do
          return True

multimodalSearchHandler :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Bool -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe (Id DC.Client) -> Maybe Text -> Maybe Bool -> Maybe Text -> FlowHandler MultimodalSearchResp
multimodalSearchHandler (personId, _merchantId) req mbInitiateJourney mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice mbIsDashboardRequest mbImeiNumber = withFlowHandlerAPI $
  withPersonIdLogTag personId $ do
    checkSearchRateLimit personId
    fork "updating person versions" $ updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice
    whenJust mbImeiNumber $ \imeiNumber -> do
      encryptedImeiNumber <- encrypt imeiNumber
      Person.updateImeiNumber (Just encryptedImeiNumber) personId
    dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) Nothing Nothing Nothing True
    riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow dSearchRes.searchRequest.merchantOperatingCityId dSearchRes.searchRequest.configInExperimentVersions >>= fromMaybeM (RiderConfigNotFound dSearchRes.searchRequest.merchantOperatingCityId.getId)
    let initiateJourney = fromMaybe False mbInitiateJourney
    multiModalSearch dSearchRes.searchRequest riderConfig initiateJourney False req personId

multiModalSearch :: SearchRequest.SearchRequest -> DRC.RiderConfig -> Bool -> Bool -> DSearch.SearchReq -> Id Person.Person -> Flow MultimodalSearchResp
multiModalSearch searchRequest riderConfig initiateJourney forkInitiateFirstJourney req' personId = do
  now <- getCurrentTime
  userPreferences <- DMC.getMultimodalUserPreferences (Just searchRequest.riderId, searchRequest.merchantId)
  let req = DSearch.extractSearchDetails now req'
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  let vehicleCategory = fromMaybe BecknV2.OnDemand.Enums.BUS searchRequest.vehicleCategory
  let isSingleModeMetroSearch = vehicleCategory == BecknV2.OnDemand.Enums.METRO
  let isOutsideMetroBusinessHours = case (riderConfig.qrTicketRestrictionStartTime, riderConfig.qrTicketRestrictionEndTime) of
        (Just start, Just end) -> JM.isWithinTimeBound start end now riderConfig.timeDiffFromUtc
        _ -> False
  when (isSingleModeMetroSearch && isOutsideMetroBusinessHours) $ throwError $ InvalidRequest "Metro booking not allowed outside business hours"
  mbIntegratedBPPConfig <- SIBC.findMaybeIntegratedBPPConfig Nothing merchantOperatingCityId vehicleCategory (fromMaybe DIBC.MULTIMODAL req.platformType)
  mbSingleModeRouteDetails <-
    case mbIntegratedBPPConfig of
      Just config ->
        JMU.measureLatency (JMU.getSingleModeRouteDetails searchRequest.routeCode searchRequest.originStopCode searchRequest.destinationStopCode config searchRequest.merchantId searchRequest.merchantOperatingCityId vehicleCategory) ("getSingleModeRouteDetails" <> show vehicleCategory <> " routeCode: " <> show searchRequest.routeCode <> " originStopCode: " <> show searchRequest.originStopCode <> " destinationStopCode: " <> show searchRequest.destinationStopCode)
      Nothing -> return Nothing
  logDebug $ "mbSingleModeRouteDetails: " <> show mbSingleModeRouteDetails
  (singleModeWarningType, otpResponse) <- case mbSingleModeRouteDetails of
    Just singleModeRouteDetails -> do
      let fromStopDetails =
            MultiModalTypes.MultiModalStopDetails
              { stopCode = Just singleModeRouteDetails.fromStop.stopCode,
                platformCode = singleModeRouteDetails.fromStop.platformNumber,
                name = Just singleModeRouteDetails.fromStop.stopName,
                gtfsId = Just singleModeRouteDetails.fromStop.stopCode
              }
      let toStopDetails =
            MultiModalTypes.MultiModalStopDetails
              { stopCode = Just singleModeRouteDetails.toStop.stopCode,
                platformCode = singleModeRouteDetails.toStop.platformNumber,
                name = Just singleModeRouteDetails.toStop.stopName,
                gtfsId = Just singleModeRouteDetails.toStop.stopCode
              }
      let fromStopLocation = LocationV2 {latLng = LatLngV2 {latitude = singleModeRouteDetails.fromStop.stopLat, longitude = singleModeRouteDetails.fromStop.stopLon}}
      let toStopLocation = LocationV2 {latLng = LatLngV2 {latitude = singleModeRouteDetails.toStop.stopLat, longitude = singleModeRouteDetails.toStop.stopLon}}
      let distance = fromMaybe (Distance 0 Meter) searchRequest.distance
      let duration = nominalDiffTimeToSeconds $ diffUTCTime singleModeRouteDetails.toStop.stopArrivalTime singleModeRouteDetails.fromStop.stopArrivalTime
      let currentLocation = fmap latLongToLocationV2 req.currentLocation
      mbPreliminaryLeg <- getPreliminaryLeg now currentLocation fromStopLocation
      let subLegOrder = if isJust mbPreliminaryLeg then 2 else 1
      let leg =
            MultiModalTypes.MultiModalLeg
              { distance = distance,
                duration = duration,
                polyline = Polyline {encodedPolyline = fromMaybe "" singleModeRouteDetails.route.polyline},
                mode = castVehicleCategoryToGeneralVehicleType vehicleCategory,
                startLocation = fromStopLocation,
                endLocation = toStopLocation,
                fromStopDetails = Just fromStopDetails,
                toStopDetails = Just toStopDetails,
                routeDetails =
                  [ MultiModalTypes.MultiModalRouteDetails
                      { gtfsId = Just singleModeRouteDetails.route.code,
                        longName = Just singleModeRouteDetails.route.longName,
                        shortName = Just singleModeRouteDetails.route.shortName,
                        alternateShortNames = singleModeRouteDetails.availableRoutes,
                        color = singleModeRouteDetails.route.color,
                        fromStopDetails = Just fromStopDetails,
                        toStopDetails = Just toStopDetails,
                        startLocation = fromStopLocation,
                        endLocation = toStopLocation,
                        subLegOrder,
                        fromArrivalTime = Just singleModeRouteDetails.fromStop.stopArrivalTime,
                        fromDepartureTime = Just singleModeRouteDetails.fromStop.stopArrivalTime,
                        toArrivalTime = Just singleModeRouteDetails.toStop.stopArrivalTime,
                        toDepartureTime = Just singleModeRouteDetails.toStop.stopArrivalTime
                      }
                  ],
                serviceTypes = [],
                agency = Nothing,
                fromArrivalTime = Just singleModeRouteDetails.fromStop.stopArrivalTime,
                fromDepartureTime = Just singleModeRouteDetails.fromStop.stopArrivalTime,
                toArrivalTime = Just singleModeRouteDetails.toStop.stopArrivalTime,
                toDepartureTime = Just singleModeRouteDetails.toStop.stopArrivalTime,
                entrance = Nothing,
                exit = Nothing
              }
      legs <- case mbPreliminaryLeg of
        Just preliminaryLeg -> return [preliminaryLeg, leg]
        Nothing -> return [leg]
      return $
        ( Nothing,
          MInterface.MultiModalResponse
            { routes =
                [ MultiModalTypes.MultiModalRoute
                    { distance = distance,
                      duration = duration,
                      startTime = Just singleModeRouteDetails.fromStop.stopArrivalTime,
                      endTime = Just singleModeRouteDetails.toStop.stopArrivalTime,
                      legs = legs,
                      relevanceScore = Nothing
                    }
                ]
            }
        )
    _ -> do
      let permissibleModesToUse =
            if (not isSingleModeMetroSearch) && isOutsideMetroBusinessHours
              then filter (/= MultiModalTypes.MetroRail) (fromMaybe [] riderConfig.permissibleModes)
              else fromMaybe [] riderConfig.permissibleModes
      let sortingType = fromMaybe DMP.FASTEST userPreferences.journeyOptionsSortingType
      destination <- extractDest searchRequest.toLocation
      -- Get stop information if integrated BPP config is available
      fromStopInfo <- case (mbIntegratedBPPConfig, searchRequest.originStopCode) of
        (Just integratedBPPConfig, Just originStopCode) ->
          OTPRest.getStationByGtfsIdAndStopCode originStopCode integratedBPPConfig
        _ ->
          return Nothing

      let searchReqLoc :: LatLngV2 =
            LatLngV2
              { latitude = searchRequest.fromLocation.lat,
                longitude = searchRequest.fromLocation.lon
              }

      -- Determine the from location based on request type and stop info
      let fromLocation :: LatLngV2 = case (req', fromStopInfo) of
            (DSearch.PTSearch _, Just stopInfo) ->
              case (stopInfo.lat, stopInfo.lon) of
                (Just lat, Just lon) ->
                  LatLngV2
                    { latitude = lat,
                      longitude = lon
                    }
                _ -> searchReqLoc
            _ -> searchReqLoc

      let transitRoutesReq =
            GetTransitRoutesReq
              { origin = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = fromLocation.latitude, longitude = fromLocation.longitude}}},
                destination = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = destination.lat, longitude = destination.lon}}},
                arrivalTime = Nothing,
                departureTime = Nothing,
                mode = Nothing,
                transitPreferences = Nothing,
                transportModes = Nothing,
                minimumWalkDistance = riderConfig.minimumWalkDistance,
                permissibleModes = permissibleModesToUse,
                maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs,
                sortingType = JMU.convertSortingType sortingType
              }
      transitServiceReq <- TMultiModal.getTransitServiceReq searchRequest.merchantId merchantOperatingCityId
      otpResponse' <- JMU.measureLatency (MultiModal.getTransitRoutes (Just searchRequest.id.getId) transitServiceReq transitRoutesReq >>= fromMaybeM (InternalError "routes dont exist")) "getTransitRoutes"
      let otpResponse'' = MInterface.MultiModalResponse (map mkRouteDetailsForWalkLegs otpResponse'.routes)
      logDebug $ "[Multimodal - OTP Response]" <> show otpResponse''
      -- Add default auto leg if no routes are found
      if null otpResponse''.routes
        then do
          case searchRequest.toLocation of
            Just toLocation -> do
              let toLocationV2 = LocationV2 {latLng = LatLngV2 {latitude = toLocation.lat, longitude = toLocation.lon}}
              let distance = fromMaybe (Distance 0 Meter) searchRequest.distance
              let duration = fromMaybe (Seconds 0) searchRequest.estimatedRideDuration
              (autoLeg, startTime, endTime) <- mkAutoOrWalkLeg now Nothing toLocationV2 MultiModalTypes.Unspecified distance duration
              let autoMultiModalResponse = mkMultimodalResponse autoLeg startTime endTime
              return (Just NoPublicTransportRoutes, autoMultiModalResponse)
            Nothing -> return (Nothing, otpResponse'')
        else do
          case req' of
            DSearch.PTSearch _ -> do
              let onlySingleModeRoutes = filter (\r -> (all (eitherWalkOrSingleMode vehicleCategory) r.legs) && (any (onlySingleMode vehicleCategory) r.legs)) otpResponse''.routes
              let filterFirstAndLastMileWalks = map filterWalkLegs onlySingleModeRoutes
              let routesWithCorrectStops = map filterRoutesWithCorrectFromAndToLocations filterFirstAndLastMileWalks
              let validRoutes = filter (\r -> not (null r.legs)) routesWithCorrectStops
              let warningType = if null validRoutes then Just NoSingleModeRoutes else Nothing
              filteredRoutes <- JM.filterTransitRoutes riderConfig (if null validRoutes then otpResponse''.routes else validRoutes)
              return (warningType, MInterface.MultiModalResponse {routes = filteredRoutes})
            _ -> do
              filteredRoutes <- JM.filterTransitRoutes riderConfig otpResponse''.routes
              return (Nothing, MInterface.MultiModalResponse {routes = filteredRoutes})

  let userPreferredTransitModes = userPreferencesToGeneralVehicleTypes userPreferences.allowedTransitModes
      hasOnlyUserPreferredTransitModes otpRoute = all (isLegModeIn userPreferredTransitModes) otpRoute.legs
      hasOnlyWalkOrUnspecifiedTransitModes otpRoute = all (isLegModeIn [MultiModalTypes.Walk, MultiModalTypes.Unspecified]) otpRoute.legs
      indexedRoutes = zip [0 ..] otpResponse.routes
      removeOnlyWalkAndUnspecifiedTransitModes = filter (not . hasOnlyWalkOrUnspecifiedTransitModes . snd)
      filteredUserPreferredIndexedRoutes = filter (hasOnlyUserPreferredTransitModes . snd) indexedRoutes
      baseRoutes = if null filteredUserPreferredIndexedRoutes then indexedRoutes else filteredUserPreferredIndexedRoutes
      indexedRoutesToProcess =
        if riderConfig.filterWalkAndUnspecifiedTransitModes
          then removeOnlyWalkAndUnspecifiedTransitModes baseRoutes
          else baseRoutes
      showMultimodalWarningForFirstJourney = null filteredUserPreferredIndexedRoutes

  mbJourneyWithIndex <- JMU.measureLatency (go indexedRoutesToProcess userPreferences) "Multimodal Init Time" -- process until first journey is found
  QSearchRequest.updateHasMultimodalSearch (Just True) searchRequest.id

  journeys <- DQuote.getJourneys searchRequest (Just True)
  {-
    - Here we are calling routeFare with correct details only once
    - This is done because user can have only one valid sdk token at a time
    - We are calling routeFare with dummy details for rest of the legs just to get the fare details
  -}
  mbCrisSdkToken <- getCrisSdkToken merchantOperatingCityId journeys
  let mbFirstJourney = listToMaybe (fromMaybe [] journeys)
  firstJourneyInfo <-
    if initiateJourney
      then do
        case mbJourneyWithIndex of
          Just (idx, firstJourney) -> do
            resp <-
              if forkInitiateFirstJourney
                then do
                  fork "Initiate first the route" $ do
                    void $ DMC.postMultimodalInitiate (Just searchRequest.riderId, searchRequest.merchantId) firstJourney.id
                  return Nothing
                else do
                  res <- DMC.postMultimodalInitiate (Just searchRequest.riderId, searchRequest.merchantId) firstJourney.id
                  return $ Just res {ApiTypes.crisSdkToken = mbCrisSdkToken}
            fork "Rest of the routes Init" $ processRestOfRoutes [route' | (j, route') <- indexedRoutesToProcess, j /= idx] userPreferences
            return resp
          Nothing -> do
            QSearchRequest.updateAllJourneysLoaded (Just True) searchRequest.id
            return Nothing
      else do
        QSearchRequest.updateAllJourneysLoaded (Just True) searchRequest.id
        return Nothing

  return $
    MultimodalSearchResp
      { searchId = searchRequest.id,
        crisSdkToken = mbCrisSdkToken,
        searchExpiry = searchRequest.validTill,
        journeys = fromMaybe [] journeys,
        firstJourney = mbFirstJourney,
        firstJourneyInfo = firstJourneyInfo,
        showMultimodalWarning = isJust singleModeWarningType || showMultimodalWarningForFirstJourney,
        multimodalWarning =
          if isJust singleModeWarningType
            then singleModeWarningType
            else
              if showMultimodalWarningForFirstJourney
                then Just NoUserPreferredFirstJourney
                else Nothing
      }
  where
    go :: [(Int, MultiModalTypes.MultiModalRoute)] -> ApiTypes.MultimodalUserPreferences -> Flow (Maybe (Int, Journey.Journey))
    go [] _ = return Nothing
    go ((idx, r) : routes) userPreferences = do
      mbResult <- processRoute r userPreferences
      case mbResult of
        Nothing -> go routes userPreferences
        Just journey -> return $ Just (idx, journey)

    processRoute :: MultiModalTypes.MultiModalRoute -> ApiTypes.MultimodalUserPreferences -> Flow (Maybe Journey.Journey)
    processRoute r userPreferences = do
      updatedRoute <- updateRouteWithLegDurations r
      let initReq =
            JMTypes.JourneyInitData
              { parentSearchId = searchRequest.id,
                merchantId = searchRequest.merchantId,
                merchantOperatingCityId = searchRequest.merchantOperatingCityId,
                personId = searchRequest.riderId,
                legs = updatedRoute.legs,
                estimatedDistance = updatedRoute.distance,
                estimatedDuration = updatedRoute.duration,
                startTime = updatedRoute.startTime,
                endTime = updatedRoute.endTime,
                maximumWalkDistance = riderConfig.maximumWalkDistance,
                straightLineThreshold = riderConfig.straightLineThreshold,
                relevanceScore = updatedRoute.relevanceScore
              }
      JM.init initReq userPreferences

    updateRouteWithLegDurations :: MultiModalTypes.MultiModalRoute -> Flow MultiModalTypes.MultiModalRoute
    updateRouteWithLegDurations route_ = do
      updatedLegs <- mapM calculateLegProportionalDuration route_.legs
      let totalDuration = sum (map (.duration) updatedLegs)
          -- Update endTime based on the new duration
          updatedEndTime =
            route_.startTime >>= \startTime ->
              Just $ addUTCTime (secondsToNominalDiffTime totalDuration) startTime
          route' = route_ {duration = totalDuration, legs = updatedLegs, endTime = updatedEndTime} :: MultiModalTypes.MultiModalRoute
      return route'

    -- Calculate proportional duration only for Walk and Unspecified legs
    calculateLegProportionalDuration :: MultiModalTypes.MultiModalLeg -> Flow MultiModalTypes.MultiModalLeg
    calculateLegProportionalDuration leg = do
      let totalEstimatedDuration = fromMaybe (Seconds 0) searchRequest.estimatedRideDuration
          totalEstimatedDistance = fromMaybe (Distance 0 Meter) searchRequest.distance
      case leg.mode of
        MultiModalTypes.Walk ->
          if (distanceToMeters leg.distance) > riderConfig.maximumWalkDistance
            then do
              -- Call OSRM for taxi/auto (car) distance/duration, fallback to proportional duration if it fails
              res <-
                try @_ @SomeException $
                  Maps.getMultimodalWalkDistance searchRequest.merchantId searchRequest.merchantOperatingCityId (Just searchRequest.id.getId) $
                    Maps.GetDistanceReq
                      { origin = Maps.LatLong {lat = leg.startLocation.latLng.latitude, lon = leg.startLocation.latLng.longitude},
                        destination = Maps.LatLong {lat = leg.endLocation.latLng.latitude, lon = leg.endLocation.latLng.longitude},
                        travelMode = Just Maps.CAR,
                        sourceDestinationMapping = Nothing,
                        distanceUnit = Meter
                      }
              case res of
                Right distResp -> do
                  let newDistance = if distResp.distance > 0 then convertMetersToDistance Meter distResp.distance else leg.distance
                      updatedDurationLeg = updateDuration totalEstimatedDuration totalEstimatedDistance leg {distance = newDistance}
                  return (updatedDurationLeg {duration = updatedDurationLeg.duration} :: MultiModalTypes.MultiModalLeg)
                Left err -> do
                  logError $ "OSRM/Maps.getMultimodalWalkDistance failed: " <> show err <> ", falling back to proportional duration and distance" <> "latlong: " <> show (leg.startLocation.latLng.latitude, leg.startLocation.latLng.longitude) <> " " <> show (leg.endLocation.latLng.latitude, leg.endLocation.latLng.longitude)
                  return $ updateDuration totalEstimatedDuration totalEstimatedDistance leg
            else return leg
        MultiModalTypes.Unspecified -> return $ updateDuration totalEstimatedDuration totalEstimatedDistance leg
        _ -> return leg -- Skip other modes
      where
        updateDuration :: Seconds -> Distance -> MultiModalTypes.MultiModalLeg -> MultiModalTypes.MultiModalLeg
        updateDuration totalEstimatedDuration totalEstimatedDistance leg' =
          let legDistance = leg.distance
              proportionalDuration =
                if totalEstimatedDistance > Distance 0 Meter
                  then
                    let totalSecs = fromIntegral totalEstimatedDuration.getSeconds :: Double
                        legMeters = fromIntegral (distanceToMeters legDistance) :: Double
                        totalMeters = fromIntegral (distanceToMeters totalEstimatedDistance) :: Double
                        propSecs = if totalMeters > 0 then totalSecs * legMeters / totalMeters else 0
                     in Seconds $ round propSecs
                  else leg.duration

              updateTimingWithBuffer mbFromTime mbToArrival mbToDeparture =
                case (mbFromTime, mbToArrival, mbToDeparture) of
                  (Just fromTime, Just arrival, Just departure) ->
                    let newToArrival = Just $ addUTCTime (secondsToNominalDiffTime proportionalDuration) fromTime
                        originalBuffer = abs (diffUTCTime departure arrival)
                        newToDeparture = Just $ addUTCTime originalBuffer (fromJust newToArrival)
                     in (newToArrival, newToDeparture)
                  _ -> (mbToArrival, mbToDeparture)

              updatedRouteDetails = map updateRouteDetailTiming leg.routeDetails
              updateRouteDetailTiming :: MultiModalTypes.MultiModalRouteDetails -> MultiModalTypes.MultiModalRouteDetails
              updateRouteDetailTiming routeDetail =
                let (newToArrival, newToDeparture) =
                      updateTimingWithBuffer
                        routeDetail.fromDepartureTime
                        routeDetail.toArrivalTime
                        routeDetail.toDepartureTime
                 in routeDetail
                      { toArrivalTime = newToArrival,
                        toDepartureTime = newToDeparture
                      }

              -- Update leg timing
              (newLegToArrival, newLegToDeparture) = updateTimingWithBuffer leg.fromDepartureTime leg.toArrivalTime leg.toDepartureTime
           in leg'
                { duration = proportionalDuration,
                  toArrivalTime = newLegToArrival,
                  toDepartureTime = newLegToDeparture,
                  routeDetails = updatedRouteDetails
                }

    eitherWalkOrSingleMode :: BecknV2.OnDemand.Enums.VehicleCategory -> MultiModalTypes.MultiModalLeg -> Bool
    eitherWalkOrSingleMode selectedMode leg = isLegModeIn [MultiModalTypes.Walk, MultiModalTypes.Unspecified, castVehicleCategoryToGeneralVehicleType selectedMode] leg

    onlySingleMode :: BecknV2.OnDemand.Enums.VehicleCategory -> MultiModalTypes.MultiModalLeg -> Bool
    onlySingleMode selectedMode leg = leg.mode == castVehicleCategoryToGeneralVehicleType selectedMode

    filterWalkLegs :: MultiModalTypes.MultiModalRoute -> MultiModalTypes.MultiModalRoute
    filterWalkLegs MultiModalTypes.MultiModalRoute {..} = do
      let legsWithIndex = zip [0 ..] legs
      let filteredLegs =
            [ leg
              | (index, leg) <- legsWithIndex,
                not ((index == 0 || index == length legs - 1) && isLegModeIn [MultiModalTypes.Walk, MultiModalTypes.Unspecified] leg)
            ]
      MultiModalTypes.MultiModalRoute {legs = if null filteredLegs then legs else filteredLegs, ..}

    filterRoutesWithCorrectFromAndToLocations :: MultiModalTypes.MultiModalRoute -> MultiModalTypes.MultiModalRoute
    filterRoutesWithCorrectFromAndToLocations multiModalRoute = do
      let routeLegs = multiModalRoute.legs
      case (listToMaybe routeLegs, listToMaybe $ reverse routeLegs) of
        (Just firstLeg, Just lastLeg) -> do
          let originStopCode = searchRequest.originStopCode
              destinationStopCode = searchRequest.destinationStopCode
              firstLegFromStopCode = firstLeg.fromStopDetails >>= (.stopCode)
              lastLegToStopCode = lastLeg.toStopDetails >>= (.stopCode)

          -- Check if the route starts from the correct origin stop and ends at the correct destination stop
          let isValidRoute = case (originStopCode, destinationStopCode) of
                (Just originCode, Just destCode) ->
                  firstLegFromStopCode == Just originCode && lastLegToStopCode == Just destCode
                (Just originCode, Nothing) ->
                  firstLegFromStopCode == Just originCode -- Only check origin if destination not specified
                (Nothing, Just destCode) ->
                  lastLegToStopCode == Just destCode -- Only check destination if origin not specified
                (Nothing, Nothing) ->
                  True -- If neither stop codes are provided, consider route valid
          if isValidRoute then multiModalRoute else multiModalRoute {MultiModalTypes.legs = []} -- Return empty legs if route doesn't match
        _ -> multiModalRoute -- If no legs, return as is
    processRestOfRoutes :: [MultiModalTypes.MultiModalRoute] -> ApiTypes.MultimodalUserPreferences -> Flow ()
    processRestOfRoutes routes userPreferences = do
      forM_ routes $ \route' -> processRoute route' userPreferences
      QSearchRequest.updateAllJourneysLoaded (Just True) searchRequest.id

    extractDest Nothing = throwError $ InvalidRequest "Destination is required for multimodal search"
    extractDest (Just d) = return d

    userPreferencesToGeneralVehicleTypes :: [DTrip.MultimodalTravelMode] -> [GeneralVehicleType]
    userPreferencesToGeneralVehicleTypes = map allowedTransitModeToGeneralVehicleType

    allowedTransitModeToGeneralVehicleType :: DTrip.MultimodalTravelMode -> GeneralVehicleType
    allowedTransitModeToGeneralVehicleType mode = case mode of
      DTrip.Bus -> MultiModalTypes.Bus
      DTrip.Metro -> MultiModalTypes.MetroRail
      DTrip.Subway -> MultiModalTypes.Subway
      DTrip.Walk -> MultiModalTypes.Walk
      _ -> MultiModalTypes.Unspecified

    castVehicleCategoryToGeneralVehicleType :: BecknV2.OnDemand.Enums.VehicleCategory -> GeneralVehicleType
    castVehicleCategoryToGeneralVehicleType vehicleCategory = case vehicleCategory of
      BecknV2.OnDemand.Enums.BUS -> MultiModalTypes.Bus
      BecknV2.OnDemand.Enums.METRO -> MultiModalTypes.MetroRail
      BecknV2.OnDemand.Enums.SUBWAY -> MultiModalTypes.Subway
      _ -> MultiModalTypes.Unspecified

    mkRouteDetailsForWalkLegs :: MultiModalTypes.MultiModalRoute -> MultiModalTypes.MultiModalRoute
    mkRouteDetailsForWalkLegs MultiModalTypes.MultiModalRoute {..} =
      let mkRouteDetail leg =
            [ MultiModalTypes.MultiModalRouteDetails
                { gtfsId = Nothing,
                  longName = Nothing,
                  shortName = Nothing,
                  alternateShortNames = [],
                  color = Nothing,
                  fromStopDetails = leg.fromStopDetails,
                  toStopDetails = leg.toStopDetails,
                  startLocation = leg.startLocation,
                  endLocation = leg.endLocation,
                  subLegOrder = 1,
                  fromArrivalTime = leg.fromArrivalTime,
                  fromDepartureTime = leg.fromDepartureTime,
                  toArrivalTime = leg.toArrivalTime,
                  toDepartureTime = leg.toDepartureTime
                }
            ]
       in MultiModalTypes.MultiModalRoute
            { legs =
                map
                  ( \leg ->
                      leg{routeDetails =
                            if null leg.routeDetails
                              then mkRouteDetail leg
                              else leg.routeDetails
                         }
                  )
                  legs,
              ..
            }

    mkAutoOrWalkLeg :: UTCTime -> Maybe LocationV2 -> LocationV2 -> MultiModalTypes.GeneralVehicleType -> Distance -> Seconds -> Flow (MultiModalTypes.MultiModalLeg, UTCTime, UTCTime)
    mkAutoOrWalkLeg now fromLocation toLocation mode distance duration = do
      let fromStopLocation = LocationV2 {latLng = LatLngV2 {latitude = searchRequest.fromLocation.lat, longitude = searchRequest.fromLocation.lon}}
      let toStopLocation = toLocation
      let startLocation = fromMaybe fromStopLocation fromLocation
      let startTime = now
      let endTime = addUTCTime (secondsToNominalDiffTime duration) startTime
      return
        ( MultiModalTypes.MultiModalLeg
            { distance = distance,
              duration = duration,
              polyline = Polyline {encodedPolyline = ""},
              mode,
              startLocation,
              endLocation = toStopLocation,
              fromStopDetails = Nothing,
              toStopDetails = Nothing,
              routeDetails =
                [ MultiModalTypes.MultiModalRouteDetails
                    { gtfsId = Nothing,
                      longName = Nothing,
                      shortName = Nothing,
                      color = Nothing,
                      alternateShortNames = [],
                      fromStopDetails = Nothing,
                      toStopDetails = Nothing,
                      startLocation,
                      endLocation = toStopLocation,
                      subLegOrder = 1,
                      fromArrivalTime = Just startTime,
                      fromDepartureTime = Just startTime,
                      toArrivalTime = Just endTime,
                      toDepartureTime = Just endTime
                    }
                ],
              serviceTypes = [],
              agency = Nothing,
              fromArrivalTime = Just startTime,
              fromDepartureTime = Just startTime,
              toArrivalTime = Just endTime,
              toDepartureTime = Just endTime,
              entrance = Nothing,
              exit = Nothing
            },
          startTime,
          endTime
        )

    isLegModeIn :: [GeneralVehicleType] -> MultiModalTypes.MultiModalLeg -> Bool
    isLegModeIn modes leg = leg.mode `elem` modes

    getCrisSdkToken :: Id MerchantOperatingCity -> Maybe [DQuote.JourneyData] -> Flow (Maybe Text)
    getCrisSdkToken _ Nothing = return Nothing
    getCrisSdkToken merchantOperatingCityId (Just journeys) = do
      person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      let subwayJourneys = filter (\j -> any (\leg -> leg.journeyMode == DTrip.Subway) j.journeyLegs) journeys
      if null subwayJourneys
        then return Nothing
        else do
          mbMobileNumber <- mapM decrypt person.mobileNumber
          mbImeiNumber <- mapM decrypt person.imeiNumber
          sessionId <- getRandomInRange (1, 1000000 :: Int)
          findValidSdkToken subwayJourneys mbMobileNumber mbImeiNumber sessionId
      where
        findValidSdkToken :: [DQuote.JourneyData] -> Maybe Text -> Maybe Text -> Int -> Flow (Maybe Text)
        findValidSdkToken [] _ _ _ = return Nothing
        findValidSdkToken (journey : restJourneys) mbMobileNumber mbImeiNumber sessionId = do
          let subwayLegs = filter (\leg -> leg.journeyMode == DTrip.Subway) journey.journeyLegs
          mbSdkToken <- findValidSdkTokenFromLegs subwayLegs mbMobileNumber mbImeiNumber sessionId
          case mbSdkToken of
            Just token -> return $ Just token
            Nothing -> findValidSdkToken restJourneys mbMobileNumber mbImeiNumber sessionId

        findValidSdkTokenFromLegs :: [DQuote.JourneyLeg] -> Maybe Text -> Maybe Text -> Int -> Flow (Maybe Text)
        findValidSdkTokenFromLegs [] _ _ _ = return Nothing
        findValidSdkTokenFromLegs (leg : restLegs) mbMobileNumber mbImeiNumber sessionId = do
          mbSdkToken <- tryGetSdkTokenFromLeg leg mbMobileNumber mbImeiNumber sessionId
          case mbSdkToken of
            Just token -> return $ Just token
            Nothing -> findValidSdkTokenFromLegs restLegs mbMobileNumber mbImeiNumber sessionId

        tryGetSdkTokenFromLeg :: DQuote.JourneyLeg -> Maybe Text -> Maybe Text -> Int -> Flow (Maybe Text)
        tryGetSdkTokenFromLeg leg mbMobileNumber mbImeiNumber sessionId = do
          let mbRouteCode = listToMaybe leg.routeDetails >>= (.routeCode)
          case (leg.fromStationCode, leg.toStationCode, mbRouteCode) of
            (Just fromCode, Just toCode, Just routeCode) -> do
              SIBC.findMaybeIntegratedBPPConfig Nothing merchantOperatingCityId BecknV2.OnDemand.Enums.SUBWAY DIBC.MULTIMODAL
                >>= \case
                  Just integratedBPPConfig -> do
                    case integratedBPPConfig.providerConfig of
                      DIBC.CRIS config' -> do
                        intermediateStations <- CallAPI.buildStations routeCode fromCode toCode integratedBPPConfig Station.START Station.END

                        let viaStations = T.intercalate "-" $ map (.stationCode) $ filter (\station -> station.stationType == Station.INTERMEDIATE) intermediateStations
                            viaPoints = if T.null viaStations then " " else viaStations

                        let routeFareReq =
                              CRISRouteFare.CRISFareRequest
                                { mobileNo = mbMobileNumber,
                                  imeiNo = fromMaybe "ed409d8d764c04f7" mbImeiNumber,
                                  appSession = sessionId,
                                  sourceCode = fromCode,
                                  changeOver = " ",
                                  destCode = toCode,
                                  via = viaPoints
                                }

                        fares <- CRISRouteFare.getRouteFare config' merchantOperatingCityId routeFareReq <&> listToMaybe
                        return $ fares >>= (.fareDetails) >>= Just . (.sdkToken)
                      _ -> return Nothing
                  Nothing -> return Nothing
            _ -> return Nothing

    mkMultimodalResponse :: MultiModalTypes.MultiModalLeg -> UTCTime -> UTCTime -> MultiModalTypes.MultiModalResponse
    mkMultimodalResponse leg startTime endTime =
      MInterface.MultiModalResponse
        { routes =
            [ MultiModalTypes.MultiModalRoute
                { distance = leg.distance,
                  duration = leg.duration,
                  startTime = Just startTime,
                  endTime = Just endTime,
                  legs = [leg],
                  relevanceScore = Nothing
                }
            ]
        }

    getPreliminaryLeg :: UTCTime -> Maybe LocationV2 -> LocationV2 -> Flow (Maybe MultiModalTypes.MultiModalLeg)
    getPreliminaryLeg now mbCurrentLocation fromStopLocation = do
      case mbCurrentLocation of
        Nothing -> return Nothing
        Just currentLocation -> do
          let fromLocation = locationV2ToLatLong currentLocation
              toLocation = locationV2ToLatLong fromStopLocation

          mbGetDistanceResp <- getDistanceAndDuration fromLocation toLocation Maps.FOOT
          case mbGetDistanceResp of
            Just getDistanceResp -> do
              let distance = getDistanceResp.distance
                  duration = getDistanceResp.duration
              if distance < riderConfig.minimumWalkDistance
                then return Nothing
                else do
                  (leg, _, _) <- mkAutoOrWalkLeg now (Just currentLocation) fromStopLocation MultiModalTypes.Walk (convertMetersToDistance Meter distance) duration
                  return $ Just leg
            Nothing -> return Nothing

    getDistanceAndDuration :: Maps.LatLong -> Maps.LatLong -> Maps.TravelMode -> Flow (Maybe (Maps.GetDistanceResp Maps.LatLong Maps.LatLong))
    getDistanceAndDuration fromLocation toLocation travelMode = do
      resp <-
        try @_ @SomeException $
          Maps.getMultimodalWalkDistance searchRequest.merchantId searchRequest.merchantOperatingCityId (Just searchRequest.id.getId) $
            Maps.GetDistanceReq
              { origin = fromLocation,
                destination = toLocation,
                travelMode = Just travelMode,
                sourceDestinationMapping = Nothing,
                distanceUnit = Meter
              }
      case resp of
        Right distResp -> return $ Just distResp
        Left err -> do
          logError $ "getMultimodalWalkDistance failed: " <> show err <> "latlong: " <> show (fromLocation, toLocation) <> "travelMode: " <> show travelMode
          return Nothing

    latLongToLocationV2 :: MapsTypes.LatLong -> LocationV2
    latLongToLocationV2 latLong = LocationV2 {latLng = LatLngV2 {latitude = latLong.lat, longitude = latLong.lon}}

    locationV2ToLatLong :: LocationV2 -> Maps.LatLong
    locationV2ToLatLong locationV2 = Maps.LatLong {lat = locationV2.latLng.latitude, lon = locationV2.latLng.longitude}

checkSearchRateLimit ::
  ( Redis.HedisFlow m r,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text]
  ) =>
  Id Person.Person ->
  m ()
checkSearchRateLimit personId = do
  let key = searchHitsCountKey personId
  hitsLimit <- asks (.searchRateLimitOptions.limit)
  limitResetTimeInSec <- asks (.searchRateLimitOptions.limitResetTimeInSec)
  unlessM (slidingWindowLimiter key hitsLimit limitResetTimeInSec) $ do
    msgTemplate <- asks (.searchLimitExceedNotificationTemplate)
    let message = T.replace "{#cust-id#}" (getId personId) msgTemplate
    _ <- SF.postMessage message
    throwError $ HitsLimitError limitResetTimeInSec

searchHitsCountKey :: Id Person.Person -> Text
searchHitsCountKey personId = "BAP:Ride:search:" <> getId personId <> ":hitsCount"

updateVersions :: (CacheFlow m r, EsqDBFlow m r, HasFlowEnv m r '["version" ::: DeploymentVersion]) => Id Person.Person -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> m ()
updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  deploymentVersion <- asks (.version)
  void $ Person.updatePersonVersions person mbBundleVersion mbClientVersion mbClientConfigVersion (getDeviceFromText mbDevice) deploymentVersion.getDeploymentVersion mbRnVersion

searchTrigger' ::
  ( DSearch.SearchRequestFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    Redis.HedisFlow m r,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text],
    MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  (Id Person.Person, Id Merchant.Merchant) ->
  DSearch.SearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe (Id DC.Client) ->
  Maybe Text ->
  Maybe Bool ->
  m DSearch.SearchResp
searchTrigger' (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice mbIsDashboardRequest = withPersonIdLogTag personId $ do
  checkSearchRateLimit personId
  fork "updating person versions" $ updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice
  merchant <- CQM.findById (cast merchantId) >>= fromMaybeM (MerchantNotFound merchantId.getId)
  -- TODO : remove this code after multiple search req issue get fixed from frontend
  --BEGIN
  whenJust merchant.stuckRideAutoCancellationBuffer $ \stuckRideAutoCancellationBuffer -> do
    mbSReq <- QSearchRequest.findLastSearchRequestInKV personId
    shouldCancelPrevSearch <- maybe (return False) (checkValidSearchReq merchant.scheduleRideBufferTime) mbSReq
    when shouldCancelPrevSearch $ do
      fork "handle multiple search request issue" $ do
        case mbSReq of
          Just sReq -> do
            mbEstimate <- QEstimate.findBySRIdAndStatusesInKV sReq.id [Estimate.DRIVER_QUOTE_REQUESTED, Estimate.GOT_DRIVER_QUOTE]
            case mbEstimate of
              Just estimate -> do
                resp <- try @_ @SomeException $ JLT.cancelSearch' (personId, merchantId) estimate.id
                case resp of
                  Left _ -> void $ handleBookingCancellation' merchantId personId stuckRideAutoCancellationBuffer sReq.id req
                  Right _ -> pure ()
              Nothing -> void $ handleBookingCancellation' merchantId personId stuckRideAutoCancellationBuffer sReq.id req
          _ -> pure ()
  -- TODO : remove this code after multiple search req issue get fixed from frontend
  --END
  dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) Nothing Nothing Nothing False
  fork "search cabs" . withShortRetry $ do
    becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
    let generatedJson = encode becknTaxiReqV2
    logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
    void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 merchantId
  fork "Multimodal Search" $ do
    riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow dSearchRes.searchRequest.merchantOperatingCityId dSearchRes.searchRequest.configInExperimentVersions >>= fromMaybeM (RiderConfigNotFound dSearchRes.searchRequest.merchantOperatingCityId.getId)
    when riderConfig.makeMultiModalSearch $ throwError $ InvalidRequest "Multimodal not supported currently for Ny Regular" -------- will support multimodal in future
  return $ DSearch.SearchResp dSearchRes.searchRequest.id dSearchRes.searchRequestExpiry dSearchRes.shortestRouteInfo
  where
    -- TODO : remove this code after multiple search req issue get fixed from frontend
    --BEGIN
    checkValidSearchReq scheduleRideBufferTime sReq = do
      now <- getCurrentTime
      let isNonScheduled = diffUTCTime sReq.startTime sReq.createdAt < scheduleRideBufferTime
          isValid = sReq.validTill > now
      return $ isNonScheduled && isValid

handleBookingCancellation' ::
  ( DSearch.SearchRequestFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    Redis.HedisFlow m r,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text],
    MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Id Merchant.Merchant ->
  Id Person.Person ->
  Seconds ->
  Id SearchRequest.SearchRequest ->
  DSearch.SearchReq ->
  m ()
handleBookingCancellation' merchantId _personId stuckRideAutoCancellationBuffer sReqId req = do
  mbBooking <- QBooking.findByTransactionIdAndStatus sReqId.getId Booking.activeBookingStatus
  case mbBooking of
    Just booking -> do
      let reasonCode = SCR.CancellationReasonCode "multiple search request issue"
          reasonStage = SCR.OnSearch
      let cancelReq =
            DCancel.CancelReq
              { additionalInfo = Nothing,
                reallocate = Just False,
                blockOnCancellationRate = Nothing,
                ..
              }
      mRide <- QR.findActiveByRBId booking.id
      whenJust mRide $ \ride -> do
        isCancellingAllowed <- checkIfCancellingAllowed ride
        when (ride.status `elem` [DRide.NEW, DRide.UPCOMING] && isCancellingAllowed) $ do
          dCancelRes <- DCancel.cancel booking mRide cancelReq SBCR.ByUser
          void $ withShortRetry $ CallBPP.cancelV2 merchantId dCancelRes.bppUrl =<< ACL.buildCancelReqV2 dCancelRes cancelReq.reallocate
    _ -> pure ()
  where
    checkIfCancellingAllowed ride =
      case req of
        DSearch.OneWaySearch DSearch.OneWaySearchReq {verifyBeforeCancellingOldBooking} -> do
          let verifyBeforeCancelling = fromMaybe False verifyBeforeCancellingOldBooking -- defaulting to old behaviour when flag is not sent from frontend
          if verifyBeforeCancelling
            then do
              now <- getCurrentTime
              if addUTCTime (fromIntegral stuckRideAutoCancellationBuffer) ride.createdAt < now
                then return True
                else throwError (InvalidRequest "ACTIVE_BOOKING_PRESENT") -- 2 mins buffer
            else do
              return True -- this is the old behaviour, to cancel automatically
        _ -> do
          return True
