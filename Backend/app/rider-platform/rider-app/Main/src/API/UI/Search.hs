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
  )
where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified API.UI.Select as Select
import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.ACL.Search as TaxiACL
import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.MultimodalConfirm as DMC
import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Client as DC
import qualified Domain.Types.Estimate as Estimate
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey as Journey
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.StationType as Station
import qualified Domain.Types.Trip as DTrip
import Environment
import ExternalBPP.ExternalAPI.CallAPI as CallAPI
import ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare as CRISRouteFare
import Kernel.External.Encryption
import Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.MultiModal.Interface as MInterface
import qualified Kernel.External.MultiModal.Interface as MultiModal
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import Kernel.Randomizer
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Version
import qualified Lib.JourneyModule.Base as JM
import qualified Lib.JourneyModule.Types as JMTypes
import qualified Lib.JourneyModule.Utils as JMU
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.Search as DSearch
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.IntegratedBPPConfig as QIntegratedBPPConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Auth
import Tools.Error
import qualified Tools.MultiModal as TMultiModal

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
  when merchant.enableForMultipleSearchIssue $ do
    mbSReq <- QSearchRequest.findLastSearchRequestInKV personId
    shouldCancelPrevSearch <- maybe (return False) (checkValidSearchReq merchant.scheduleRideBufferTime) mbSReq
    when shouldCancelPrevSearch $ do
      fork "handle multiple search request issue" $ do
        case mbSReq of
          Just sReq -> do
            mbEstimate <- QEstimate.findBySRIdAndStatusesInKV sReq.id [Estimate.DRIVER_QUOTE_REQUESTED, Estimate.GOT_DRIVER_QUOTE]
            case mbEstimate of
              Just estimate -> do
                resp <- try @_ @SomeException $ Select.cancelSearch' (personId, merchantId) estimate.id
                case resp of
                  Left _ -> void $ handleBookingCancellation merchantId personId sReq.id
                  Right _ -> pure ()
              Nothing -> void $ handleBookingCancellation merchantId personId sReq.id
          _ -> pure ()
  -- TODO : remove this code after multiple search req issue get fixed from frontend
  --END
  dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) Nothing False
  fork "search cabs" . withShortRetry $ do
    becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
    let generatedJson = encode becknTaxiReqV2
    logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
    void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 merchantId
  fork "Multimodal Search" $ do
    riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow dSearchRes.searchRequest.merchantOperatingCityId dSearchRes.searchRequest.configInExperimentVersions >>= fromMaybeM (RiderConfigNotFound dSearchRes.searchRequest.merchantOperatingCityId.getId)
    when riderConfig.makeMultiModalSearch $ do
      void (multiModalSearch dSearchRes.searchRequest riderConfig False req personId)
  return $ DSearch.SearchResp dSearchRes.searchRequest.id dSearchRes.searchRequestExpiry dSearchRes.shortestRouteInfo
  where
    -- TODO : remove this code after multiple search req issue get fixed from frontend
    --BEGIN
    checkValidSearchReq scheduleRideBufferTime sReq = do
      now <- getCurrentTime
      let isNonScheduled = diffUTCTime sReq.startTime sReq.createdAt < scheduleRideBufferTime
          isValid = sReq.validTill > now
      return $ isNonScheduled && isValid

handleBookingCancellation :: Id Merchant.Merchant -> Id Person.Person -> Id SearchRequest.SearchRequest -> Flow ()
handleBookingCancellation merchantId _personId sReqId = do
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
      dCancelRes <- DCancel.cancel booking mRide cancelReq SBCR.ByUser
      void $ withShortRetry $ CallBPP.cancelV2 merchantId dCancelRes.bppUrl =<< ACL.buildCancelReqV2 dCancelRes cancelReq.reallocate
    _ -> pure ()

multimodalSearchHandler :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Bool -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe (Id DC.Client) -> Maybe Text -> Maybe Bool -> Maybe Text -> FlowHandler MultimodalSearchResp
multimodalSearchHandler (personId, _merchantId) req mbInitateJourney mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice mbIsDashboardRequest mbImeiNumber = withFlowHandlerAPI $
  withPersonIdLogTag personId $ do
    checkSearchRateLimit personId
    fork "updating person versions" $ updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice
    whenJust mbImeiNumber $ \imeiNumber -> do
      encryptedImeiNumber <- encrypt imeiNumber
      Person.updateImeiNumber (Just encryptedImeiNumber) personId
    dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) Nothing True
    riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow dSearchRes.searchRequest.merchantOperatingCityId dSearchRes.searchRequest.configInExperimentVersions >>= fromMaybeM (RiderConfigNotFound dSearchRes.searchRequest.merchantOperatingCityId.getId)
    let initateJourney = fromMaybe False mbInitateJourney
    multiModalSearch dSearchRes.searchRequest riderConfig initateJourney req personId

multiModalSearch :: SearchRequest.SearchRequest -> DRC.RiderConfig -> Bool -> DSearch.SearchReq -> Id Person.Person -> Flow MultimodalSearchResp
multiModalSearch searchRequest riderConfig initateJourney req' personId = do
  now <- getCurrentTime
  userPreferences <- DMC.getMultimodalUserPreferences (Just searchRequest.riderId, searchRequest.merchantId)
  let req = DSearch.extractSearchDetails now req'
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  let vehicleCategory = fromMaybe BecknV2.OnDemand.Enums.BUS searchRequest.vehicleCategory
  integratedBPPConfig <- QIntegratedBPPConfig.findByDomainAndCityAndVehicleCategory "FRFS" merchantOperatingCityId vehicleCategory (fromMaybe DIBC.MULTIMODAL req.platformType) >>= fromMaybeM (InternalError "No integrated bpp config found")
  mbSingleModeRouteDetails <- JMU.measureLatency (JMU.getSingleModeRouteDetails searchRequest.routeCode searchRequest.originStopCode searchRequest.destinationStopCode integratedBPPConfig searchRequest.merchantId searchRequest.merchantOperatingCityId vehicleCategory) "getSingleModeRouteDetails"
  (singleModeWarningType, otpResponse) <- case mbSingleModeRouteDetails of
    Just singleModeRouteDetails -> do
      let fromStopDetails =
            MultiModalTypes.MultiModalStopDetails
              { stopCode = Just singleModeRouteDetails.fromStop.stopCode,
                platformCode = Nothing,
                name = Just singleModeRouteDetails.fromStop.stopName,
                gtfsId = Just singleModeRouteDetails.fromStop.stopCode
              }
      let toStopDetails =
            MultiModalTypes.MultiModalStopDetails
              { stopCode = Just singleModeRouteDetails.toStop.stopCode,
                platformCode = Nothing,
                name = Just singleModeRouteDetails.toStop.stopName,
                gtfsId = Just singleModeRouteDetails.toStop.stopCode
              }
      let fromStopLocation = LocationV2 {latLng = LatLngV2 {latitude = singleModeRouteDetails.fromStop.stopLat, longitude = singleModeRouteDetails.fromStop.stopLon}}
      let toStopLocation = LocationV2 {latLng = LatLngV2 {latitude = singleModeRouteDetails.toStop.stopLat, longitude = singleModeRouteDetails.toStop.stopLon}}
      let distance = fromMaybe (Distance 0 Meter) searchRequest.distance
      let duration = nominalDiffTimeToSeconds $ diffUTCTime singleModeRouteDetails.toStop.stopArrivalTime singleModeRouteDetails.fromStop.stopArrivalTime
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
                        subLegOrder = 0,
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
                exit = Nothing,
                steps = Nothing
              }
      return $
        ( Nothing,
          MInterface.MultiModalResponse
            { routes =
                [ MultiModalTypes.MultiModalRoute
                    { distance = distance,
                      duration = duration,
                      startTime = Just singleModeRouteDetails.fromStop.stopArrivalTime,
                      endTime = Just singleModeRouteDetails.toStop.stopArrivalTime,
                      legs = [leg],
                      relevanceScore = Nothing
                    }
                ]
            }
        )
    _ -> do
      let permissibleModesToUse = fromMaybe [] riderConfig.permissibleModes
      let sortingType = fromMaybe DMP.FASTEST userPreferences.journeyOptionsSortingType
      destination <- extractDest searchRequest.toLocation
      let transitRoutesReq =
            GetTransitRoutesReq
              { origin = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = searchRequest.fromLocation.lat, longitude = searchRequest.fromLocation.lon}}},
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
      otpResponse'' <- MInterface.MultiModalResponse <$> JM.filterTransitRoutes otpResponse'.routes merchantOperatingCityId
      logDebug $ "[Multimodal - OTP Response]" <> show otpResponse''
      -- Add default auto leg if no routes are found
      if null otpResponse''.routes
        then do
          case searchRequest.toLocation of
            Just toLocation -> do
              autoLeg <- mkAutoLeg now toLocation
              return (Just NoPublicTransportRoutes, autoLeg)
            Nothing -> return (Nothing, otpResponse'')
        else do
          case req' of
            DSearch.PTSearch _ -> do
              let onlySingleModeRoutes = filter (\r -> (all (eitherWalkOrSingleMode vehicleCategory) r.legs) && (any (onlySingleMode vehicleCategory) r.legs)) otpResponse''.routes
              let filterFirstAndLastMileWalks = map filterWalkLegs onlySingleModeRoutes
              let warningType = if null onlySingleModeRoutes then Just NoSingleModeRoutes else Nothing
              return (warningType, MInterface.MultiModalResponse {routes = if null onlySingleModeRoutes then otpResponse''.routes else filterFirstAndLastMileWalks})
            _ -> return (Nothing, otpResponse'')

  let userPreferredTransitModes = userPreferencesToGeneralVehicleTypes userPreferences.allowedTransitModes
      hasOnlyUserPreferredTransitModes otpRoute = all (isLegModeIn userPreferredTransitModes) otpRoute.legs
      indexedRoutes = zip [0 ..] otpResponse.routes
      filteredIndexedRoutes = filter (hasOnlyUserPreferredTransitModes . snd) indexedRoutes
      indexedRoutesToProcess = if null filteredIndexedRoutes then indexedRoutes else filteredIndexedRoutes
      showMultimodalWarningForFirstJourney = null filteredIndexedRoutes

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
    if initateJourney
      then do
        case mbJourneyWithIndex of
          Just (idx, firstJourney) -> do
            resp <- DMC.postMultimodalInitiate (Just searchRequest.riderId, searchRequest.merchantId) firstJourney.id
            fork "Rest of the routes Init" $ processRestOfRoutes [x | (j, x) <- zip [0 ..] otpResponse.routes, j /= idx] userPreferences
            return $ Just resp{crisSdkToken = mbCrisSdkToken}
          Nothing -> return Nothing
      else return Nothing

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
      let initReq =
            JMTypes.JourneyInitData
              { parentSearchId = searchRequest.id,
                merchantId = searchRequest.merchantId,
                merchantOperatingCityId = searchRequest.merchantOperatingCityId,
                personId = searchRequest.riderId,
                legs = r.legs,
                estimatedDistance = r.distance,
                estimatedDuration = r.duration,
                startTime = r.startTime,
                endTime = r.endTime,
                maximumWalkDistance = riderConfig.maximumWalkDistance,
                straightLineThreshold = riderConfig.straightLineThreshold,
                relevanceScore = r.relevanceScore
              }
      JM.init initReq userPreferences

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

    processRestOfRoutes :: [MultiModalTypes.MultiModalRoute] -> ApiTypes.MultimodalUserPreferences -> Flow ()
    processRestOfRoutes routes userPreferences = do
      forM_ routes $ \route' -> processRoute route' userPreferences
      QSearchRequest.updateAllJourneysLoaded (Just True) searchRequest.id

    extractDest Nothing = throwError $ InvalidRequest "Destination is required for multimodal search"
    extractDest (Just d) = return d

    userPreferencesToGeneralVehicleTypes :: [DTrip.MultimodalTravelMode] -> [GeneralVehicleType]
    userPreferencesToGeneralVehicleTypes = mapMaybe allowedTransitModeToGeneralVehicleType

    allowedTransitModeToGeneralVehicleType :: DTrip.MultimodalTravelMode -> Maybe GeneralVehicleType
    allowedTransitModeToGeneralVehicleType mode = case mode of
      DTrip.Bus -> Just MultiModalTypes.Bus
      DTrip.Metro -> Just MultiModalTypes.MetroRail
      DTrip.Subway -> Just MultiModalTypes.Subway
      DTrip.Walk -> Just MultiModalTypes.Walk
      _ -> Nothing

    castVehicleCategoryToGeneralVehicleType :: BecknV2.OnDemand.Enums.VehicleCategory -> GeneralVehicleType
    castVehicleCategoryToGeneralVehicleType vehicleCategory = case vehicleCategory of
      BecknV2.OnDemand.Enums.BUS -> MultiModalTypes.Bus
      BecknV2.OnDemand.Enums.METRO -> MultiModalTypes.MetroRail
      BecknV2.OnDemand.Enums.SUBWAY -> MultiModalTypes.Subway
      _ -> MultiModalTypes.Unspecified

    mkAutoLeg now toLocation = do
      let fromStopLocation = LocationV2 {latLng = LatLngV2 {latitude = searchRequest.fromLocation.lat, longitude = searchRequest.fromLocation.lon}}
      let toStopLocation = LocationV2 {latLng = LatLngV2 {latitude = toLocation.lat, longitude = toLocation.lon}}
      let distance = fromMaybe (Distance 0 Meter) searchRequest.distance
      let duration = fromMaybe (Seconds 0) searchRequest.estimatedRideDuration
      let (_, startTime) = JMU.getISTTimeInfo now
      let endTime = addUTCTime (secondsToNominalDiffTime duration) startTime
      let leg =
            MultiModalTypes.MultiModalLeg
              { distance = distance,
                duration = duration,
                polyline = Polyline {encodedPolyline = ""},
                mode = MultiModalTypes.Unspecified,
                startLocation = fromStopLocation,
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
                        startLocation = fromStopLocation,
                        endLocation = toStopLocation,
                        subLegOrder = 0,
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
                exit = Nothing,
                steps = Nothing
              }
      return $
        MInterface.MultiModalResponse
          { routes =
              [ MultiModalTypes.MultiModalRoute
                  { distance = distance,
                    duration = duration,
                    startTime = Just startTime,
                    endTime = Just endTime,
                    legs = [leg],
                    relevanceScore = Nothing
                  }
              ]
          }

    isLegModeIn :: [GeneralVehicleType] -> MultiModalTypes.MultiModalLeg -> Bool
    isLegModeIn modes leg = leg.mode `elem` modes

    getCrisSdkToken :: Id MerchantOperatingCity -> Maybe [DQuote.JourneyData] -> Flow (Maybe Text)
    getCrisSdkToken _ Nothing = return Nothing
    getCrisSdkToken merchantOperatingCityId (Just journeys) = do
      person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      mbIntegratedBPPConfig <- QIntegratedBPPConfig.findByDomainAndCityAndVehicleCategory "FRFS" merchantOperatingCityId BecknV2.OnDemand.Enums.SUBWAY DIBC.MULTIMODAL

      case mbIntegratedBPPConfig of
        Just integratedBPPConfig -> do
          let subwayJourneys = filter (\j -> any (\leg -> leg.journeyMode == DTrip.Subway) j.journeyLegs) journeys
          if null subwayJourneys
            then return Nothing
            else do
              mbMobileNumber <- mapM decrypt person.mobileNumber
              mbImeiNumber <- mapM decrypt person.imeiNumber
              sessionId <- getRandomInRange (1, 1000000 :: Int)
              findValidSdkToken subwayJourneys integratedBPPConfig mbMobileNumber mbImeiNumber sessionId
        Nothing -> return Nothing
      where
        findValidSdkToken :: [DQuote.JourneyData] -> DIBC.IntegratedBPPConfig -> Maybe Text -> Maybe Text -> Int -> Flow (Maybe Text)
        findValidSdkToken [] _ _ _ _ = return Nothing
        findValidSdkToken (journey : restJourneys) integratedBPPConfig mbMobileNumber mbImeiNumber sessionId = do
          let subwayLegs = filter (\leg -> leg.journeyMode == DTrip.Subway) journey.journeyLegs
          mbSdkToken <- findValidSdkTokenFromLegs subwayLegs integratedBPPConfig mbMobileNumber mbImeiNumber sessionId
          case mbSdkToken of
            Just token -> return $ Just token
            Nothing -> findValidSdkToken restJourneys integratedBPPConfig mbMobileNumber mbImeiNumber sessionId

        findValidSdkTokenFromLegs :: [DQuote.JourneyLeg] -> DIBC.IntegratedBPPConfig -> Maybe Text -> Maybe Text -> Int -> Flow (Maybe Text)
        findValidSdkTokenFromLegs [] _ _ _ _ = return Nothing
        findValidSdkTokenFromLegs (leg : restLegs) integratedBPPConfig mbMobileNumber mbImeiNumber sessionId = do
          mbSdkToken <- tryGetSdkTokenFromLeg leg integratedBPPConfig mbMobileNumber mbImeiNumber sessionId
          case mbSdkToken of
            Just token -> return $ Just token
            Nothing -> findValidSdkTokenFromLegs restLegs integratedBPPConfig mbMobileNumber mbImeiNumber sessionId

        tryGetSdkTokenFromLeg :: DQuote.JourneyLeg -> DIBC.IntegratedBPPConfig -> Maybe Text -> Maybe Text -> Int -> Flow (Maybe Text)
        tryGetSdkTokenFromLeg leg integratedBPPConfig mbMobileNumber mbImeiNumber sessionId = do
          let mbRouteCode = listToMaybe leg.routeDetails >>= (.routeCode)
          case (leg.fromStationCode, leg.toStationCode, mbRouteCode) of
            (Just fromCode, Just toCode, Just routeCode) -> do
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
            _ -> return Nothing

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
