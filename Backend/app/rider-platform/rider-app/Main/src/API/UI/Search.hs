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
import qualified API.Types.UI.RiderLocation as RL
import qualified API.UI.CancelSearch as CancelSearch
import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.ACL.Search as TaxiACL
import qualified BecknV2.OnDemand.Enums
import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Dispatcher as Dispatcher
import qualified Domain.Action.UI.MultimodalConfirm as DMC
import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Client as DC
import qualified Domain.Types.EstimateStatus as Estimate
import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey as Journey
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RideStatus as DRide
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.Trip as DTrip
import Environment
import ExternalBPP.ExternalAPI.CallAPI as CallAPI
import ExternalBPP.ExternalAPI.Subway.CRIS.RouteFareV3 as RouteFareV3
import ExternalBPP.ExternalAPI.Subway.CRIS.SDKData
import Kernel.External.Encryption
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.External.Maps.Types
import qualified Kernel.External.Maps.Types as MapsTypes
import qualified Kernel.External.MultiModal.Interface as MInterface
import qualified Kernel.External.MultiModal.Interface as MultiModal
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
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
import qualified Tools.Metrics.BAPMetrics as Metrics
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
      :> Header "departure-time" UTCTime
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

allRoutesLoadedKey :: Text -> Text
allRoutesLoadedKey searchReqId = "allRoutesLoaded:" <> searchReqId

cacheAllRoutesLoadedKey :: Text -> Bool -> Flow ()
cacheAllRoutesLoadedKey searchReqId allRoutesLoaded = do
  let key = allRoutesLoadedKey searchReqId
  Redis.setExp key allRoutesLoaded 600

getAllRoutesLoadedKey :: Text -> Flow Bool
getAllRoutesLoadedKey searchReqId = do
  let key = allRoutesLoadedKey searchReqId
  Redis.safeGet key >>= \case
    Just allRoutesLoaded -> return allRoutesLoaded
    Nothing -> return False

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
                resp <- withTryCatch "cancelSearch:search" $ CancelSearch.cancelSearch' (personId, merchantId) estimate.id
                case resp of
                  Left _ -> void $ handleBookingCancellation merchantId personId stuckRideAutoCancellationBuffer sReq.id req
                  Right _ -> pure ()
              Nothing -> void $ handleBookingCancellation merchantId personId stuckRideAutoCancellationBuffer sReq.id req
          _ -> pure ()
  -- TODO : remove this code after multiple search req issue get fixed from frontend
  --END
  dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) False Nothing
  fork "search cabs" . withShortRetry $ do
    becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
    let generatedJson = encode becknTaxiReqV2
    logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
    void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 merchantId
  fork "Multimodal Search" $ do
    riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow dSearchRes.searchRequest.merchantOperatingCityId dSearchRes.searchRequest.configInExperimentVersions >>= fromMaybeM (RiderConfigNotFound dSearchRes.searchRequest.merchantOperatingCityId.getId)
    if riderConfig.makeMultiModalSearch
      then void (multiModalSearch dSearchRes.searchRequest riderConfig riderConfig.initiateFirstMultimodalJourney True req personId Nothing)
      else QSearchRequest.updateAllJourneysLoaded (Just True) dSearchRes.searchRequest.id
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

multimodalSearchHandler :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Bool -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe (Id DC.Client) -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe UTCTime -> FlowHandler MultimodalSearchResp
multimodalSearchHandler (personId, _merchantId) req mbInitiateJourney mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice mbIsDashboardRequest mbImeiNumber mbDepartureTime = withFlowHandlerAPI $
  withPersonIdLogTag personId $ do
    checkSearchRateLimit personId
    fork "updating person versions" $ updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice
    whenJust mbImeiNumber $ \imeiNumber -> do
      encryptedImeiNumber <- encrypt imeiNumber
      Person.updateImeiNumber (Just encryptedImeiNumber) personId
    dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) True Nothing
    riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow dSearchRes.searchRequest.merchantOperatingCityId dSearchRes.searchRequest.configInExperimentVersions >>= fromMaybeM (RiderConfigNotFound dSearchRes.searchRequest.merchantOperatingCityId.getId)
    let initiateJourney = fromMaybe False mbInitiateJourney
    JMU.measureLatency (multiModalSearch dSearchRes.searchRequest riderConfig initiateJourney False req personId mbDepartureTime) "multiModalSearch"

multiModalSearch :: SearchRequest.SearchRequest -> DRC.RiderConfig -> Bool -> Bool -> DSearch.SearchReq -> Id Person.Person -> Maybe UTCTime -> Flow MultimodalSearchResp
multiModalSearch searchRequest riderConfig initiateJourney forkInitiateFirstJourney req' personId mbDepartureTime = withLogTag ("multimodalSearch" <> searchRequest.id.getId) $ do
  now <- getCurrentTime
  userPreferences <- DMC.getMultimodalUserPreferences (Just searchRequest.riderId, searchRequest.merchantId)
  let req = DSearch.extractSearchDetails now req'
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  let vehicleCategory = fromMaybe BecknV2.OnDemand.Enums.BUS searchRequest.vehicleCategory
  let currentLocation = fmap latLongToLocationV2 req.currentLocation
  mbIntegratedBPPConfig <- SIBC.findMaybeIntegratedBPPConfig Nothing merchantOperatingCityId vehicleCategory (fromMaybe DIBC.MULTIMODAL req.platformType)
  let mode = castVehicleCategoryToGeneralVehicleType vehicleCategory
  let isSingleMode =
        case req' of
          DSearch.PTSearch _ -> True
          _ -> False
  routeLiveInfo <-
    case req' of
      DSearch.PTSearch ptSearchDetails -> do
        case (mbIntegratedBPPConfig, ptSearchDetails.vehicleNumber, ptSearchDetails.routeCode) of
          (Just integratedBPPConfig, Just userPassedVehicleNumber, Just userPassedRouteCode) -> do
            fork "getVehicleLiveRouteInfo" $ Metrics.incrementBusScanSearchRequestCount "ANNA_APP" merchantOperatingCityId.getId
            mbVehicleOverrideInfo <- Dispatcher.getFleetOverrideInfo userPassedVehicleNumber
            mbRouteLiveInfo <-
              case mbVehicleOverrideInfo of
                Just (updatedVehicleNumber, newDeviceWaybillNo) -> do
                  mbUpdatedVehicleRouteInfo <- JMU.getVehicleLiveRouteInfo [integratedBPPConfig] updatedVehicleNumber
                  if Just newDeviceWaybillNo /= ((.waybillId) . snd =<< mbUpdatedVehicleRouteInfo)
                    then do
                      Dispatcher.delFleetOverrideInfo userPassedVehicleNumber
                      JMU.getVehicleLiveRouteInfo [integratedBPPConfig] userPassedVehicleNumber
                    else pure mbUpdatedVehicleRouteInfo
                Nothing -> JMU.getVehicleLiveRouteInfo [integratedBPPConfig] userPassedVehicleNumber
            return $
              maybe
                Nothing
                ( \routeLiveInfo@(JMU.VehicleLiveRouteInfo {..}) ->
                    if routeCode == Just userPassedRouteCode
                      then Just routeLiveInfo
                      else Just (JMU.VehicleLiveRouteInfo {routeCode = Just userPassedRouteCode, ..})
                )
                (snd <$> mbRouteLiveInfo)
          _ -> return Nothing
      _ -> return Nothing
  let result
        | vehicleCategory == BecknV2.OnDemand.Enums.BUS && riderConfig.busScanRouteCalculationEnabledModes == Just True && isJust searchRequest.routeCode && isJust searchRequest.originStopCode && isJust searchRequest.destinationStopCode && isJust routeLiveInfo = do
          JMU.measureLatency (JMU.buildOneWayBusScanRouteDetails (fromJust searchRequest.routeCode) (fromJust searchRequest.originStopCode) (fromJust searchRequest.destinationStopCode) mbIntegratedBPPConfig >>= (\x -> return (x, []))) "buildOneWayBusScanRouteDetails" -- TODO: make this syntax better in coming future if you get time and 🔥 CAUTION 🔥 never let this fromJust be there without its prechecked condition in any case.
        | mode `elem` (fromMaybe [] riderConfig.domainRouteCalculationEnabledModes) = do
          case vehicleCategory of
            BecknV2.OnDemand.Enums.BUS -> JMU.measureLatency (JMU.buildSingleModeDirectRoutes (getPreliminaryLeg now currentLocation searchRequest.fromLocation.address.area) searchRequest.routeCode searchRequest.originStopCode searchRequest.destinationStopCode mbIntegratedBPPConfig searchRequest.merchantId searchRequest.merchantOperatingCityId vehicleCategory mode >>= (\x -> return (x, []))) "buildSingleModeDirectRoutes"
            BecknV2.OnDemand.Enums.METRO -> JMU.measureLatency (JMU.buildSingleModeDirectRoutes (getPreliminaryLeg now currentLocation searchRequest.fromLocation.address.area) searchRequest.routeCode searchRequest.originStopCode searchRequest.destinationStopCode mbIntegratedBPPConfig searchRequest.merchantId searchRequest.merchantOperatingCityId vehicleCategory mode >>= (\x -> return (x, []))) "buildSingleModeDirectRoutes"
            BecknV2.OnDemand.Enums.SUBWAY -> JMU.measureLatency (JMU.buildTrainAllViaRoutes (getPreliminaryLeg now currentLocation searchRequest.fromLocation.address.area) searchRequest.originStopCode searchRequest.destinationStopCode mbIntegratedBPPConfig searchRequest.merchantId searchRequest.merchantOperatingCityId vehicleCategory mode False personId searchRequest.id.getId) "buildTrainAllViaRoutes"
            _ -> return ([], [])
        | otherwise = return ([], [])
  (directSingleModeRoutes, restOfViaPoints) <- result
  (singleModeWarningType, otpResponse) <- do
    if not (null directSingleModeRoutes)
      then do
        return $
          ( Nothing,
            MInterface.MultiModalResponse
              { routes = directSingleModeRoutes
              }
          )
      else do
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
                  departureTime = mbDepartureTime,
                  mode = Nothing,
                  transitPreferences = Nothing,
                  transportModes = Nothing,
                  minimumWalkDistance = riderConfig.minimumWalkDistance,
                  permissibleModes = fromMaybe [] riderConfig.permissibleModes,
                  maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs,
                  sortingType = JMU.convertSortingType sortingType
                }
        transitServiceReq <- TMultiModal.getTransitServiceReq searchRequest.merchantId merchantOperatingCityId
        otpResponse' <- JMU.measureLatency (MultiModal.getTransitRoutes (Just searchRequest.id.getId) transitServiceReq transitRoutesReq >>= fromMaybeM (InternalError "routes dont exist")) "getTransitRoutes"
        let otpResponse'' = MInterface.MultiModalResponse (map mkRouteDetailsForWalkLegs otpResponse'.routes)
        logDebug $ "[Multimodal - OTP Response]" <> show otpResponse'' <> show searchRequest.id.getId
        -- Add default auto leg if no routes are found
        if null otpResponse''.routes
          then do
            case searchRequest.toLocation of
              Just toLocation -> do
                let toLocationV2 = LocationV2 {latLng = LatLngV2 {latitude = toLocation.lat, longitude = toLocation.lon}}
                let distance = fromMaybe (Distance 0 Meter) searchRequest.distance
                let duration = fromMaybe (Seconds 0) searchRequest.estimatedRideDuration
                (autoLeg, startTime, endTime) <- mkAutoOrWalkLeg now Nothing toLocationV2 MultiModalTypes.Unspecified distance duration searchRequest.fromLocation.address.area (searchRequest.toLocation >>= ((.area) . (.address)))
                let autoMultiModalResponse = mkMultimodalResponse autoLeg startTime endTime
                return (Just NoPublicTransportRoutes, autoMultiModalResponse)
              Nothing -> return (Nothing, otpResponse'')
          else do
            (finalRoutes, warningType) <- case req' of
              DSearch.PTSearch _ -> do
                let mbBestOneWayRoute = JMU.getBestOneWayRoute (castVehicleCategoryToGeneralVehicleType vehicleCategory) otpResponse''.routes searchRequest.originStopCode searchRequest.destinationStopCode
                case mbBestOneWayRoute of
                  Just bestOneWayRoute -> do
                    mbPreliminaryLeg <-
                      if ((listToMaybe bestOneWayRoute.legs) <&> (.mode)) == Just MultiModalTypes.Walk
                        then return Nothing
                        else join <$> mapM (getPreliminaryLeg now currentLocation searchRequest.fromLocation.address.area ((listToMaybe bestOneWayRoute.legs) >>= (.toStopDetails) >>= (.name))) ((listToMaybe bestOneWayRoute.legs) <&> (.startLocation))
                    let updatedBestOneWayRoute =
                          case mbPreliminaryLeg of
                            Just leg -> bestOneWayRoute {MultiModalTypes.legs = [leg] ++ bestOneWayRoute.legs}
                            Nothing -> bestOneWayRoute
                    return ([updatedBestOneWayRoute], Nothing)
                  Nothing -> return (otpResponse''.routes, Just NoSingleModeRoutes)
              _ -> return (otpResponse''.routes, Nothing)
            logDebug $ "finalRoutes: " <> show finalRoutes
            filteredRoutes <- JM.filterTransitRoutes riderConfig finalRoutes
            return (warningType, MInterface.MultiModalResponse {routes = filteredRoutes})
  when (not (null restOfViaPoints) && isJust mbIntegratedBPPConfig) $ do
    fork "Process rest of single mode routes" $ processSingleModeRoutes isSingleMode restOfViaPoints userPreferences mbIntegratedBPPConfig (getPreliminaryLeg now currentLocation searchRequest.fromLocation.address.area) routeLiveInfo

  let (indexedRoutesToProcess, showMultimodalWarningForFirstJourney) = getIndexedRoutesAndWarning userPreferences otpResponse

  mbCrisSdkToken <- JMU.measureLatency (getCrisSdkToken merchantOperatingCityId indexedRoutesToProcess) "getCrisSdkToken"

  JMU.measureLatency (multimodalIntiateHelper isSingleMode singleModeWarningType otpResponse userPreferences indexedRoutesToProcess showMultimodalWarningForFirstJourney mbCrisSdkToken True (null restOfViaPoints) routeLiveInfo) "multimodalIntiateHelper"
  where
    processSingleModeRoutes isSingleMode restOfViaPoints userPreferences mbIntegratedBPPConfig preliminaryLeg routeLiveInfo = do
      (restOfRoutes, _) <- JMU.measureLatency (JMU.getSubwayValidRoutes restOfViaPoints preliminaryLeg (fromJust mbIntegratedBPPConfig) searchRequest.merchantId searchRequest.merchantOperatingCityId (fromMaybe BecknV2.OnDemand.Enums.BUS searchRequest.vehicleCategory) (castVehicleCategoryToGeneralVehicleType (fromMaybe BecknV2.OnDemand.Enums.BUS searchRequest.vehicleCategory)) True) "getSubwayValidRoutes"
      if null restOfRoutes
        then
          getAllRoutesLoadedKey searchRequest.id.getId >>= \case
            True -> QSearchRequest.updateAllJourneysLoaded (Just True) searchRequest.id
            False -> cacheAllRoutesLoadedKey searchRequest.id.getId True
        else do
          let multimodalResponse = MInterface.MultiModalResponse {routes = restOfRoutes}
              (indexedRoutesToProcess, showMultimodalWarningForFirstJourney) = getIndexedRoutesAndWarning userPreferences multimodalResponse
          void $ multimodalIntiateHelper isSingleMode Nothing multimodalResponse userPreferences indexedRoutesToProcess showMultimodalWarningForFirstJourney Nothing False True routeLiveInfo

    multimodalIntiateHelper isSingleMode singleModeWarningType otpResponse userPreferences indexedRoutesToProcess showMultimodalWarningForFirstJourney mbCrisSdkToken isFirstJourneyReq allJourneysLoaded routeLiveInfo = do
      mbJourneyWithIndex <- JMU.measureLatency (go isSingleMode indexedRoutesToProcess userPreferences routeLiveInfo searchRequest.busLocationData) "Multimodal Init Time" -- process until first journey is found
      QSearchRequest.updateHasMultimodalSearch (Just True) searchRequest.id

      journeys <- JMU.measureLatency (if isFirstJourneyReq then DQuote.getJourneys searchRequest (Just True) else return Nothing) "getJourneys Multimodal Init time"
      let mbFirstJourney = listToMaybe (fromMaybe [] journeys)
      firstJourneyInfo <-
        if initiateJourney && isFirstJourneyReq
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
                      res <- JMU.measureLatency (DMC.postMultimodalInitiate (Just searchRequest.riderId, searchRequest.merchantId) firstJourney.id) "DMC.postMultimodalInitiate"
                      return $ Just res
                fork "Rest of the routes Init" $ processRestOfRoutes [x | (j, x) <- zip [0 ..] otpResponse.routes, j /= idx] userPreferences routeLiveInfo allJourneysLoaded searchRequest.busLocationData
                return resp
              Nothing -> do
                QSearchRequest.updateAllJourneysLoaded (Just True) searchRequest.id
                return Nothing
          else do
            case mbJourneyWithIndex of
              Just (idx, _) -> do
                fork "Process all routes " $ processRestOfRoutes [x | (j, x) <- indexedRoutesToProcess, j /= idx] userPreferences routeLiveInfo allJourneysLoaded searchRequest.busLocationData
              Nothing -> do
                fork "Process all routes " $ processRestOfRoutes (map snd indexedRoutesToProcess) userPreferences routeLiveInfo allJourneysLoaded searchRequest.busLocationData
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
    go :: Bool -> [(Int, MultiModalTypes.MultiModalRoute)] -> ApiTypes.MultimodalUserPreferences -> Maybe JMU.VehicleLiveRouteInfo -> [RL.BusLocation] -> Flow (Maybe (Int, Journey.Journey))
    go _ [] _ _ _ = return Nothing
    go isSingleMode ((idx, r) : routes) userPreferences routeLiveInfo busLocationData = do
      mbResult <- processRoute isSingleMode r userPreferences routeLiveInfo busLocationData
      case mbResult of
        Nothing -> go isSingleMode routes userPreferences routeLiveInfo busLocationData
        Just journey -> return $ Just (idx, journey)

    processRoute :: Bool -> MultiModalTypes.MultiModalRoute -> ApiTypes.MultimodalUserPreferences -> Maybe JMU.VehicleLiveRouteInfo -> [RL.BusLocation] -> Flow (Maybe Journey.Journey)
    processRoute isSingleMode r userPreferences routeLiveInfo busLocationData = do
      updatedRoute <- updateRouteWithLegDurations r
      let initReq =
            JMTypes.JourneyInitData
              { parentSearchId = searchRequest.id,
                merchantId = searchRequest.merchantId,
                merchantOperatingCityId = searchRequest.merchantOperatingCityId,
                personId = searchRequest.riderId,
                legs = updatedRoute.legs,
                routeLiveInfo = routeLiveInfo,
                estimatedDistance = updatedRoute.distance,
                estimatedDuration = updatedRoute.duration,
                startTime = updatedRoute.startTime,
                endTime = updatedRoute.endTime,
                isSingleMode,
                maximumWalkDistance = riderConfig.maximumWalkDistance,
                relevanceScore = updatedRoute.relevanceScore,
                busLocationData
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

    straightLineDistance leg = highPrecMetersToMeters $ distanceBetweenInMeters (LatLong leg.startLocation.latLng.latitude leg.startLocation.latLng.longitude) (LatLong leg.endLocation.latLng.latitude leg.endLocation.latLng.longitude)

    -- Calculate proportional duration only for Walk and Unspecified legs
    calculateLegProportionalDuration :: MultiModalTypes.MultiModalLeg -> Flow MultiModalTypes.MultiModalLeg
    calculateLegProportionalDuration leg = do
      let totalEstimatedDuration = fromMaybe (Seconds 0) searchRequest.estimatedRideDuration
          totalEstimatedDistance = fromMaybe (Distance 0 Meter) searchRequest.distance
      case leg.mode of
        MultiModalTypes.Walk ->
          if straightLineDistance leg > riderConfig.maximumWalkDistance
            then do
              -- Call OSRM for taxi/auto (car) distance/duration, fallback to proportional duration if it fails
              res <-
                withTryCatch "getMultimodalWalkDistance:calculateLegProportionalDuration" $
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
            else return leg{duration = JM.calculateWalkDuration leg.distance}
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

    processRestOfRoutes :: [MultiModalTypes.MultiModalRoute] -> ApiTypes.MultimodalUserPreferences -> Maybe JMU.VehicleLiveRouteInfo -> Bool -> [RL.BusLocation] -> Flow ()
    processRestOfRoutes routes userPreferences routeLiveInfo allJourneysLoaded busLocationData = do
      forM_ routes $ \route' -> processRoute False route' userPreferences routeLiveInfo busLocationData
      allRoutesLoaded <- getAllRoutesLoadedKey searchRequest.id.getId
      when (allJourneysLoaded || allRoutesLoaded) $ QSearchRequest.updateAllJourneysLoaded (Just True) searchRequest.id
      cacheAllRoutesLoadedKey searchRequest.id.getId True

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

    mkAutoOrWalkLeg :: UTCTime -> Maybe LocationV2 -> LocationV2 -> MultiModalTypes.GeneralVehicleType -> Distance -> Seconds -> Maybe Text -> Maybe Text -> Flow (MultiModalTypes.MultiModalLeg, UTCTime, UTCTime)
    mkAutoOrWalkLeg now fromLocation toLocation mode distance duration fromStopName toStopName = do
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
              fromStopDetails =
                Just
                  MultiModalTypes.MultiModalStopDetails
                    { stopCode = Nothing,
                      platformCode = Nothing,
                      name = fromStopName,
                      gtfsId = Nothing
                    },
              toStopDetails =
                Just
                  MultiModalTypes.MultiModalStopDetails
                    { stopCode = Nothing,
                      platformCode = Nothing,
                      name = toStopName,
                      gtfsId = Nothing
                    },
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

    getCrisSdkToken :: Id MerchantOperatingCity -> [(Int, MultiModalTypes.MultiModalRoute)] -> Flow (Maybe Text)
    getCrisSdkToken merchantOperatingCityId indexedRoutes = do
      let subwayRoutes = filter (\(_, multiModalRoute) -> any (\leg -> leg.mode == MultiModalTypes.Subway) multiModalRoute.legs) indexedRoutes
      if null subwayRoutes
        then return Nothing
        else do
          SIBC.findMaybeIntegratedBPPConfig Nothing merchantOperatingCityId BecknV2.OnDemand.Enums.SUBWAY DIBC.MULTIMODAL >>= \case
            Just integratedBPPConfig -> do
              case integratedBPPConfig.providerConfig of
                DIBC.CRIS config -> do
                  if (config.useRouteFareV4 == Just True)
                    then do
                      mbGetSdkDataReq <- mkGetSDKDataReq personId
                      case mbGetSdkDataReq of
                        Just getSDKDataReq -> do
                          getSdkDataResp <- getSDKData config getSDKDataReq
                          return $ Just getSdkDataResp.sdkData
                        Nothing -> return Nothing
                    else findValidSdkToken integratedBPPConfig merchantOperatingCityId subwayRoutes
                _ -> return Nothing
            Nothing -> return Nothing

    findValidSdkToken :: DIBC.IntegratedBPPConfig -> Id MerchantOperatingCity -> [(Int, MultiModalTypes.MultiModalRoute)] -> Flow (Maybe Text)
    findValidSdkToken _ _ [] = return Nothing
    findValidSdkToken integratedBPPConfig mocId ((_, multiModalRoute) : restRoutes) = do
      let subwayLegs = filter (\leg -> leg.mode == MultiModalTypes.Subway) multiModalRoute.legs
      mbSdkToken <- findValidSdkTokenFromLegs integratedBPPConfig mocId subwayLegs
      case mbSdkToken of
        Just token -> return $ Just token
        Nothing -> findValidSdkToken integratedBPPConfig mocId restRoutes

    findValidSdkTokenFromLegs :: DIBC.IntegratedBPPConfig -> Id MerchantOperatingCity -> [MultiModalTypes.MultiModalLeg] -> Flow (Maybe Text)
    findValidSdkTokenFromLegs _ _ [] = return Nothing
    findValidSdkTokenFromLegs integratedBPPConfig mocId (leg : restLegs) = do
      mbSdkToken <- tryGetSdkTokenFromLeg integratedBPPConfig mocId leg
      case mbSdkToken of
        Just token -> return $ Just token
        Nothing -> findValidSdkTokenFromLegs integratedBPPConfig mocId restLegs

    tryGetSdkTokenFromLeg :: DIBC.IntegratedBPPConfig -> Id MerchantOperatingCity -> MultiModalTypes.MultiModalLeg -> Flow (Maybe Text)
    tryGetSdkTokenFromLeg integratedBPPConfig mocId leg = do
      let mbRouteCode = listToMaybe leg.routeDetails >>= (.gtfsId) <&> gtfsIdtoDomainCode
          mbFromStopCode = (leg.fromStopDetails >>= (.stopCode)) <|> ((leg.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)
          mbToStopCode = (leg.toStopDetails >>= (.stopCode)) <|> ((leg.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)
      case (mbFromStopCode, mbToStopCode, mbRouteCode) of
        (Just fromCode, Just toCode, Just routeCode) -> do
          case integratedBPPConfig.providerConfig of
            DIBC.CRIS config' -> do
              (viaPoints, changeOver) <- CallAPI.getChangeOverAndViaPoints [CallAPI.BasicRouteDetail {routeCode = routeCode, startStopCode = fromCode, endStopCode = toCode}] integratedBPPConfig
              routeFareReq <- JMU.getRouteFareRequest fromCode toCode changeOver viaPoints personId False
              (_, sdkToken) <- RouteFareV3.getRouteFare config' mocId routeFareReq False True
              return $ sdkToken
            _ -> return Nothing
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

    getPreliminaryLeg :: UTCTime -> Maybe LocationV2 -> Maybe Text -> Maybe Text -> LocationV2 -> Flow (Maybe MultiModalTypes.MultiModalLeg)
    getPreliminaryLeg now mbCurrentLocation fromStopName toStopName fromStopLocation = do
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
                  (leg, _, _) <- mkAutoOrWalkLeg now (Just currentLocation) fromStopLocation MultiModalTypes.Walk (convertMetersToDistance Meter distance) duration fromStopName toStopName
                  return $ Just leg
            Nothing -> return Nothing

    getDistanceAndDuration :: Maps.LatLong -> Maps.LatLong -> Maps.TravelMode -> Flow (Maybe (Maps.GetDistanceResp Maps.LatLong Maps.LatLong))
    getDistanceAndDuration fromLocation toLocation travelMode = do
      resp <-
        withTryCatch "getMultimodalWalkDistance:getDistanceAndDuration" $
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

    getIndexedRoutesAndWarning :: ApiTypes.MultimodalUserPreferences -> MultiModalTypes.MultiModalResponse -> ([(Int, MultiModalTypes.MultiModalRoute)], Bool)
    getIndexedRoutesAndWarning userPreferences otpResponse = do
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
      (indexedRoutesToProcess, showMultimodalWarningForFirstJourney)

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
                resp <- withTryCatch "cancelSearch:searchTrigger" $ JLT.cancelSearch' (personId, merchantId) estimate.id
                case resp of
                  Left _ -> void $ handleBookingCancellation' merchantId personId stuckRideAutoCancellationBuffer sReq.id req
                  Right _ -> pure ()
              Nothing -> void $ handleBookingCancellation' merchantId personId stuckRideAutoCancellationBuffer sReq.id req
          _ -> pure ()
  -- TODO : remove this code after multiple search req issue get fixed from frontend
  --END
  dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) False Nothing
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
