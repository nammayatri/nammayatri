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
import Control.Monad.Extra (maybeM)
import Data.Aeson
import Data.List (partition, sortBy, sortOn)
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
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.Trip as DTrip
import Environment
import Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.MultiModal.Interface as MInterface
import qualified Kernel.External.MultiModal.Interface as MultiModal
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
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
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.Search as DSearch
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.IntegratedBPPConfig as QIntegratedBPPConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMultiModal
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Auth
import Tools.Error
import qualified Tools.MultiModal as TMultiModal

-------- Search Flow --------

data MultimodalSearchResp = MultimodalSearchResp
  { searchId :: Id SearchRequest.SearchRequest,
    searchExpiry :: UTCTime,
    journeys :: [DQuote.JourneyData],
    firstJourney :: Maybe DQuote.JourneyData,
    firstJourneyInfo :: Maybe ApiTypes.JourneyInfoResp
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
  fork "Multimodal Search" $ void (multiModalSearch dSearchRes.searchRequest False req)
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

multimodalSearchHandler :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Bool -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe (Id DC.Client) -> Maybe Text -> Maybe Bool -> FlowHandler MultimodalSearchResp
multimodalSearchHandler (personId, _merchantId) req mbInitateJourney mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice mbIsDashboardRequest = withFlowHandlerAPI $
  withPersonIdLogTag personId $ do
    checkSearchRateLimit personId
    fork "updating person versions" $ updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice
    dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) Nothing True
    let initateJourney = fromMaybe False mbInitateJourney
    multiModalSearch dSearchRes.searchRequest initateJourney req

multiModalSearch :: SearchRequest.SearchRequest -> Bool -> DSearch.SearchReq -> Flow MultimodalSearchResp
multiModalSearch searchRequest initateJourney req' = do
  now <- getCurrentTime
  let req = DSearch.extractSearchDetails now req'
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow merchantOperatingCityId searchRequest.configInExperimentVersions >>= fromMaybeM (RiderConfigNotFound merchantOperatingCityId.getId)
  userPreferences <- DMC.getMultimodalUserPreferences (Just searchRequest.riderId, searchRequest.merchantId)
  let permissibleModesToUse =
        if null userPreferences.allowedTransitModes
          then fromMaybe [] riderConfig.permissibleModes
          else userPreferencesToGeneralVehicleTypes userPreferences.allowedTransitModes
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
            sortingType = convertSortingType sortingType
          }
  transitServiceReq <- TMultiModal.getTransitServiceReq searchRequest.merchantId merchantOperatingCityId
  otpResponse' <- MultiModal.getTransitRoutes transitServiceReq transitRoutesReq >>= fromMaybeM (InternalError "routes dont exist")
  otpResponse <- MInterface.MultiModalResponse <$> JM.filterTransitRoutes otpResponse'.routes merchantOperatingCityId

  logDebug $ "[Multimodal - OTP Response]" <> show otpResponse

  directJourney <-
    case searchRequest.routeCode of
      Just routeCode -> do
        let originStopCode = searchRequest.originStopCode
        let destinationStopCode = searchRequest.destinationStopCode
        fullBusData <- CQMultiModal.getRoutesBuses routeCode
        integratedBPPConfig <- maybeM (pure Nothing) (QIntegratedBPPConfig.findByDomainAndCityAndVehicleCategory "FRFS" merchantOperatingCityId BecknV2.OnDemand.Enums.BUS) (pure req.platformType)
        route'' <- maybeM (pure Nothing) (\config -> QRoute.findByRouteCode routeCode config.id) (pure integratedBPPConfig)

        -- Find the best bus with ETAs for our route
        let mbBestBus =
              listToMaybe
                ( sortOn getBestBusScore $
                    filter hasBothStops fullBusData.buses
                )
                >>= \bus -> Just (bus.vehicleNumber, bus.busData)
              where
                hasBothStops bus =
                  case (originStopCode, destinationStopCode) of
                    (Just origCode, Just destCode) ->
                      any (\eta -> eta.stopId == origCode) (fromMaybe [] bus.busData.eta_data)
                        && any (\eta -> eta.stopId == destCode) (fromMaybe [] bus.busData.eta_data)
                    _ -> True

                defaultLargeTimeDiff :: NominalDiffTime
                defaultLargeTimeDiff = realToFrac (1000000 :: Double)

                getBestBusScore bus =
                  case (originStopCode, destinationStopCode) of
                    (Just origCode, Just destCode) -> do
                      let mbOrigTime = getStopArrivalTime origCode (fromMaybe [] bus.busData.eta_data)
                      let mbDestTime = getStopArrivalTime destCode (fromMaybe [] bus.busData.eta_data)
                      case (mbOrigTime, mbDestTime) of
                        (Just origTime, Just _) ->
                          abs (diffUTCTime origTime searchRequest.startTime) -- Sort by closest arrival time
                        _ -> defaultLargeTimeDiff
                    _ -> defaultLargeTimeDiff

        let originStopTime = case (mbBestBus, originStopCode) of
              (Just (_, busData), Just origCode) ->
                getStopArrivalTime origCode (fromMaybe [] busData.eta_data)
              _ -> Nothing
        let destStopTime = case (mbBestBus, destinationStopCode) of
              (Just (_, busData), Just destCode) ->
                getStopArrivalTime destCode (fromMaybe [] busData.eta_data)
              _ -> Nothing

        let calculatedDuration = case (originStopTime, destStopTime) of
              (Just origTime, Just destTime) ->
                Seconds $ round $ diffUTCTime destTime origTime
              _ -> Seconds $ 0

        let originStopName = case (mbBestBus, originStopCode) of
              (Just (_, busData), Just origCode) ->
                getStopName origCode (fromMaybe [] busData.eta_data)
              _ -> Nothing

        let destStopName = case (mbBestBus, destinationStopCode) of
              (Just (_, busData), Just destCode) ->
                getStopName destCode (fromMaybe [] busData.eta_data)
              _ -> Nothing

        let routeDetails =
              maybe
                []
                ( \route' -> do
                    [ MultiModalTypes.MultiModalRouteDetails
                        { gtfsId = Just routeCode,
                          longName = Just route'.longName,
                          shortName = Just route'.shortName,
                          color = route'.color,
                          frequency = Nothing,
                          fromStopDetails =
                            Just $
                              MultiModalTypes.MultiModalStopDetails
                                { stopCode = originStopCode,
                                  platformCode = Nothing,
                                  name = originStopName,
                                  gtfsId = originStopCode
                                },
                          toStopDetails =
                            Just $
                              MultiModalTypes.MultiModalStopDetails
                                { stopCode = destinationStopCode,
                                  platformCode = Nothing,
                                  name = destStopName,
                                  gtfsId = destinationStopCode
                                },
                          startLocation = LocationV2 {latLng = LatLngV2 {latitude = searchRequest.fromLocation.lat, longitude = searchRequest.fromLocation.lon}},
                          endLocation = LocationV2 {latLng = LatLngV2 {latitude = destination.lat, longitude = destination.lon}},
                          subLegOrder = 0,
                          fromArrivalTime = originStopTime,
                          fromDepartureTime = originStopTime,
                          toArrivalTime = destStopTime,
                          toDepartureTime = destStopTime
                        }
                      ]
                )
                route''

        departureTimeFromSource <- case originStopTime of
          Just time -> return time
          Nothing -> return searchRequest.startTime
        let arrivalTimeAtDestination = destStopTime
        let distance = fromMaybe (Distance 0 Meter) searchRequest.distance
        toLocation <- searchRequest.toLocation & fromMaybeM (InternalError "To Location not found")
        let leg = mkMultiModalLeg distance calculatedDuration MultiModalTypes.Bus searchRequest.fromLocation.lat searchRequest.fromLocation.lon toLocation.lat toLocation.lon departureTimeFromSource arrivalTimeAtDestination originStopCode destinationStopCode originStopName destStopName routeDetails

        let directRouteInitReq =
              JMTypes.JourneyInitData
                { parentSearchId = searchRequest.id,
                  merchantId = searchRequest.merchantId,
                  merchantOperatingCityId,
                  personId = searchRequest.riderId,
                  legs = [leg],
                  estimatedDistance = distance,
                  estimatedDuration = calculatedDuration,
                  startTime = Just searchRequest.startTime,
                  endTime = arrivalTimeAtDestination,
                  maximumWalkDistance = riderConfig.maximumWalkDistance
                }
        JM.init directRouteInitReq
      Nothing -> pure Nothing

  forM_ otpResponse.routes $ \r -> do
    let initReq =
          JMTypes.JourneyInitData
            { parentSearchId = searchRequest.id,
              merchantId = searchRequest.merchantId,
              merchantOperatingCityId,
              personId = searchRequest.riderId,
              legs = r.legs,
              estimatedDistance = r.distance,
              estimatedDuration = r.duration,
              startTime = r.startTime,
              endTime = r.endTime,
              maximumWalkDistance = riderConfig.maximumWalkDistance
            }
    JM.init initReq
  QSearchRequest.updateHasMultimodalSearch (Just True) searchRequest.id
  journeys <- DQuote.getJourneys searchRequest (Just True)
  let sortedJourneys = case sortingType of
        DMP.FASTEST -> sortRoutesByDuration <$> journeys
        DMP.CHEAPEST -> sortRoutesByFare <$> journeys
        DMP.MINIMUM_TRANSITS -> sortRoutesByNumberOfLegs <$> journeys
  let sortedJourneys' = case (directJourney, sortedJourneys) of
        (Just dj, Just js) ->
          Just $
            let (directJs, otherJs) = partition (\j -> j.journeyId == dj.id) js
             in directJs <> otherJs
        _ -> sortedJourneys
  logDebug $ "sortedJourneys' " <> show sortedJourneys'
  let mbFirstJourney = listToMaybe (fromMaybe [] sortedJourneys)
  firstJourneyInfo <-
    if initateJourney
      then do
        case mbFirstJourney of
          Just firstJourney -> do
            resp <- DMC.postMultimodalInitiate (Just searchRequest.riderId, searchRequest.merchantId) firstJourney.journeyId
            return $ Just resp
          Nothing -> return Nothing
      else return Nothing
  return $
    MultimodalSearchResp
      { searchId = searchRequest.id,
        searchExpiry = searchRequest.validTill,
        journeys = fromMaybe [] sortedJourneys,
        firstJourney = mbFirstJourney,
        firstJourneyInfo = firstJourneyInfo
      }
  where
    extractDest Nothing = throwError $ InvalidRequest "Destination is required for multimodal search"
    extractDest (Just d) = return d

    getStopArrivalTime :: Text -> [CQMultiModal.BusStopETA] -> Maybe UTCTime
    getStopArrivalTime stopId eta_data =
      listToMaybe [eta.arrivalTime | eta <- eta_data, eta.stopId == stopId]

    getStopName :: Text -> [CQMultiModal.BusStopETA] -> Maybe Text
    getStopName stopId eta_data =
      listToMaybe [eta.stopName | eta <- eta_data, eta.stopId == stopId]

    sortRoutesByDuration :: [DQuote.JourneyData] -> [DQuote.JourneyData]
    sortRoutesByDuration = sortBy (\j1 j2 -> fromMaybe LT (compare <$> j1.duration <*> j2.duration))

    sortRoutesByNumberOfLegs :: [DQuote.JourneyData] -> [DQuote.JourneyData]
    sortRoutesByNumberOfLegs = sortBy (\j1 j2 -> compare (length j1.journeyLegs) (length j2.journeyLegs))

    sortRoutesByFare :: [DQuote.JourneyData] -> [DQuote.JourneyData]
    sortRoutesByFare = sortBy (\j1 j2 -> compare j1.totalMaxFare.getHighPrecMoney j2.totalMaxFare.getHighPrecMoney)

    userPreferencesToGeneralVehicleTypes :: [DTrip.MultimodalTravelMode] -> [GeneralVehicleType]
    userPreferencesToGeneralVehicleTypes = mapMaybe allowedTransitModeToGeneralVehicleType

    allowedTransitModeToGeneralVehicleType :: DTrip.MultimodalTravelMode -> Maybe GeneralVehicleType
    allowedTransitModeToGeneralVehicleType mode = case mode of
      DTrip.Bus -> Just MultiModalTypes.Bus
      DTrip.Metro -> Just MultiModalTypes.MetroRail
      DTrip.Subway -> Just MultiModalTypes.Subway
      DTrip.Walk -> Just MultiModalTypes.Walk
      _ -> Nothing

    convertSortingType :: DMP.JourneyOptionsSortingType -> SortingType
    convertSortingType sortType = case sortType of
      DMP.FASTEST -> Fastest
      DMP.MINIMUM_TRANSITS -> Minimum_Transits
      _ -> Fastest -- Default case for any other values
    mkMultiModalLeg distance duration mode originLat originLon destLat destLon departureTimeFromSource arrivalTimeAtDestination originStopCode destinationStopCode originStopName destStopName routeDetails =
      MultiModalTypes.MultiModalLeg
        { distance,
          duration,
          polyline = Polyline {encodedPolyline = ""},
          mode,
          startLocation = LocationV2 {latLng = LatLngV2 {latitude = originLat, longitude = originLon}},
          endLocation = LocationV2 {latLng = LatLngV2 {latitude = destLat, longitude = destLon}},
          fromStopDetails =
            Just $
              MultiModalTypes.MultiModalStopDetails
                { stopCode = originStopCode,
                  platformCode = Nothing,
                  name = originStopName,
                  gtfsId = originStopCode
                },
          toStopDetails =
            Just $
              MultiModalTypes.MultiModalStopDetails
                { stopCode = destinationStopCode,
                  platformCode = Nothing,
                  name = destStopName,
                  gtfsId = destinationStopCode
                },
          routeDetails = routeDetails,
          agency = Nothing,
          fromArrivalTime = Just departureTimeFromSource,
          fromDepartureTime = Just departureTimeFromSource,
          toArrivalTime = arrivalTimeAtDestination,
          toDepartureTime = arrivalTimeAtDestination
        }

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
