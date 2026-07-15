module ExternalBPP.Flow.Common where

import qualified BecknV2.FRFS.Enums as Spec
import Control.Applicative ((<|>))
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Ord (Down (..))
import Domain.Action.Beckn.FRFS.Common
import Domain.Action.Beckn.FRFS.OnInit
import Domain.Action.Beckn.FRFS.OnSearch
import Domain.Types
import qualified Domain.Types.Beckn.FRFS.OnCancel as DOnCancel
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSCancellationConfig as DFRFSCancellationConfig
import Domain.Types.FRFSConfig
import Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import ExternalBPP.ExternalAPI.Types
import qualified ExternalBPP.Flow.Fare as Flow
import Kernel.External.Maps.Google.MapsClient.Types (LatLngV2 (..), LocationV2 (..), WayPointV2 (..))
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import qualified Kernel.External.MultiModal.Interface as KMultiModal
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.JourneyModule.Utils as JMU
import SharedLogic.FRFSUtils
import qualified Storage.CachedQueries.FRFSCancellationConfig as CQFRFSCancellationConfig
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics
import qualified Tools.MultiModal as TMultiModal

search :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasMasterCloudForwarder r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Maybe BaseUrl -> Maybe Text -> DFRFSSearch.FRFSSearch -> [FRFSRouteDetails] -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> Maybe Text -> m DOnSearch
search = searchImpl False

-- | Same as 'search', but builds quotes by discovering the transit route
-- (including interchanges) between the search's stations via the transit
-- planner, instead of the route-stop mappings. Used as a fallback when
-- 'search' yields no quotes, e.g. metro journeys that need an interchange
-- where no single route serves both stations.
multimodalDiscoverySearch :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasMasterCloudForwarder r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Maybe BaseUrl -> Maybe Text -> DFRFSSearch.FRFSSearch -> [FRFSRouteDetails] -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> Maybe Text -> m DOnSearch
multimodalDiscoverySearch = searchImpl True

searchImpl :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasMasterCloudForwarder r) => Bool -> Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Maybe BaseUrl -> Maybe Text -> DFRFSSearch.FRFSSearch -> [FRFSRouteDetails] -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> Maybe Text -> m DOnSearch
searchImpl useMultimodalDiscovery merchant merchantOperatingCity integratedBPPConfig bapConfig mbNetworkHostUrl mbNetworkId searchReq routeDetails blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode mbProviderRouteId = do
  quotes <- bool buildQuotes buildMultimodalDiscoveryQuotes useMultimodalDiscovery routeDetails
  logDebug $ "Route Details Debug: " <> show routeDetails
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.searchTTLSec
  messageId <- generateGUID
  return $
    DOnSearch
      { bppSubscriberId = fromMaybe bapConfig.subscriberId mbNetworkId,
        bppSubscriberUrl = showBaseUrl $ fromMaybe bapConfig.subscriberUrl mbNetworkHostUrl,
        providerDescription = Nothing,
        providerId = bapConfig.uniqueKeyId,
        providerName = CallAPI.getProviderName integratedBPPConfig,
        quotes = quotes,
        validTill = validTill,
        transactionId = searchReq.id.getId,
        messageId = messageId,
        bppDelayedInterest = Nothing
      }
  where
    -- Build Single Transit Route Quote
    buildQuotes [routeDetail] = buildSingleTransitRouteQuote routeDetail
    -- Build Multiple Transit Routes Quotes
    buildQuotes routeDetails_@(_ : _) = buildMultipleNonTransitRouteQuotes routeDetails_
    buildQuotes [] = return []

    buildSingleTransitRouteQuote FRFSRouteDetails {..} = do
      case routeCode of
        Just routeCode' -> do
          route <- OTPRest.getRouteByRouteId integratedBPPConfig routeCode' >>= fromMaybeM (RouteNotFound routeCode')
          let routeInfo =
                RouteStopInfo
                  { route,
                    totalStops = Nothing,
                    stops = Nothing,
                    startStopCode = startStationCode,
                    endStopCode = endStationCode,
                    travelTime = Nothing
                  }
          mkQuote serviceTier searchReq.vehicleType [routeInfo]
        Nothing -> do
          routesInfo <- bool getPossibleRoutesBetweenTwoStops getPossibleRoutesBetweenTwoParentStops (fromMaybe False searchReq.searchAsParentStops) startStationCode endStationCode integratedBPPConfig
          quotes <-
            mapM
              ( \routeInfo -> do
                  mkQuote Nothing searchReq.vehicleType [routeInfo]
              )
              routesInfo
          return $ concat quotes

    -- Multimodal discovery path: quote a single transit journey (no fixed
    -- route) by discovering the actual legs from the transit planner. Covers
    -- interchange journeys where no single route serves both stations.
    buildMultimodalDiscoveryQuotes = \case
      [FRFSRouteDetails {routeCode = Nothing, startStationCode, endStationCode}] -> mkInterchangeQuote startStationCode endStationCode
      _ -> return []

    -- Discover the interchange legs from the transit planner and quote them as
    -- a multi-leg journey through the regular quote path. Enabled only for
    -- providers whose fare depends solely on origin and destination (CMRL), so
    -- the fare is correct irrespective of the discovered legs.
    mkInterchangeQuote startStationCode endStationCode = do
      routesInfoResult <- withTryCatch "discoverInterchangeRoutesInfo" (discoverInterchangeRoutesInfo startStationCode endStationCode)
      case routesInfoResult of
        Left err -> do
          logError $ "Failed to discover interchange routes between stations: " <> startStationCode <> " and " <> endStationCode <> ", error: " <> show err
          return []
        Right routesInfo -> mkQuote Nothing searchReq.vehicleType routesInfo

    -- Asks the transit planner (OTP) for a transit journey between the two
    -- stations, restricted to the search's vehicle mode, and maps each leg of
    -- the best route to a RouteStopInfo.
    discoverInterchangeRoutesInfo startStationCode endStationCode = do
      fromStation <- OTPRest.getStationByGtfsIdAndStopCode startStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound startStationCode)
      toStation <- OTPRest.getStationByGtfsIdAndStopCode endStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound endStationCode)
      fromLat <- fromStation.lat & fromMaybeM (InternalError $ "Latitude not found for station: " <> fromStation.code)
      fromLon <- fromStation.lon & fromMaybeM (InternalError $ "Longitude not found for station: " <> fromStation.code)
      toLat <- toStation.lat & fromMaybeM (InternalError $ "Latitude not found for station: " <> toStation.code)
      toLon <- toStation.lon & fromMaybeM (InternalError $ "Longitude not found for station: " <> toStation.code)
      riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId}) (Just (CQRC.findByMerchantOperatingCityId merchantOperatingCity.id)) >>= fromMaybeM (RiderConfigNotFound merchantOperatingCity.id.getId)
      transitServiceReq <- TMultiModal.getTransitServiceReq merchant.id merchantOperatingCity.id
      now <- getCurrentTime
      let expectedMode = case searchReq.vehicleType of
            Spec.METRO -> MultiModalTypes.MetroRail
            Spec.SUBWAY -> MultiModalTypes.Subway
            Spec.BUS -> MultiModalTypes.Bus
          transitRoutesReq =
            GetTransitRoutesReq
              { origin = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = fromLat, longitude = fromLon}}},
                destination = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = toLat, longitude = toLon}}},
                arrivalTime = Nothing,
                departureTime = Just now,
                mode = Nothing,
                transitPreferences = Nothing,
                transportModes = Nothing,
                minimumWalkDistance = riderConfig.minimumWalkDistance,
                permissibleModes = [expectedMode],
                maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs,
                sortingType = MultiModalTypes.Fastest,
                walkSpeed = Nothing
              }
      otpResponse <- KMultiModal.getTransitRoutes Nothing transitServiceReq transitRoutesReq >>= fromMaybeM (InternalError $ "No transit routes found between stations: " <> fromStation.code <> " and " <> toStation.code)
      transitRoute <-
        find (\route -> any (\leg -> leg.mode == expectedMode) route.legs && all (\leg -> leg.mode == expectedMode || leg.mode == MultiModalTypes.Walk) route.legs) otpResponse.routes
          & fromMaybeM (InternalError $ "No suitable transit route found between stations: " <> fromStation.code <> " and " <> toStation.code)
      let legsRouteDetails = sortOn (.subLegOrder) $ concatMap (.routeDetails) (filter (\leg -> leg.mode == expectedMode) transitRoute.legs)
      forM legsRouteDetails $ \legRouteDetail -> do
        routeCode <- (legRouteDetail.gtfsId <&> gtfsIdtoDomainCode) & fromMaybeM (InternalError "Route gtfsId not found in transit leg")
        legStartStopCode <- ((legRouteDetail.fromStopDetails >>= (.stopCode)) <|> ((legRouteDetail.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InternalError "From stop code not found in transit leg")
        legEndStopCode <- ((legRouteDetail.toStopDetails >>= (.stopCode)) <|> ((legRouteDetail.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)) & fromMaybeM (InternalError "To stop code not found in transit leg")
        route <- OTPRest.getRouteByRouteId integratedBPPConfig routeCode >>= fromMaybeM (RouteNotFound routeCode)
        return $
          RouteStopInfo
            { route,
              totalStops = Nothing,
              stops = Nothing,
              startStopCode = legStartStopCode,
              endStopCode = legEndStopCode,
              travelTime = nominalDiffTimeToSeconds <$> (diffUTCTime <$> legRouteDetail.toArrivalTime <*> legRouteDetail.fromDepartureTime)
            }

    buildMultipleNonTransitRouteQuotes routesDetails = do
      case routesDetails of
        [] -> return []
        _ -> do
          routesInfo <-
            mapM
              ( \routeDetail -> do
                  route <- (maybe (pure Nothing) (OTPRest.getRouteByRouteId integratedBPPConfig) routeDetail.routeCode) >>= fromMaybeM (RouteNotFound (fromMaybe " " routeDetail.routeCode))
                  return $
                    RouteStopInfo
                      { route,
                        totalStops = Nothing,
                        stops = Nothing,
                        startStopCode = routeDetail.startStationCode,
                        endStopCode = routeDetail.endStationCode,
                        travelTime = Nothing
                      }
              )
              routesDetails
          mkQuote Nothing searchReq.vehicleType routesInfo

    mkQuote :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasMasterCloudForwarder r) => Maybe Spec.ServiceTierType -> Spec.VehicleCategory -> [RouteStopInfo] -> m [DQuote]
    mkQuote _serviceTier _vehicleType [] = return []
    mkQuote serviceTier vehicleType routesInfo = do
      logDebug $ "Routes Info Debug: " <> show routesInfo
      let segments = map (\routeInfo -> CallAPI.BasicRouteDetail {routeCode = routeInfo.route.code, startStopCode = routeInfo.startStopCode, endStopCode = routeInfo.endStopCode}) routesInfo
          fareRoute = CallAPI.FareRoute {segments = NE.fromList segments, mbProviderRouteId}
      stations <- CallAPI.buildStations segments integratedBPPConfig
      (_, fares) <- Flow.getFares searchReq.riderId merchant.id merchantOperatingCity.id integratedBPPConfig fareRoute vehicleType serviceTier searchReq.multimodalSearchRequestId blacklistedServiceTiers blacklistedFareQuoteTypes False isSingleMode
      return $
        map
          ( \FRFSFare {..} ->
              let adultPrice = maybe (Price (Money 0) (HighPrecMoney 0.0) INR) (.price) (find (\category -> category.category == ADULT) categories)
                  adultBppItemId = maybe (CallAPI.getProviderName integratedBPPConfig) (.bppItemId) (find (\category -> category.category == ADULT) categories)
                  routeStations =
                    map
                      ( \routeInfo ->
                          DRouteStation
                            { routeCode = routeInfo.route.code,
                              routeLongName = routeInfo.route.longName,
                              routeShortName = routeInfo.route.shortName,
                              routeStartPoint = routeInfo.route.startPoint,
                              routeEndPoint = routeInfo.route.endPoint,
                              routeStations = stations,
                              routeTravelTime = routeInfo.travelTime,
                              routeServiceTier = Just $ mkDVehicleServiceTier vehicleServiceTier,
                              routePrice = adultPrice,
                              routeSequenceNum = Nothing,
                              routeColor = Nothing
                            }
                      )
                      routesInfo
               in DQuote
                    { bppItemId = adultBppItemId,
                      routeCode = (NE.head fareRoute.segments).routeCode,
                      _type = DFRFSQuote.SingleJourney,
                      routeStations = routeStations,
                      fareDetails = fareDetails,
                      categories = map mkDCategory categories,
                      ..
                    }
          )
          fares

    mkDVehicleServiceTier FRFSVehicleServiceTier {..} = DVehicleServiceTier {..}

    mkDCategory FRFSTicketCategory {..} =
      DCategory
        { bppItemId = bppItemId,
          offeredPrice = offeredPrice,
          price = price,
          ..
        }

init :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, HasMasterCloudForwarder r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> m DOnInit
init merchant merchantOperatingCity integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking quoteCategories = do
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.initTTLSec
  paymentDetails <- mkPaymentDetails bapConfig.collectedBy
  bankAccountNumber <- paymentDetails.bankAccNumber & fromMaybeM (InternalError "Bank Account Number Not Found")
  bankCode <- paymentDetails.bankCode & fromMaybeM (InternalError "Bank Code Not Found")
  bppOrderId <- CallAPI.getBppOrderId integratedBPPConfig booking
  return $
    DOnInit
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.totalPrice,
        categories = map mkDCategorySelect quoteCategories,
        fareBreakUp = [],
        validTill = validTill,
        transactionId = booking.searchId.getId,
        messageId = booking.id.getId,
        bankAccNum = bankAccountNumber,
        bankCode = bankCode,
        bppOrderId = bppOrderId
      }
  where
    mkDCategorySelect quoteCategory =
      DCategorySelect
        { bppItemId = CallAPI.getProviderName integratedBPPConfig,
          quantity = quoteCategory.selectedQuantity,
          category = quoteCategory.category,
          price = quoteCategory.offeredPrice
        }
    mkPaymentDetails = \case
      Spec.BAP -> do
        let paymentParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
        paymentParams & fromMaybeM (InternalError "BknPaymentParams Not Found")
      Spec.BPP -> CallAPI.getPaymentDetails merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking

confirm :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c, Metrics.HasBAPMetrics m r, HasMasterCloudForwarder r) => Merchant -> MerchantOperatingCity -> FRFSConfig -> IntegratedBPPConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> Maybe Bool -> m DOrder
confirm _merchant _merchantOperatingCity frfsConfig integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking quoteCategories mbIsSingleMode = do
  let baseQrTtl =
        case booking.vehicleType of
          Spec.BUS -> Seconds $ if (fromMaybe False mbIsSingleMode) then frfsConfig.busStationTtl.getSeconds else 2 * frfsConfig.busStationTtl.getSeconds
          Spec.METRO -> Seconds frfsConfig.metroStationTtl
          _ -> Seconds frfsConfig.metroStationTtl
  qrTtl <- case (booking.vehicleType, booking.tripId, booking.routeCode) of
    (Spec.BUS, Just tripId, Just routeCode) -> do
      let (waybillNo, tripNo) = JMU.getWaybillNoAndTripNoFromTripId tripId
      mbSchedule <- withTryCatch "getBusTripSchedule_confirm" (OTPRest.getBusTripSchedule waybillNo tripNo routeCode integratedBPPConfig)
      case mbSchedule of
        Right [] -> return baseQrTtl
        Right (firstSchedule : _) -> do
          case find (\eta -> eta.stopCode == booking.fromStationCode) firstSchedule.eta of
            Just boardingStopEta -> do
              currentTime <- getCurrentTime
              let extraTtlSeconds =
                    ( nominalDiffTimeToSeconds $
                        diffUTCTime
                          (unixToUTC boardingStopEta.arrivalTimeUnix)
                          currentTime
                    ).getSeconds
              let addedTtl = max extraTtlSeconds 0
              return $ Seconds (baseQrTtl.getSeconds + addedTtl)
            Nothing -> return baseQrTtl
        Left err -> do
          logError $ "Failed to fetch bus trip schedule: " <> show err
          return baseQrTtl
    _ -> return baseQrTtl
  order <- CallAPI.createOrder integratedBPPConfig qrTtl (mRiderName, mRiderNumber) booking quoteCategories
  let tickets =
        map
          ( \ticket ->
              DTicket
                { qrData = ticket.qrData,
                  vehicleNumber = ticket.vehicleNumber,
                  bppFulfillmentId = Just $ CallAPI.getProviderName integratedBPPConfig,
                  ticketNumber = ticket.ticketNumber,
                  validTill = ticket.qrValidity,
                  status = ticket.qrStatus,
                  description = ticket.description,
                  qrRefreshAt = ticket.qrRefreshAt,
                  commencingHours = ticket.commencingHours,
                  isReturnTicket = Nothing
                }
          )
          order.tickets
  return $
    DOrder
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.totalPrice.amount,
        fareBreakUp = [],
        bppOrderId = order.orderId,
        bppItemId = CallAPI.getProviderName integratedBPPConfig,
        transactionId = booking.searchId.getId,
        orderStatus = Nothing,
        messageId = booking.id.getId,
        tickets = tickets
      }

status :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, HasMasterCloudForwarder r) => Id Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
status _merchantId _merchantOperatingCity integratedBPPConfig bapConfig booking = do
  bppOrderId <- booking.bppOrderId & fromMaybeM (InternalError "BPP Order Id Not Found")
  tickets' <- CallAPI.getTicketStatus integratedBPPConfig booking
  let tickets =
        map
          ( \ticket ->
              DTicket
                { qrData = ticket.qrData,
                  vehicleNumber = ticket.vehicleNumber,
                  bppFulfillmentId = Just $ CallAPI.getProviderName integratedBPPConfig,
                  ticketNumber = ticket.ticketNumber,
                  validTill = ticket.qrValidity,
                  status = ticket.qrStatus,
                  qrRefreshAt = ticket.qrRefreshAt,
                  description = ticket.description,
                  commencingHours = ticket.commencingHours,
                  isReturnTicket = Nothing
                }
          )
          tickets'
  return $
    DOrder
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.totalPrice.amount,
        fareBreakUp = [],
        bppOrderId = bppOrderId,
        bppItemId = CallAPI.getProviderName integratedBPPConfig,
        transactionId = booking.searchId.getId,
        orderStatus = Nothing,
        messageId = booking.id.getId,
        tickets = tickets
      }

verifyTicket :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, HasMasterCloudForwarder r) => Id Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Text -> m DTicketPayload
verifyTicket _merchantId _merchantOperatingCity integratedBPPConfig _bapConfig encryptedQrData = do
  TicketPayload {..} <- CallAPI.verifyTicket integratedBPPConfig encryptedQrData
  return DTicketPayload {..}

cancel :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, HasMasterCloudForwarder r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Spec.CancellationType -> DFRFSTicketBooking.FRFSTicketBooking -> m DOnCancel.DOnCancel
cancel _merchant merchantOperatingCity integratedBPPConfig bapConfig cancellationType booking = do
  bppOrderId <- booking.bppOrderId & fromMaybeM (InternalError "BPP Order Id Not Found")
  let orderStatus = case cancellationType of
        Spec.SOFT_CANCEL -> Spec.SOFT_CANCELLED
        Spec.CONFIRM_CANCEL -> Spec.CANCELLED
  let baseFare = booking.totalPrice.amount
      departureTime = fromMaybe booking.validTill booking.startTime
  (charges, refund) <- calculateCancellationCharges merchantOperatingCity.id booking.vehicleType baseFare departureTime
  return $
    DOnCancel.DOnCancel
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.totalPrice.amount,
        bppOrderId = bppOrderId,
        bppItemId = CallAPI.getProviderName integratedBPPConfig,
        transactionId = booking.searchId.getId,
        messageId = booking.id.getId,
        orderStatus = orderStatus,
        refundAmount = Just refund,
        baseFare = baseFare,
        cancellationCharges = Just charges
      }

calculateCancellationCharges ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Spec.VehicleCategory ->
  HighPrecMoney ->
  UTCTime ->
  m (HighPrecMoney, HighPrecMoney)
calculateCancellationCharges merchantOpCityId vehicleCategory baseFare departureTime = do
  configs <- CQFRFSCancellationConfig.findAllByMerchantOpCityAndVehicleCategory merchantOpCityId vehicleCategory
  now <- getCurrentTime
  let minutesBefore = floor (diffUTCTime departureTime now / 60) :: Int
      sortedConfigs = sortOn (Down . (.minMinutesBeforeDeparture)) configs
      mbMatchingTier = find (matchesTier minutesBefore) sortedConfigs
  case mbMatchingTier of
    Nothing -> return (0, baseFare) -- no config → full refund
    Just tier -> do
      let rawCharges = case tier.cancellationChargeType of
            DFRFSCancellationConfig.PERCENTAGE -> baseFare * tier.cancellationChargeValue / 100
            DFRFSCancellationConfig.FLAT -> tier.cancellationChargeValue
          charges = min baseFare (max 0 rawCharges)
          refund = baseFare - charges
      when (rawCharges /= charges) $
        logError $
          "FRFSCancellationConfig misconfigured for tier " <> tier.id.getId
            <> " (type="
            <> show tier.cancellationChargeType
            <> ", value="
            <> show tier.cancellationChargeValue
            <> ", baseFare="
            <> show baseFare
            <> "): rawCharges="
            <> show rawCharges
            <> " clamped to "
            <> show charges
      return (charges, refund)
  where
    matchesTier mins tier =
      mins >= tier.minMinutesBeforeDeparture
        && maybe True (mins <) tier.maxMinutesBeforeDeparture
