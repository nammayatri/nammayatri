module ExternalBPP.Flow.Common where

import qualified BecknV2.FRFS.Enums as Spec
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time (utctDay)
import Domain.Action.Beckn.FRFS.Common
import Domain.Action.Beckn.FRFS.OnInit
import Domain.Action.Beckn.FRFS.OnSearch
import Domain.Types
import Domain.Types.BecknConfig
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
import qualified Domain.Types.StationType as Station
import qualified ExternalBPP.ExternalAPI.Bus.OSRTC.Enums as OSRTCEnums
import qualified ExternalBPP.ExternalAPI.Bus.OSRTC.Trip as OSRTCTrip
import qualified ExternalBPP.ExternalAPI.Bus.OSRTC.Types as OSRTCTypes
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import ExternalBPP.ExternalAPI.Types
import qualified ExternalBPP.Flow.Fare as Flow
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
import qualified Lib.Payment.Domain.Action as LibPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified SharedLogic.External.Nandi.Flow as NandiFlow
import SharedLogic.FRFSUtils
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics
import qualified Tools.MultiModal as MM

search :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Maybe BaseUrl -> Maybe Text -> DFRFSSearch.FRFSSearch -> [FRFSRouteDetails] -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> Maybe Text -> m DOnSearch
search merchant merchantOperatingCity integratedBPPConfig bapConfig mbNetworkHostUrl mbNetworkId searchReq routeDetails blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode mbProviderRouteId = do
  case integratedBPPConfig.providerConfig of
    OSRTC osrtcConfig -> osrtcSearchTrips osrtcConfig
    _ -> defaultSearch
  where
    defaultSearch = do
      quotes <- buildQuotes routeDetails
      logDebug $ "Route Details Debug: " <> show routeDetails
      mkDOnSearch quotes

    mkDOnSearch quotes = do
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

    mkQuote :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Maybe Spec.ServiceTierType -> Spec.VehicleCategory -> [RouteStopInfo] -> m [DQuote]
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
                      osrtcTripDetail = Nothing,
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

    osrtcSearchTrips osrtcConfig = do
      journeyDate <- searchReq.journeyDate & fromMaybeM (InvalidRequest "journeyDate is required for OSRTC search")
      let day = utctDay journeyDate
      let resolveOSRTCStationIntId stationCode = do
            baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
            rawStation <- NandiFlow.getStationsByGtfsIdAndStopCode baseUrl integratedBPPConfig.feedKey stationCode
            readMaybe (T.unpack rawStation.providerCode)
              & fromMaybeM
                ( InvalidRequest $
                    "Cannot resolve OSRTC intStationID for code: "
                      <> stationCode
                      <> ". providerCode='"
                      <> rawStation.providerCode
                      <> "' is not a valid integer. Ensure the GTFS feed has intStationID as providerCode."
                )
      fromStationId <- resolveOSRTCStationIntId searchReq.fromStationCode
      toStationId <- resolveOSRTCStationIntId searchReq.toStationCode
      resp <-
        OSRTCTrip.searchTrip osrtcConfig $
          OSRTCTypes.OSRTCSearchTripReq
            { intFromStationID = fromStationId,
              intToStationID = toStationId,
              dteJourneyDate = day,
              intPlatformID = osrtcConfig.platformId
            }
      let fromStation =
            DStation
              { stationCode = searchReq.fromStationCode,
                stationName = fromMaybe searchReq.fromStationCode searchReq.fromStationName,
                stationLat = (.lat) <$> searchReq.fromStationPoint,
                stationLon = (.lon) <$> searchReq.fromStationPoint,
                stationType = Station.START,
                stopSequence = Nothing,
                towards = Nothing
              }
          toStation =
            DStation
              { stationCode = searchReq.toStationCode,
                stationName = fromMaybe searchReq.toStationCode searchReq.toStationName,
                stationLat = (.lat) <$> searchReq.toStationPoint,
                stationLon = (.lon) <$> searchReq.toStationPoint,
                stationType = Station.END,
                stopSequence = Nothing,
                towards = Nothing
              }
          osrtcTrips = filter isOSRTCTrip resp._data
          nonOsrtcCount = length resp._data - length osrtcTrips
          tripQuotes = mapMaybe (mkOSRTCQuote fromStation toStation) osrtcTrips
      when (nonOsrtcCount > 0) $
        logWarning $ "OSRTC SearchTrip returned " <> show nonOsrtcCount <> " trip(s) with non-OSRTC operator — skipped"
      mkDOnSearch tripQuotes

    isOSRTCTrip trip =
      OSRTCTypes.strServiceOperatorName trip == "OSRTC"
        && OSRTCTypes.strOrganizationEntityName trip == "OSRTC"

    mkOSRTCQuote fromStation toStation trip = do
      let tripId = show trip.intServiceTripDepartureID
          adultFareRows = filter (\fr -> fr.intTicketCategoryID == OSRTCEnums.Adult || fr.intTicketCategoryID == OSRTCEnums.Men) trip.fare
          minFare = if null adultFareRows then trip.decMinFare else minimum (map (.decFareAmount) adultFareRows)
          adultPrice = Price (Money $ round minFare) (HighPrecMoney $ toRational minFare) INR
      guard trip.bAllowBooking
      Just $
        DQuote
          { bppItemId = tripId,
            routeCode = trip.strServiceTripCode,
            vehicleType = Spec.BUS,
            routeStations = [],
            stations = [fromStation, toStation],
            categories = [DCategory ADULT adultPrice adultPrice tripId True],
            fareDetails = Nothing,
            osrtcTripDetail = Just $ A.toJSON trip,
            _type = DFRFSQuote.SingleJourney
          }

init :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> m DOnInit
init merchant merchantOperatingCity integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking quoteCategories = do
  case integratedBPPConfig.providerConfig of
    OSRTC osrtcConfig -> osrtcInit osrtcConfig
    _ -> defaultInit
  where
    defaultInit = buildOnInit Nothing

    buildOnInit mbBppOrderId = do
      validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.initTTLSec
      paymentDetails <- mkPaymentDetails bapConfig.collectedBy
      bankAccountNumber <- paymentDetails.bankAccNumber & fromMaybeM (InternalError "Bank Account Number Not Found")
      bankCode <- paymentDetails.bankCode & fromMaybeM (InternalError "Bank Code Not Found")
      bppOrderId <- case mbBppOrderId of
        Just pnr -> return (Just pnr)
        Nothing -> CallAPI.getBppOrderId integratedBPPConfig booking
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

    osrtcInit osrtcConfig = do
      mbExistingPnr :: Maybe Text <- Hedis.get (osrtcPnrKey booking.id)
      case mbExistingPnr of
        Just existingPnr -> do
          logInfo $ "OSRTC init retry: reusing existing PNR for bookingId=" <> booking.id.getId
          buildOnInit (Just existingPnr)
        Nothing -> do
          mbBookingData :: Maybe OSRTCTypes.OSRTCInsertBookingReq <- Hedis.get (osrtcBookingDataKey booking.searchId)
          bookingData <- fromMaybeM (InvalidRequest "OSRTC booking data not found. Complete seat selection and fare calculation first.") mbBookingData
          let insertReq =
                bookingData
                  { OSRTCTypes.intPlatformID = osrtcConfig.platformId,
                    OSRTCTypes.intPaymentModeID = osrtcConfig.intPaymentModeId
                  }
          resp <- OSRTCTrip.insertTicketBooking osrtcConfig insertReq
          insertRes <- case resp._data of
            (r : _) -> return r
            [] -> throwError $ InternalError "OSRTC InsertTicketBooking returned empty response"
          Hedis.setExp (osrtcPnrKey booking.id) insertRes.strPNRNo osrtcHoldTtl
          Hedis.setExp (osrtcPaymentRefKey booking.id) insertRes.strPaymentObjRefNo osrtcHoldTtl
          Hedis.setExp (osrtcTicketBookingIdKey booking.id) insertRes.intTicketBookingID osrtcHoldTtl
          logInfo $ "OSRTC InsertTicketBooking success: PNR=" <> insertRes.strPNRNo <> " bookingId=" <> booking.id.getId
          buildOnInit (Just insertRes.strPNRNo)

confirm :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, Metrics.HasBAPMetrics m r) => Merchant -> MerchantOperatingCity -> FRFSConfig -> IntegratedBPPConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> Maybe Bool -> Maybe (DPaymentOrder.PaymentOrder, Maybe LibPayment.PaymentStatusResp) -> m DOrder
confirm _merchant _merchantOperatingCity frfsConfig integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking quoteCategories mbIsSingleMode mbOsrtcPaymentContext = do
  case integratedBPPConfig.providerConfig of
    OSRTC osrtcConfig -> osrtcConfirm osrtcConfig mbOsrtcPaymentContext
    _ -> defaultConfirm
  where
    defaultConfirm = do
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

    osrtcConfirm osrtcConfig mbPaymentContext = do
      pnrNo <- Hedis.get (osrtcPnrKey booking.id) >>= fromMaybeM (InternalError "OSRTC PNR not found for booking")
      paymentRefNo <- Hedis.get (osrtcPaymentRefKey booking.id) >>= fromMaybeM (InternalError "OSRTC payment ref not found for booking")
      (payOrder, mbPayStatus) <- mbPaymentContext & fromMaybeM (InternalError "OSRTC confirm requires Juspay payment order and status")
      let isCharged = case mbPayStatus of
            Just LibPayment.PaymentStatus {status = paymentStatus} -> paymentStatus == Payment.CHARGED
            _ -> False
      unless isCharged $
        throwError $ InternalError $ "OSRTC confirm called with non-CHARGED payment status: " <> show mbPayStatus
      let meta = mkOSRTCPaymentMetadata payOrder mbPayStatus
      let updateReq =
            OSRTCTypes.OSRTCUpdateBookingReq
              { strPaymentObjRefNo = paymentRefNo,
                intPGPaymentStatusID = 2,
                strStatusCode = OSRTCEnums.SUCCESS,
                strBankRefNo = meta.pmBankRefNo,
                strPGTrackingRefNo = meta.pmPgTrackingRefNo,
                intAmount = meta.pmAmount,
                strPaymentMode = meta.pmPaymentMode,
                strCardNumber = meta.pmCardNumber,
                strBankStatus = OSRTCEnums.SUCCESS,
                strPGType = osrtcConfig.strPGType,
                strFullResponse = meta.pmFullResponse
              }
      resp <- OSRTCTrip.updateTicketBookingResponse osrtcConfig updateReq
      mbJourneyDate :: Maybe UTCTime <- Hedis.get (osrtcJourneyDateKey booking.searchId)
      journeyDate <- case mbJourneyDate of
        Just jd -> return jd
        Nothing -> do
          logWarning $ "OSRTC: journeyDate Redis key missing for searchId=" <> booking.searchId.getId <> "; falling back to DB"
          journeySearch <- QFRFSSearch.findById booking.searchId >>= fromMaybeM (InternalError $ "OSRTC confirm: FRFSSearch not found for searchId=" <> booking.searchId.getId)
          journeySearch.journeyDate & fromMaybeM (InternalError $ "OSRTC confirm: journeyDate not set on FRFSSearch for searchId=" <> booking.searchId.getId)
      let qrValidTill = addUTCTime (24 * 60 * 60) journeyDate
      Hedis.setExp (osrtcTicketInfoKey booking.id) (OSRTCTicketInfo {qrData = resp._data.strQRCode, validTill = qrValidTill}) (7 * 24 * 60 * 60)
      return $
        DOrder
          { providerId = bapConfig.uniqueKeyId,
            totalPrice = booking.totalPrice.amount,
            fareBreakUp = [],
            bppOrderId = pnrNo,
            bppItemId = CallAPI.getProviderName integratedBPPConfig,
            transactionId = booking.searchId.getId,
            orderStatus = Nothing,
            messageId = booking.id.getId,
            tickets =
              [ DTicket
                  { qrData = resp._data.strQRCode,
                    vehicleNumber = Nothing,
                    bppFulfillmentId = Just $ CallAPI.getProviderName integratedBPPConfig,
                    ticketNumber = pnrNo,
                    validTill = qrValidTill,
                    status = "ACTIVE",
                    description = Nothing,
                    qrRefreshAt = Nothing,
                    commencingHours = Nothing,
                    isReturnTicket = Nothing
                  }
              ]
          }

status :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
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

verifyTicket :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Text -> m DTicketPayload
verifyTicket _merchantId _merchantOperatingCity integratedBPPConfig _bapConfig encryptedQrData = do
  TicketPayload {..} <- CallAPI.verifyTicket integratedBPPConfig encryptedQrData
  return DTicketPayload {..}

-- OSRTC Redis key helpers

osrtcBookingDataKey :: Id DFRFSSearch.FRFSSearch -> Text
osrtcBookingDataKey searchId = "OSRTC:BookingData:" <> searchId.getId

osrtcPnrKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
osrtcPnrKey bookingId = "OSRTC:PNR:" <> bookingId.getId

osrtcPaymentRefKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
osrtcPaymentRefKey bookingId = "OSRTC:PaymentRef:" <> bookingId.getId

osrtcTicketBookingIdKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
osrtcTicketBookingIdKey bookingId = "OSRTC:TicketBookingId:" <> bookingId.getId

osrtcJourneyDateKey :: Id DFRFSSearch.FRFSSearch -> Text
osrtcJourneyDateKey searchId = "OSRTC:JourneyDate:" <> searchId.getId

osrtcTicketInfoKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
osrtcTicketInfoKey bookingId = "OSRTC:TicketInfo:" <> bookingId.getId

data OSRTCTicketInfo = OSRTCTicketInfo
  { qrData :: Text,
    validTill :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

osrtcHoldTtl :: Int
osrtcHoldTtl = 900

-- | TTL for Redis OSRTC:BookingData:{searchId} — seat/passenger payload before payment (seconds).
osrtcBookingDataTtl :: Int
osrtcBookingDataTtl = 1200

data OSRTCPaymentMetadata = OSRTCPaymentMetadata
  { pmBankRefNo :: Text,
    pmPgTrackingRefNo :: Text,
    pmPaymentMode :: Text,
    pmCardNumber :: Text,
    pmFullResponse :: Text,
    pmAmount :: Double
  }

mkOSRTCPaymentMetadata :: DPaymentOrder.PaymentOrder -> Maybe LibPayment.PaymentStatusResp -> OSRTCPaymentMetadata
mkOSRTCPaymentMetadata order mbResp =
  case mbResp of
    Just resp@LibPayment.PaymentStatus {txnId, authIdCode, paymentMethodType, card, amount} ->
      OSRTCPaymentMetadata
        { pmBankRefNo = fromMaybe order.paymentServiceOrderId (txnId <|> authIdCode),
          pmPgTrackingRefNo = order.paymentServiceOrderId,
          pmPaymentMode = fromMaybe "ONLINE" paymentMethodType,
          pmCardNumber = maybe "" (\c -> fromMaybe "" c.lastFourDigits) card,
          pmFullResponse = encodeToText resp,
          pmAmount = fromRational $ toRational amount
        }
    _ ->
      OSRTCPaymentMetadata
        { pmBankRefNo = order.paymentServiceOrderId,
          pmPgTrackingRefNo = order.paymentServiceOrderId,
          pmPaymentMode = "ONLINE",
          pmCardNumber = "",
          pmFullResponse = maybe "" encodeToText mbResp,
          pmAmount = fromRational $ toRational order.amount
        }

osrtcReleaseHold ::
  (CoreMetrics m, CacheFlow m r, EncFlow m r, MonadFlow m, HasRequestId r, MonadReader r m) =>
  OSRTCConfig ->
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  HighPrecMoney ->
  Maybe DPaymentOrder.PaymentOrder ->
  Maybe LibPayment.PaymentStatusResp ->
  m ()
osrtcReleaseHold osrtcConfig bookingId amount mbPayOrder mbPayStatus = do
  mbPnrNo :: Maybe Text <- Hedis.get (osrtcPnrKey bookingId)
  mbPaymentRefNo :: Maybe Text <- Hedis.get (osrtcPaymentRefKey bookingId)
  case (mbPnrNo, mbPaymentRefNo) of
    (Just _pnrNo, Just paymentRefNo) -> do
      void $ OSRTCTrip.updateTicketBookingResponse osrtcConfig (updateReq paymentRefNo)
      mapM_ Hedis.del [osrtcPnrKey bookingId, osrtcPaymentRefKey bookingId]
      logInfo $ "OSRTC hold released for bookingId=" <> bookingId.getId
    _ -> logWarning $ "OSRTC PNR/PaymentRef not found in Redis for bookingId=" <> bookingId.getId <> ", hold may have already expired"
  where
    meta = case mbPayOrder of
      Just order -> mkOSRTCPaymentMetadata order mbPayStatus
      Nothing ->
        OSRTCPaymentMetadata
          { pmBankRefNo = "",
            pmPgTrackingRefNo = "",
            pmPaymentMode = "ONLINE",
            pmCardNumber = "",
            pmFullResponse = maybe "" encodeToText mbPayStatus,
            pmAmount = fromRational $ toRational amount
          }
    updateReq paymentRefNo =
      OSRTCTypes.OSRTCUpdateBookingReq
        { strPaymentObjRefNo = paymentRefNo,
          intPGPaymentStatusID = 1,
          strStatusCode = OSRTCEnums.FAILED,
          strBankRefNo = meta.pmBankRefNo,
          strPGTrackingRefNo = meta.pmPgTrackingRefNo,
          intAmount = meta.pmAmount,
          strPaymentMode = meta.pmPaymentMode,
          strCardNumber = meta.pmCardNumber,
          strBankStatus = OSRTCEnums.FAILED,
          strPGType = osrtcConfig.strPGType,
          strFullResponse = meta.pmFullResponse
        }
