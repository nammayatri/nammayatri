module ExternalBPP.Flow where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.List.NonEmpty as NE
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
import Domain.Types.Person
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FRFSUtils
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics

getFares :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id Person -> Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> NonEmpty CallAPI.BasicRouteDetail -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> Maybe Text -> m (Bool, [FRFSFare])
getFares riderId merchant merchantOperatingCity integratedBPPConfig _bapConfig fareRouteDetails vehicleCategory serviceTier mbParentSearchReqId = do
  try @_ @SomeException (CallAPI.getFares riderId merchant merchantOperatingCity integratedBPPConfig fareRouteDetails vehicleCategory serviceTier mbParentSearchReqId) >>= \case
    Left _ -> return (True, [])
    Right fares -> return fares

search :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Maybe BaseUrl -> Maybe Text -> DFRFSSearch.FRFSSearch -> [FRFSRouteDetails] -> m DOnSearch
search merchant merchantOperatingCity integratedBPPConfig bapConfig mbNetworkHostUrl mbNetworkId searchReq routeDetails = do
  quotes <- buildQuotes routeDetails
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
          mkQuote searchReq.vehicleType [routeInfo]
        Nothing -> do
          routesInfo <- bool getPossibleRoutesBetweenTwoStops getPossibleRoutesBetweenTwoParentStops (fromMaybe False searchReq.searchAsParentStops) startStationCode endStationCode integratedBPPConfig
          quotes <-
            mapM
              ( \routeInfo -> do
                  mkQuote searchReq.vehicleType [routeInfo]
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
          mkQuote searchReq.vehicleType routesInfo

    mkQuote :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Spec.VehicleCategory -> [RouteStopInfo] -> m [DQuote]
    mkQuote _vehicleType [] = return []
    mkQuote vehicleType routesInfo = do
      let fareRouteDetails = map (\routeInfo -> CallAPI.BasicRouteDetail {routeCode = routeInfo.route.code, startStopCode = routeInfo.startStopCode, endStopCode = routeInfo.endStopCode}) routesInfo
      stations <- CallAPI.buildStations fareRouteDetails integratedBPPConfig
      let nonEmptyFareRouteDetails = NE.fromList fareRouteDetails
      (_, fares) <- CallAPI.getFares searchReq.riderId merchant merchantOperatingCity integratedBPPConfig nonEmptyFareRouteDetails vehicleType Nothing searchReq.multimodalSearchRequestId
      return $
        map
          ( \FRFSFare {..} ->
              let adultPrice = maybe (Price (Money 0) (HighPrecMoney 0.0) INR) (.price) (find (\category -> category.category == ADULT) categories)
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
                              routeColor = Nothing,
                              routeFarePolicyId = farePolicyId
                            }
                      )
                      routesInfo
               in DQuote
                    { bppItemId = CallAPI.getProviderName integratedBPPConfig,
                      routeCode = (NE.head nonEmptyFareRouteDetails).routeCode,
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
        { bppItemId = CallAPI.getProviderName integratedBPPConfig,
          offeredPrice = offeredPrice,
          price = price,
          categoryMeta =
            Just $
              DFRFSQuoteCategory.QuoteCategoryMetadata
                { code = code,
                  title = title,
                  description = description,
                  tnc = tnc
                },
          ..
        }

select :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> DFRFSQuote.FRFSQuote -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> m DOnSelect
select _merchant _merchantOperatingCity _integratedBPPConfig _bapConfig quote quoteCategories = do
  return $
    DOnSelect
      { providerId = quote.providerId,
        validTill = Just quote.validTill,
        fareBreakUp = [],
        transactionId = quote.searchId.getId,
        messageId = quote.id.getId,
        categories =
          mapMaybe
            ( \category -> do
                selectedQuantity <- category.selectedQuantity
                return $
                  DCategorySelect
                    { bppItemId = category.bppItemId,
                      quantity = selectedQuantity,
                      category = category.category,
                      price = category.offeredPrice
                    }
            )
            quoteCategories
      }

init :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> m DOnInit
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
        categories = mapMaybe mkDCategorySelect quoteCategories,
        fareBreakUp = [],
        validTill = validTill,
        transactionId = booking.searchId.getId,
        messageId = booking.id.getId,
        bankAccNum = bankAccountNumber,
        bankCode = bankCode,
        bppOrderId = bppOrderId
      }
  where
    mkDCategorySelect quoteCategory = do
      quantity <- quoteCategory.selectedQuantity
      return $
        DCategorySelect
          { bppItemId = CallAPI.getProviderName integratedBPPConfig,
            quantity = quantity,
            category = quoteCategory.category,
            price = quoteCategory.offeredPrice
          }
    mkPaymentDetails = \case
      Spec.BAP -> do
        let paymentParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
        paymentParams & fromMaybeM (InternalError "BknPaymentParams Not Found")
      Spec.BPP -> CallAPI.getPaymentDetails merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking

confirm :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c, Metrics.HasBAPMetrics m r) => Merchant -> MerchantOperatingCity -> FRFSConfig -> IntegratedBPPConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> Maybe Bool -> m DOrder
confirm _merchant _merchantOperatingCity frfsConfig integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking quoteCategories mbIsSingleMode = do
  let qrTtl =
        case booking.vehicleType of
          Spec.BUS -> if fromMaybe False mbIsSingleMode then frfsConfig.singleModeBusStationTtl else frfsConfig.busStationTtl
          Spec.METRO -> Seconds frfsConfig.metroStationTtl
          _ -> Seconds frfsConfig.metroStationTtl
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
                  commencingHours = ticket.commencingHours
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
                  commencingHours = ticket.commencingHours
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
