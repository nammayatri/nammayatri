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
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import Tools.Error

getFares :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id Person -> Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> NonEmpty CallAPI.BasicRouteDetail -> Spec.VehicleCategory -> m (Bool, [FRFSFare])
getFares riderId merchant merchantOperatingCity integratedBPPConfig _bapConfig fareRouteDetails vehicleCategory = do
  try @_ @SomeException (CallAPI.getFares riderId merchant merchantOperatingCity integratedBPPConfig fareRouteDetails vehicleCategory) >>= \case
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
          routesInfo <- getPossibleRoutesBetweenTwoStops startStationCode endStationCode integratedBPPConfig
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
      (_, fares) <- CallAPI.getFares searchReq.riderId merchant merchantOperatingCity integratedBPPConfig nonEmptyFareRouteDetails vehicleType
      return $
        map
          ( \FRFSFare {..} ->
              let routeStations =
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
                              routePrice = price,
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
                      discounts = map mkDDiscount discounts,
                      ..
                    }
          )
          fares

    mkDVehicleServiceTier FRFSVehicleServiceTier {..} = DVehicleServiceTier {..}

    mkDDiscount FRFSDiscount {..} = DDiscount {..}

select :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> DFRFSQuote.FRFSQuote -> Maybe Int -> Maybe Int -> m DOnSelect
select _merchant _merchantOperatingCity _integratedBPPConfig _bapConfig quote ticketQuantity childTicketQuantity = do
  void $ QFRFSQuote.updateTicketAndChildTicketQuantityById quote.id ticketQuantity childTicketQuantity
  return $
    DOnSelect
      { providerId = quote.providerId,
        totalPrice = quote.price,
        fareBreakUp = [],
        bppItemId = quote.bppItemId,
        validTill = Just quote.validTill,
        transactionId = quote.searchId.getId,
        messageId = quote.id.getId
      }

init :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOnInit
init merchant merchantOperatingCity integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking = do
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.initTTLSec
  paymentDetails <- mkPaymentDetails bapConfig.collectedBy
  bankAccountNumber <- paymentDetails.bankAccNumber & fromMaybeM (InternalError "Bank Account Number Not Found")
  bankCode <- paymentDetails.bankCode & fromMaybeM (InternalError "Bank Code Not Found")
  bppOrderId <- CallAPI.getBppOrderId integratedBPPConfig booking
  return $
    DOnInit
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.price,
        totalQuantity = booking.quantity,
        totalChildTicketQuantity = booking.childTicketQuantity,
        fareBreakUp = [],
        bppItemId = CallAPI.getProviderName integratedBPPConfig,
        validTill = validTill,
        transactionId = booking.searchId.getId,
        messageId = booking.id.getId,
        bankAccNum = bankAccountNumber,
        bankCode = bankCode,
        bppOrderId = bppOrderId
      }
  where
    mkPaymentDetails = \case
      Spec.BAP -> do
        let paymentParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
        paymentParams & fromMaybeM (InternalError "BknPaymentParams Not Found")
      Spec.BPP -> CallAPI.getPaymentDetails merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking

confirm :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => Merchant -> MerchantOperatingCity -> FRFSConfig -> IntegratedBPPConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
confirm _merchant _merchantOperatingCity frfsConfig integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking = do
  let qrTtl =
        case booking.vehicleType of
          Spec.BUS -> frfsConfig.busStationTtl
          Spec.METRO -> Seconds frfsConfig.metroStationTtl
          _ -> Seconds frfsConfig.metroStationTtl
  order <- CallAPI.createOrder integratedBPPConfig qrTtl (mRiderName, mRiderNumber) booking
  let tickets =
        map
          ( \ticket ->
              DTicket
                { qrData = ticket.qrData,
                  vehicleNumber = ticket.vehicleNumber,
                  bppFulfillmentId = CallAPI.getProviderName integratedBPPConfig,
                  ticketNumber = ticket.ticketNumber,
                  validTill = ticket.qrValidity,
                  status = ticket.qrStatus,
                  description = ticket.description,
                  qrRefreshAt = ticket.qrRefreshAt
                }
          )
          order.tickets
  return $
    DOrder
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.price.amount,
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
                  bppFulfillmentId = CallAPI.getProviderName integratedBPPConfig,
                  ticketNumber = ticket.ticketNumber,
                  validTill = ticket.qrValidity,
                  status = ticket.qrStatus,
                  qrRefreshAt = ticket.qrRefreshAt,
                  description = ticket.description
                }
          )
          tickets'
  return $
    DOrder
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.price.amount,
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
