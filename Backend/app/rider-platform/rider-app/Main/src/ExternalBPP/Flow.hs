module ExternalBPP.Flow where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (intersect, nubBy, sortOn)
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
import Domain.Types.RouteStopMapping
import Domain.Types.Station
import Domain.Types.StationType
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import ExternalBPP.ExternalAPI.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FRFSUtils
import Storage.Queries.Route as QRoute
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Storage.Queries.Station as QStation
import Tools.Error

getFares :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Maybe (Id Person) -> Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> Text -> Text -> Text -> Spec.VehicleCategory -> m [FRFSFare]
getFares riderId merchant merchantOperatingCity config _bapConfig routeCode startStationCode endStationCode vehicleCategory = CallAPI.getFares riderId merchant merchantOperatingCity config routeCode startStationCode endStationCode vehicleCategory

search :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> DFRFSSearch.FRFSSearch -> [FRFSRouteDetails] -> m DOnSearch
search merchant merchantOperatingCity config bapConfig searchReq routeDetails = do
  quotes <- buildQuotes routeDetails
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.searchTTLSec
  messageId <- generateGUID
  return $
    DOnSearch
      { bppSubscriberId = bapConfig.subscriberId,
        bppSubscriberUrl = showBaseUrl bapConfig.subscriberUrl,
        providerDescription = Nothing,
        providerId = bapConfig.uniqueKeyId,
        providerName = CallAPI.getProviderName config,
        quotes = quotes,
        validTill = validTill,
        transactionId = searchReq.id.getId,
        messageId = messageId,
        bppDelayedInterest = Nothing
      }
  where
    -- Build Multiple Service Tier Quotes For Non Transit Routes
    buildQuotes [routeDetail] = buildMultipleNonTransitRouteQuotes routeDetail
    -- Build Single Quote For Transit Routes
    buildQuotes routesDetail@(_ : _) =
      buildSingleTransitRouteQuote routesDetail
        >>= \case
          Just quote -> return [quote]
          Nothing -> return []
    buildQuotes [] = return []

    buildMultipleNonTransitRouteQuotes FRFSRouteDetails {..} = do
      case routeCode of
        Just routeCode' -> do
          route <- QRoute.findByRouteCode routeCode' >>= fromMaybeM (RouteNotFound routeCode')
          let routeInfo =
                RouteStopInfo
                  { route,
                    totalStops = Nothing,
                    stops = Nothing,
                    startStopCode = startStationCode,
                    endStopCode = endStationCode,
                    travelTime = Nothing
                  }
          mkSingleRouteQuote searchReq.vehicleType routeInfo merchantOperatingCity.id
        Nothing -> do
          routesInfo <- getPossibleRoutesBetweenTwoStops startStationCode endStationCode
          quotes <-
            mapM
              ( \routeInfo -> do
                  mkSingleRouteQuote searchReq.vehicleType routeInfo merchantOperatingCity.id
              )
              routesInfo
          return $ concat quotes

    buildSingleTransitRouteQuote routesDetail = do
      routeStationsAndDiscounts' <-
        mapWithIndexM
          ( \idx FRFSRouteDetails {..} -> do
              case routeCode of
                Just routeCode' -> do
                  fares <- CallAPI.getFares (Just searchReq.riderId) merchant merchantOperatingCity config routeCode' startStationCode endStationCode searchReq.vehicleType
                  case listToMaybe fares of
                    Just fare -> do
                      fromStation <- QStation.findByStationCodeAndMerchantOperatingCityId startStationCode merchantOperatingCity.id >>= fromMaybeM (StationNotFound startStationCode)
                      toStation <- QStation.findByStationCodeAndMerchantOperatingCityId endStationCode merchantOperatingCity.id >>= fromMaybeM (StationNotFound endStationCode)
                      route <- QRoute.findByRouteCode routeCode' >>= fromMaybeM (RouteNotFound routeCode')
                      stops <- QRouteStopMapping.findByRouteCode route.code
                      stations <- mkStations fromStation toStation stops & fromMaybeM (StationsNotFound fromStation.id.getId toStation.id.getId)
                      return $
                        Just $
                          ( fare.discounts,
                            DRouteStation
                              { routeCode = route.code,
                                routeLongName = route.longName,
                                routeShortName = route.shortName,
                                routeStartPoint = route.startPoint,
                                routeEndPoint = route.endPoint,
                                routeStations = stations,
                                routeTravelTime = Nothing,
                                routeServiceTier = Just $ mkDVehicleServiceTier fare.vehicleServiceTier,
                                routePrice = fare.price,
                                routeSequenceNum = Just $ idx + 1,
                                routeColor = Nothing
                              }
                          )
                    Nothing -> return Nothing
                Nothing -> return Nothing
          )
          routesDetail
      let routeStationsAndDiscounts = catMaybes routeStationsAndDiscounts'
      if length routeStationsAndDiscounts /= length routesDetail
        then return Nothing
        else do
          let routeStations = snd <$> routeStationsAndDiscounts
              discounts = findCommonDiscountsByCode (fst <$> routeStationsAndDiscounts)
              stations =
                case getFirstAndLastStation routeStations of
                  Just (startStation, endStation) -> [startStation] ++ [endStation]
                  Nothing -> []
          return $
            Just $
              DQuote
                { bppItemId = CallAPI.getProviderName config,
                  _type = DFRFSQuote.SingleJourney,
                  routeStations = routeStations,
                  stations,
                  discounts = map mkDDiscount discounts,
                  vehicleType = searchReq.vehicleType,
                  price =
                    Price
                      { amount = sum $ map ((.amount) . (.routePrice)) routeStations,
                        amountInt = sum $ map ((.amountInt) . (.routePrice)) routeStations,
                        currency = INR
                      }
                }

    findCommonDiscountsByCode :: [[FRFSDiscount]] -> [FRFSDiscount]
    findCommonDiscountsByCode discounts =
      let commonDiscountCodes = foldr intersect (map (.code) $ concat discounts) (map (map (.code)) discounts)
       in nubBy (\a b -> a.code == b.code) $ filter (\discount -> discount.code `elem` commonDiscountCodes) (concat discounts)

    mkStations :: Station -> Station -> [RouteStopMapping] -> Maybe [DStation]
    mkStations fromStation toStation stops =
      ((,) <$> find (\stop -> stop.stopCode == fromStation.code) stops <*> find (\stop -> stop.stopCode == toStation.code) stops)
        <&> \(startStop, endStop) ->
          do
            let startStation = DStation startStop.stopCode startStop.stopName (Just startStop.stopPoint.lat) (Just startStop.stopPoint.lon) START (Just startStop.sequenceNum) Nothing
                endStation = DStation endStop.stopCode endStop.stopName (Just endStop.stopPoint.lat) (Just endStop.stopPoint.lon) END (Just endStop.sequenceNum) Nothing
                intermediateStations =
                  (sortOn (.sequenceNum) $ filter (\stop -> stop.sequenceNum > startStop.sequenceNum && stop.sequenceNum < endStop.sequenceNum) stops)
                    <&> (\stop -> DStation stop.stopCode stop.stopName (Just stop.stopPoint.lat) (Just stop.stopPoint.lon) INTERMEDIATE (Just stop.sequenceNum) Nothing)
            [startStation] ++ intermediateStations ++ [endStation]

    mkSingleRouteQuote :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Spec.VehicleCategory -> RouteStopInfo -> Id MerchantOperatingCity -> m [DQuote]
    mkSingleRouteQuote vehicleType routeInfo merchantOperatingCityId = do
      stops <- QRouteStopMapping.findByRouteCode routeInfo.route.code
      startStation <- QStation.findByStationCodeAndMerchantOperatingCityId routeInfo.startStopCode merchantOperatingCityId >>= fromMaybeM (StationNotFound $ routeInfo.startStopCode <> " for merchantOperatingCityId: " <> merchantOperatingCityId.getId)
      endStation <- QStation.findByStationCodeAndMerchantOperatingCityId routeInfo.endStopCode merchantOperatingCityId >>= fromMaybeM (StationNotFound $ routeInfo.endStopCode <> " for merchantOperatingCityId: " <> merchantOperatingCityId.getId)
      stations <- mkStations startStation endStation stops & fromMaybeM (StationsNotFound startStation.id.getId endStation.id.getId)
      fares <- return [] -- CallAPI.getFares (Just searchReq.riderId) merchant merchantOperatingCity config routeInfo.route.code startStation.code endStation.code vehicleType
      return $
        map
          ( \FRFSFare {..} ->
              let routeStations =
                    [ DRouteStation
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
                          routeColor = Nothing
                        }
                    ]
               in DQuote
                    { bppItemId = CallAPI.getProviderName config,
                      _type = DFRFSQuote.SingleJourney,
                      routeStations = routeStations,
                      discounts = map mkDDiscount discounts,
                      ..
                    }
          )
          fares

    mkDVehicleServiceTier FRFSVehicleServiceTier {..} = DVehicleServiceTier {..}

    mkDDiscount FRFSDiscount {..} = DDiscount {..}

    getFirstAndLastStation routeStations =
      (,) <$> (listToMaybe =<< ((.routeStations) <$> listToMaybe routeStations))
        <*> (listToMaybe =<< (reverse . (.routeStations) <$> listToMaybe (reverse routeStations)))

    mapWithIndexM f xs = zipWithM f [0 ..] xs

init :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOnInit
init merchant merchantOperatingCity config bapConfig (mRiderName, mRiderNumber) booking = do
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.initTTLSec
  paymentDetails <- mkPaymentDetails bapConfig.collectedBy
  bankAccountNumber <- paymentDetails.bankAccNumber & fromMaybeM (InternalError "Bank Account Number Not Found")
  bankCode <- paymentDetails.bankCode & fromMaybeM (InternalError "Bank Code Not Found")
  return $
    DOnInit
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.price,
        fareBreakUp = [],
        bppItemId = CallAPI.getProviderName config,
        validTill = validTill,
        transactionId = booking.searchId.getId,
        messageId = booking.id.getId,
        bankAccNum = bankAccountNumber,
        bankCode = bankCode
      }
  where
    mkPaymentDetails = \case
      Spec.BAP -> do
        let paymentParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
        paymentParams & fromMaybeM (InternalError "BknPaymentParams Not Found")
      Spec.BPP -> CallAPI.getPaymentDetails merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking

confirm :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Merchant -> MerchantOperatingCity -> FRFSConfig -> ProviderConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
confirm _merchant _merchantOperatingCity frfsConfig config bapConfig (mRiderName, mRiderNumber) booking = do
  order <- CallAPI.createOrder config frfsConfig.busStationTtl (mRiderName, mRiderNumber) booking
  let tickets =
        map
          ( \ticket ->
              DTicket
                { qrData = ticket.qrData,
                  bppFulfillmentId = CallAPI.getProviderName config,
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
        bppItemId = CallAPI.getProviderName config,
        transactionId = booking.searchId.getId,
        orderStatus = Nothing,
        messageId = booking.id.getId,
        tickets = tickets
      }

status :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
status _merchantId _merchantOperatingCity config bapConfig booking = do
  bppOrderId <- booking.bppOrderId & fromMaybeM (InternalError "BPP Order Id Not Found")
  tickets' <- CallAPI.getTicketStatus config booking
  let tickets =
        map
          ( \ticket ->
              DTicket
                { qrData = ticket.qrData,
                  bppFulfillmentId = CallAPI.getProviderName config,
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
        bppItemId = CallAPI.getProviderName config,
        transactionId = booking.searchId.getId,
        orderStatus = Nothing,
        messageId = booking.id.getId,
        tickets = tickets
      }

verifyTicket :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> Text -> m DTicketPayload
verifyTicket _merchantId _merchantOperatingCity config _bapConfig encryptedQrData = do
  TicketPayload {..} <- CallAPI.verifyTicket config encryptedQrData
  return DTicketPayload {..}
