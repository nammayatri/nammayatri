module ExternalBPP.Bus.Flow where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (sortOn)
import Domain.Action.Beckn.FRFS.Common
import Domain.Action.Beckn.FRFS.OnInit
import Domain.Action.Beckn.FRFS.OnSearch
import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSConfig
import qualified Domain.Types.FRFSFarePolicy as DFRFSFarePolicy
import Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketDiscount as DFRFSTicketDiscount
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.RouteStopMapping
import Domain.Types.Station
import Domain.Types.StationType
import ExternalBPP.Bus.ExternalAPI.CallAPI as CallAPI
import ExternalBPP.Bus.ExternalAPI.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common
import SharedLogic.FRFSUtils
import Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import Storage.Queries.FRFSRouteStopStageFare as QFRFSRouteStopStageFare
import Storage.Queries.FRFSStageFare as QFRFSStageFare
import Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import Storage.Queries.Route as QRoute
import Storage.Queries.RouteStopFare as QRouteStopFare
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Storage.Queries.Station as QStation
import Tools.Error

search :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Merchant -> MerchantOperatingCity -> BecknConfig -> DFRFSSearch.FRFSSearch -> m DOnSearch
search merchant merchantOperatingCity bapConfig searchReq = do
  fromStation <- QStation.findById searchReq.fromStationId >>= fromMaybeM (StationNotFound searchReq.fromStationId.getId)
  toStation <- QStation.findById searchReq.toStationId >>= fromMaybeM (StationNotFound searchReq.toStationId.getId)
  quotes <- do
    case searchReq.routeId of
      Just routeId -> do
        route <- QRoute.findByRouteId routeId >>= fromMaybeM (RouteNotFound routeId.getId)
        let routeInfo =
              RouteStopInfo
                { route,
                  totalStops = Nothing,
                  stops = Nothing,
                  startStopCode = fromStation.code,
                  endStopCode = toStation.code,
                  travelTime = Nothing
                }
        mkSingleRouteQuote searchReq.vehicleType routeInfo merchantOperatingCity.id
      Nothing -> do
        transitRoutesInfo <- getPossibleTransitRoutesBetweenTwoStops fromStation.code toStation.code
        if null transitRoutesInfo
          then do
            routesInfo <- getPossibleRoutesBetweenTwoStops fromStation.code toStation.code
            quotes' <-
              mapM
                ( \routeInfo -> do
                    mkSingleRouteQuote searchReq.vehicleType routeInfo merchantOperatingCity.id
                )
                routesInfo
            return $ concat quotes'
          else do
            quotes' <-
              mapM
                ( \transitRouteInfo -> do
                    mkTransitRoutesQuote searchReq.vehicleType transitRouteInfo merchantOperatingCity.id
                )
                transitRoutesInfo
            return $ concat quotes'
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.searchTTLSec
  messageId <- generateGUID
  return $
    DOnSearch
      { bppSubscriberId = bapConfig.subscriberId,
        bppSubscriberUrl = showBaseUrl bapConfig.subscriberUrl,
        providerDescription = Nothing,
        providerId = bapConfig.uniqueKeyId,
        providerName = "Buses",
        quotes = quotes,
        validTill = validTill,
        transactionId = searchReq.id.getId,
        messageId = messageId,
        bppDelayedInterest = Nothing
      }
  where
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

    mkSingleRouteQuote :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Spec.VehicleCategory -> RouteStopInfo -> Id MerchantOperatingCity -> m [DQuote]
    mkSingleRouteQuote vehicleType routeInfo merchantOperatingCityId = do
      stops <- QRouteStopMapping.findByRouteCode routeInfo.route.code
      startStation <- QStation.findByStationCodeAndMerchantOperatingCityId routeInfo.startStopCode merchantOperatingCityId >>= fromMaybeM (StationNotFound $ routeInfo.startStopCode <> " for merchantOperatingCityId: " <> merchantOperatingCityId.getId)
      endStation <- QStation.findByStationCodeAndMerchantOperatingCityId routeInfo.endStopCode merchantOperatingCityId >>= fromMaybeM (StationNotFound $ routeInfo.endStopCode <> " for merchantOperatingCityId: " <> merchantOperatingCityId.getId)
      stations <- mkStations startStation endStation stops & fromMaybeM (StationsNotFound startStation.id.getId endStation.id.getId)

      currentTime <- getCurrentTime
      fareProducts <- QFRFSRouteFareProduct.findByRouteCode routeInfo.route.code merchant.id merchantOperatingCity.id
      let serviceableFareProducts = DTB.findBoundedDomain fareProducts currentTime ++ filter (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded) fareProducts
      mapM
        ( \fareProduct -> do
            vehicleServiceTier <- QFRFSVehicleServiceTier.findById fareProduct.vehicleServiceTierId >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> fareProduct.vehicleServiceTierId.getId)
            farePolicy <- QFRFSFarePolicy.findById fareProduct.farePolicyId >>= fromMaybeM (InternalError $ "FRFS Fare Policy Not Found : " <> fareProduct.farePolicyId.getId)
            price <-
              case farePolicy._type of
                DFRFSFarePolicy.MatrixBased -> do
                  routeStopFare <- QRouteStopFare.findByRouteStartAndStopCode farePolicy.id routeInfo.route.code routeInfo.startStopCode routeInfo.endStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Fare Not Found")
                  return $
                    Price
                      { amountInt = round routeStopFare.amount,
                        amount = routeStopFare.amount,
                        currency = routeStopFare.currency
                      }
                DFRFSFarePolicy.StageBased -> do
                  stageFares <- QFRFSStageFare.findAllByFarePolicyId farePolicy.id
                  startStageFare <- QFRFSRouteStopStageFare.findByRouteAndStopCode farePolicy.id routeInfo.route.code routeInfo.startStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Stage Fare Not Found")
                  endStageFare <- QFRFSRouteStopStageFare.findByRouteAndStopCode farePolicy.id routeInfo.route.code routeInfo.endStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Stage Fare Not Found")
                  let stage = endStageFare.stage - startStageFare.stage
                  stageFare <- find (\stageFare -> stageFare.stage == stage) stageFares & fromMaybeM (InternalError "FRFS Stage Fare Not Found")
                  return $
                    Price
                      { amountInt = round stageFare.amount,
                        amount = stageFare.amount,
                        currency = stageFare.currency
                      }
            discountsWithEligibility <- getFRFSTicketDiscountWithEligibility merchant.id merchantOperatingCity.id vehicleType searchReq.riderId farePolicy.applicableDiscountIds
            let routeServiceTier =
                  DVehicleServiceTier
                    { serviceTierType = vehicleServiceTier._type,
                      serviceTierProviderCode = vehicleServiceTier.providerCode,
                      serviceTierShortName = vehicleServiceTier.shortName,
                      serviceTierDescription = vehicleServiceTier.description,
                      serviceTierLongName = vehicleServiceTier.longName
                    }
                routeStations =
                  [ DRouteStation
                      { routeCode = routeInfo.route.code,
                        routeLongName = routeInfo.route.longName,
                        routeShortName = routeInfo.route.shortName,
                        routeStartPoint = routeInfo.route.startPoint,
                        routeEndPoint = routeInfo.route.endPoint,
                        routeStations = stations,
                        routeTravelTime = routeInfo.travelTime,
                        routeServiceTier = Just routeServiceTier,
                        routePrice = price,
                        routeSequenceNum = Nothing,
                        routeColor = Nothing
                      }
                  ]
            return $
              DQuote
                { bppItemId = "Buses Item - " <> show vehicleServiceTier._type <> " - " <> vehicleServiceTier.providerCode,
                  _type = DFRFSQuote.SingleJourney,
                  routeStations = routeStations,
                  discounts = map (mkDiscount price) discountsWithEligibility,
                  ..
                }
        )
        serviceableFareProducts

    mkTransitRoutesQuote :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Spec.VehicleCategory -> [RouteStopInfo] -> Id MerchantOperatingCity -> m [DQuote]
    mkTransitRoutesQuote vehicleType transitRouteInfo merchantOperatingCityId = do
      -- TODO :: This is to be handled properly with OTP
      routeStations <-
        mapM
          ( \routeInfo -> do
              stops <- QRouteStopMapping.findByRouteCode routeInfo.route.code
              startStation <- QStation.findByStationCodeAndMerchantOperatingCityId routeInfo.startStopCode merchantOperatingCityId >>= fromMaybeM (StationNotFound routeInfo.startStopCode)
              endStation <- QStation.findByStationCodeAndMerchantOperatingCityId routeInfo.endStopCode merchantOperatingCityId >>= fromMaybeM (StationNotFound routeInfo.endStopCode)
              stations <- mkStations startStation endStation stops & fromMaybeM (StationsNotFound startStation.id.getId endStation.id.getId)

              currentTime <- getCurrentTime
              fareProducts <- QFRFSRouteFareProduct.findByRouteCode routeInfo.route.code merchant.id merchantOperatingCity.id
              let serviceableFareProducts = DTB.findBoundedDomain fareProducts currentTime ++ filter (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded) fareProducts
              fareProduct <- listToMaybe serviceableFareProducts & fromMaybeM (InternalError "Fare Product Not Found")
              vehicleServiceTier <- QFRFSVehicleServiceTier.findById fareProduct.vehicleServiceTierId >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> fareProduct.vehicleServiceTierId.getId)
              farePolicy <- QFRFSFarePolicy.findById fareProduct.farePolicyId >>= fromMaybeM (InternalError $ "FRFS Fare Policy Not Found : " <> fareProduct.farePolicyId.getId)
              price <-
                case farePolicy._type of
                  DFRFSFarePolicy.MatrixBased -> do
                    routeStopFare <- QRouteStopFare.findByRouteStartAndStopCode farePolicy.id routeInfo.route.code routeInfo.startStopCode routeInfo.endStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Fare Not Found")
                    return $
                      Price
                        { amountInt = round routeStopFare.amount,
                          amount = routeStopFare.amount,
                          currency = routeStopFare.currency
                        }
                  DFRFSFarePolicy.StageBased -> do
                    stageFares <- QFRFSStageFare.findAllByFarePolicyId farePolicy.id
                    startStageFare <- QFRFSRouteStopStageFare.findByRouteAndStopCode farePolicy.id routeInfo.route.code routeInfo.startStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Stage Fare Not Found")
                    endStageFare <- QFRFSRouteStopStageFare.findByRouteAndStopCode farePolicy.id routeInfo.route.code routeInfo.endStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Stage Fare Not Found")
                    let stage = endStageFare.stage - startStageFare.stage
                    stageFare <- find (\stageFare -> stageFare.stage == stage) stageFares & fromMaybeM (InternalError "FRFS Stage Fare Not Found")
                    return $
                      Price
                        { amountInt = round stageFare.amount,
                          amount = stageFare.amount,
                          currency = stageFare.currency
                        }
              let routeServiceTier =
                    DVehicleServiceTier
                      { serviceTierType = vehicleServiceTier._type,
                        serviceTierProviderCode = vehicleServiceTier.providerCode,
                        serviceTierShortName = vehicleServiceTier.shortName,
                        serviceTierDescription = vehicleServiceTier.description,
                        serviceTierLongName = vehicleServiceTier.longName
                      }

              return $
                DRouteStation
                  { routeCode = routeInfo.route.code,
                    routeLongName = routeInfo.route.longName,
                    routeShortName = routeInfo.route.shortName,
                    routeStartPoint = routeInfo.route.startPoint,
                    routeEndPoint = routeInfo.route.endPoint,
                    routeStations = stations,
                    routeTravelTime = routeInfo.travelTime,
                    routeServiceTier = Just routeServiceTier,
                    routePrice = price,
                    routeSequenceNum = Nothing,
                    routeColor = Nothing
                  }
          )
          transitRouteInfo

      let stations' =
            concatMap
              (\routeStation -> map (\station -> station {stationType = INTERMEDIATE}) routeStation.routeStations)
              routeStations

          stations =
            mapWithIndex
              ( \idx DStation {..} ->
                  if idx == 0
                    then DStation {stationType = START, ..}
                    else
                      if idx == length stations' - 1
                        then DStation {stationType = END, ..}
                        else DStation {..}
              )
              stations'

      let totalPrice =
            foldr
              (\routeStation price -> price + routeStation.routePrice.amount)
              (HighPrecMoney 0.0)
              routeStations

      return $
        [ DQuote
            { bppItemId = "Buses Item",
              _type = DFRFSQuote.SingleJourney,
              routeStations = routeStations,
              discounts = [], -- TODO :: Discounts not handled
              price =
                Price
                  { amount = totalPrice,
                    amountInt = round totalPrice,
                    currency = INR
                  },
              ..
            }
        ]
      where
        mapWithIndex f xs = zipWith f [0 ..] xs

    mkDiscount price (discount, eligibility) =
      let discountPrice =
            case discount.value of
              DFRFSTicketDiscount.FixedAmount amount ->
                Price
                  { amountInt = round amount,
                    amount = amount,
                    currency = discount.currency
                  }
              DFRFSTicketDiscount.Percentage percent ->
                Price
                  { amountInt = round ((HighPrecMoney (toRational percent) * price.amount) / 100),
                    amount = (HighPrecMoney (toRational percent) * price.amount) / 100,
                    currency = discount.currency
                  }
       in DDiscount
            { code = discount.code,
              title = discount.title,
              description = discount.description,
              tnc = discount.tnc,
              price = discountPrice,
              ..
            }

init :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOnInit
init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking = do
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.initTTLSec
  paymentDetails <- mkPaymentDetails bapConfig.collectedBy
  bankAccountNumber <- paymentDetails.bankAccNumber & fromMaybeM (InternalError "Bank Account Number Not Found")
  bankCode <- paymentDetails.bankCode & fromMaybeM (InternalError "Bank Code Not Found")
  return $
    DOnInit
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.price,
        fareBreakUp = [],
        bppItemId = "Buses Item",
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
confirm _merchant _merchantOperatingCity frfsConfig config bapConfig (_mRiderName, _mRiderNumber) booking = do
  order <- CallAPI.createOrder config frfsConfig.busStationTtl booking
  let tickets =
        map
          ( \ticket ->
              DTicket
                { qrData = ticket.qrData,
                  bppFulfillmentId = "Buses Fulfillment",
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
        bppItemId = "Buses Item",
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
                  bppFulfillmentId = "Buses Fulfillment",
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
        bppItemId = "Buses Item",
        transactionId = booking.searchId.getId,
        orderStatus = Nothing,
        messageId = booking.id.getId,
        tickets = tickets
      }

verifyTicket :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> Text -> m DTicketPayload
verifyTicket _merchantId _merchantOperatingCity config _bapConfig encryptedQrData = do
  TicketPayload {..} <- CallAPI.verifyTicket config encryptedQrData
  return DTicketPayload {..}
