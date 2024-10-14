module ExternalBPP.Bus.Flow where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (sortOn)
import Domain.Action.Beckn.FRFS.Common
import Domain.Action.Beckn.FRFS.OnInit
import Domain.Action.Beckn.FRFS.OnSearch
import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSConfig
import Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.RouteStopMapping
import Domain.Types.Station
import Domain.Types.StationType
import ExternalBPP.Bus.ExternalAPI.CallAPI as CallAPI
import Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common
import Storage.Queries.FRFSTicket as QFRFSTicket
import Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import Storage.Queries.Route as QRoute
import Storage.Queries.RouteStopFare as QRouteStopFare
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Storage.Queries.Station as QStation
import Tools.Error

search :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Merchant -> MerchantOperatingCity -> BecknConfig -> DFRFSSearch.FRFSSearch -> m DOnSearch
search _merchant _merchantOperatingCity bapConfig searchReq = do
  fromStation <- QStation.findById searchReq.fromStationId >>= fromMaybeM (StationNotFound searchReq.fromStationId.getId)
  toStation <- QStation.findById searchReq.toStationId >>= fromMaybeM (StationNotFound searchReq.toStationId.getId)
  routeId <- searchReq.routeId & fromMaybeM (InternalError "Route Id is not present in Search Request")
  route <- QRoute.findByRouteId routeId >>= fromMaybeM (RouteNotFound routeId.getId)
  stops <- QRouteStopMapping.findByRouteCode route.code
  stations <- mkStations fromStation toStation stops & fromMaybeM (StationsNotFound fromStation.id.getId toStation.id.getId)
  let routeStations =
        [ DRouteStation
            { routeCode = route.code,
              routeLongName = route.longName,
              routeShortName = route.shortName,
              routeStartPoint = route.startPoint,
              routeEndPoint = route.endPoint,
              routeStations = stations,
              routeSequenceNum = Nothing,
              routeColor = Nothing
            }
        ]
  quotes <- mkQuote routeStations stations searchReq.vehicleType route.code fromStation.code toStation.code
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.searchTTLSec
  messageId <- generateGUID
  return $
    DOnSearch
      { bppSubscriberId = bapConfig.subscriberId,
        bppSubscriberUrl = showBaseUrl bapConfig.subscriberUrl,
        providerDescription = Nothing,
        providerId = bapConfig.uniqueKeyId,
        providerName = "Kolkata Buses",
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

    mkQuote routeStations stations vehicleType routeCode startStopCode endStopCode = do
      currentTime <- getCurrentTime
      farePolicies <- QRouteStopFare.findByRouteStartAndStopCode routeCode startStopCode endStopCode
      let serviceableFarePolicies = DTB.findBoundedDomain farePolicies currentTime ++ filter (\farePolicy -> farePolicy.timeBounds == DTB.Unbounded) farePolicies
      mapM
        ( \farePolicy -> do
            vehicleServiceTier <- QFRFSVehicleServiceTier.findById farePolicy.vehicleServiceTierId >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> farePolicy.vehicleServiceTierId.getId)
            let price =
                  Price
                    { amountInt = round farePolicy.amount,
                      amount = farePolicy.amount,
                      currency = farePolicy.currency
                    }
            return $
              DQuote
                { bppItemId = "Kolkata Buses Item - " <> show vehicleServiceTier._type <> " - " <> vehicleServiceTier.providerCode,
                  _type = DFRFSQuote.SingleJourney,
                  routeStations = routeStations,
                  serviceTierType = Just vehicleServiceTier._type,
                  serviceTierProviderCode = Just vehicleServiceTier.providerCode,
                  serviceTierShortName = Just vehicleServiceTier.shortName,
                  serviceTierDescription = Just vehicleServiceTier.description,
                  serviceTierLongName = Just vehicleServiceTier.longName,
                  ..
                }
        )
        serviceableFarePolicies

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
        bppItemId = "Kolkata Buses Item",
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
  (bppOrderId, ticketNum) <- CallAPI.getOrderId config booking
  ticket <- mkTicket bppOrderId ticketNum
  return $
    DOrder
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.price.amount,
        fareBreakUp = [],
        bppOrderId = bppOrderId,
        bppItemId = "Kolkata Buses Item",
        transactionId = booking.searchId.getId,
        orderStatus = Nothing,
        messageId = booking.id.getId,
        tickets = [ticket]
      }
  where
    mkTicket bppOrderId ticketNum = do
      (qrData, qrStatus, qrValidity, ticketNumber) <- CallAPI.generateQRByProvider config bppOrderId ticketNum frfsConfig.busStationTtl booking
      return $
        DTicket
          { qrData = qrData,
            bppFulfillmentId = "Kolkata Buses Fulfillment",
            ticketNumber = ticketNumber,
            validTill = qrValidity,
            status = qrStatus
          }

status :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
status _merchantId _merchantOperatingCity config bapConfig booking = do
  tickets' <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  (bppOrderId, _) <- CallAPI.getOrderId config booking
  ticketStatus <-
    if any (\ticket -> ticket.status == DFRFSTicket.ACTIVE) tickets'
      then CallAPI.getTicketStatus config booking bppOrderId
      else return "UNCLAIMED"
  let tickets =
        map
          ( \DFRFSTicket.FRFSTicket {status = _status, ..} ->
              DTicket
                { bppFulfillmentId = "Kolkata Buses Fulfillment",
                  status = ticketStatus,
                  ..
                }
          )
          tickets'
  return $
    DOrder
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.price.amount,
        fareBreakUp = [],
        bppOrderId = bppOrderId,
        bppItemId = "Kolkata Buses Item",
        transactionId = booking.searchId.getId,
        orderStatus = Nothing,
        messageId = booking.id.getId,
        tickets = tickets
      }
