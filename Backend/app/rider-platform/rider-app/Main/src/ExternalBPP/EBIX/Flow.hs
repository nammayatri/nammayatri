module ExternalBPP.EBIX.Flow where

import qualified API.Types.UI.FRFSTicketService as API
import Data.List (sortOn)
import Domain.Action.Beckn.FRFS.Common
import Domain.Action.Beckn.FRFS.OnInit
import Domain.Action.Beckn.FRFS.OnSearch
import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSConfig
import Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.RouteStopMapping
import Domain.Types.Station
import ExternalBPP.EBIX.ExternalAPI.Order as EBIXOrder
import ExternalBPP.EBIX.ExternalAPI.Payment as EBIXPayment
import ExternalBPP.EBIX.ExternalAPI.Verification as EBIXVerification
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Utils.Common
import Storage.Queries.Route as QRoute
import Storage.Queries.RouteStopFare as QRouteStopFare
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Storage.Queries.Station as QStation
import Tools.Error

search :: (CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Merchant -> MerchantOperatingCity -> BecknConfig -> DFRFSSearch.FRFSSearch -> m DOnSearch
search _merchant _merchantOperatingCity bapConfig searchReq = do
  fromStation <- QStation.findById searchReq.fromStationId >>= fromMaybeM (StationNotFound searchReq.fromStationId.getId)
  toStation <- QStation.findById searchReq.toStationId >>= fromMaybeM (StationNotFound searchReq.toStationId.getId)
  routeId <- searchReq.routeId & fromMaybeM (InternalError "Route Id is not present in Search Request")
  route <- QRoute.findByRouteId routeId >>= fromMaybeM (RouteNotFound routeId.getId)
  stops <- QRouteStopMapping.findByRouteCode route.code
  stations <- mkStations fromStation toStation stops & fromMaybeM (StationsNotFound fromStation.id.getId toStation.id.getId)
  quotes <- mkQuote stations searchReq.vehicleType route.code fromStation.code toStation.code
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
        messageId = messageId
      }
  where
    mkStations :: Station -> Station -> [RouteStopMapping] -> Maybe [DStation]
    mkStations fromStation toStation stops =
      ((,) <$> find (\stop -> stop.stopCode == fromStation.code) stops <*> find (\stop -> stop.stopCode == toStation.code) stops)
        <&> \(startStop, endStop) ->
          do
            let startStation = DStation startStop.stopCode startStop.stopName (Just startStop.stopPoint.lat) (Just startStop.stopPoint.lon) API.START (Just startStop.sequenceNum)
                endStation = DStation endStop.stopCode endStop.stopName (Just endStop.stopPoint.lat) (Just endStop.stopPoint.lon) API.END (Just endStop.sequenceNum)
                intermediateStations =
                  (sortOn (.sequenceNum) $ filter (\stop -> stop.sequenceNum > startStop.sequenceNum && stop.sequenceNum < endStop.sequenceNum) stops)
                    <&> (\stop -> DStation stop.stopCode stop.stopName (Just stop.stopPoint.lat) (Just stop.stopPoint.lon) API.INTERMEDIATE (Just stop.sequenceNum))
            [startStation] ++ intermediateStations ++ [endStation]

    mkQuote stations vehicleType routeCode startStopCode endStopCode = do
      farePolicy <- QRouteStopFare.findByRouteStartAndStopCode routeCode startStopCode endStopCode >>= fromMaybeM (RouteFareNotFound routeCode startStopCode endStopCode)
      let price =
            Price
              { amountInt = round farePolicy.amount,
                amount = farePolicy.amount,
                currency = farePolicy.currency
              }
      return $
        [ DQuote
            { bppItemId = "Kolkata Buses Item",
              _type = DFRFSQuote.SingleJourney,
              routeCode = Just routeCode,
              ..
            }
        ]

init :: (CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOnInit
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
      BAP -> do
        let paymentParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
        paymentParams & fromMaybeM (InternalError "BknPaymentParams Not Found")
      BPP -> EBIXPayment.getPaymentDetails merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking

confirm :: (CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Merchant -> MerchantOperatingCity -> FRFSConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
confirm merchant merchantOperatingCity frfsConfig bapConfig (mRiderName, mRiderNumber) booking = do
  bppOrderId <- EBIXOrder.getOrderId merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking
  ticket <- mkTicket
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
    mkTicket = do
      (qrData, qrStatus, qrValidity, ticketNumber) <-
        case bapConfig.verifiedBy of
          BAP -> do
            qrValidity <- addUTCTime (secondsToNominalDiffTime frfsConfig.busStationTtl) <$> getCurrentTime
            let qrData = "I'm a Dummy QR Guys, will be replaced soon!"
                qrStatus = "UNCLAIMED" -- "UNCLAIMED" | "CLAIMED"
                ticketNumber = "#007-24042024-001"
            return (qrData, qrStatus, qrValidity, ticketNumber)
          BPP -> EBIXVerification.getVerificationDetails merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking
      return $
        DTicket
          { qrData = qrData,
            bppFulfillmentId = "Kolkata Buses Fulfillment",
            ticketNumber = ticketNumber,
            validTill = qrValidity,
            status = qrStatus
          }
