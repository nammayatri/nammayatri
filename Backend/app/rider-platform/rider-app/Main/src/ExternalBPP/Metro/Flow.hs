module ExternalBPP.Metro.Flow where

import qualified BecknV2.FRFS.Enums as Spec
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
import Domain.Types.Station
import ExternalBPP.Metro.ExternalAPI.CallAPI as CallAPI
import Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.FRFSTicket as QFRFSTicket
import Storage.Queries.Station as QStation
import Tools.Error

search :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Merchant -> MerchantOperatingCity -> BecknConfig -> DFRFSSearch.FRFSSearch -> m DOnSearch
search _merchant _merchantOperatingCity bapConfig searchReq = do
  fromStation <- QStation.findById searchReq.fromStationId >>= fromMaybeM (StationNotFound searchReq.fromStationId.getId)
  toStation <- QStation.findById searchReq.toStationId >>= fromMaybeM (StationNotFound searchReq.toStationId.getId)
  let stations = mkStations fromStation toStation
  let routeStations = []
  quote <- mkQuote routeStations stations searchReq.vehicleType
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.searchTTLSec
  messageId <- generateGUID
  return $
    DOnSearch
      { bppSubscriberId = bapConfig.subscriberId,
        bppSubscriberUrl = showBaseUrl bapConfig.subscriberUrl,
        providerDescription = Nothing,
        providerId = bapConfig.uniqueKeyId,
        providerName = "Metro",
        quotes = [quote],
        validTill = validTill,
        transactionId = searchReq.id.getId,
        messageId = messageId,
        bppDelayedInterest = Nothing
      }
  where
    mkStations :: Station -> Station -> [DStation]
    mkStations fromStation toStation = do
      let startStation = DStation fromStation.code fromStation.name fromStation.lat fromStation.lon START Nothing Nothing
          endStation = DStation toStation.code toStation.name toStation.lat toStation.lon END Nothing Nothing
      [startStation] ++ [endStation]

    mkQuote routeStations stations vehicleType = do
      let amount = HighPrecMoney 20.0 -- call external api
      let price =
            Price
              { amountInt = round amount,
                amount = amount,
                currency = INR -- fix later
              }
      return $
        DQuote
          { bppItemId = "Metro",
            _type = DFRFSQuote.SingleJourney,
            serviceTierType = Nothing,
            serviceTierProviderCode = Nothing,
            serviceTierShortName = Nothing,
            serviceTierDescription = Nothing,
            serviceTierLongName = Nothing,
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
        bppItemId = "Metro",
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
  bppOrderId <- CallAPI.getOrderId config booking
  tickets <- mkTickets bppOrderId
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
  where
    mkTickets bppOrderId = do
      ticketsData <- CallAPI.generateQRByProvider config bppOrderId frfsConfig.busStationTtl booking
      ticketsData `forM` \(qrData, qrStatus, qrValidity, ticketNumber) -> do
        return $
          DTicket
            { qrData = qrData,
              bppFulfillmentId = "Metro Fulfillment",
              ticketNumber = ticketNumber,
              validTill = qrValidity,
              status = qrStatus
            }

status :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
status _merchantId _merchantOperatingCity config bapConfig booking = do
  tickets' <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  bppOrderId <- CallAPI.getOrderId config booking
  ticketStatus <-
    if any (\ticket -> ticket.status == DFRFSTicket.ACTIVE) tickets'
      then CallAPI.getTicketStatus config booking bppOrderId
      else return "UNCLAIMED"
  let tickets =
        map
          ( \DFRFSTicket.FRFSTicket {status = _status, ..} ->
              DTicket
                { bppFulfillmentId = "Metro Fulfillment",
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
        bppItemId = "Metro Item",
        transactionId = booking.searchId.getId,
        orderStatus = Nothing,
        messageId = booking.id.getId,
        tickets = tickets
      }
