module ExternalBPP.Metro.Flow where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.Text as T
import qualified Data.UUID as UU
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
import Domain.Types.StationType
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.FareByOriginDest as CallAPIFareByOriginDest
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.QR as CallAPIQR
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.TicketStatus as CallAPITicketStatus
import qualified ExternalBPP.Metro.ExternalAPI.CallAPI as CallAPI
import Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.FRFSTicket as QFRFSTicket
import Storage.Queries.Station as QStation
import Tools.Error

getStationCode :: Text -> Text
getStationCode stationCode = fromMaybe stationCode (listToMaybe $ T.splitOn "|" stationCode)

search :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> DFRFSSearch.FRFSSearch -> m DOnSearch
search _merchant _merchantOperatingCity providerConfig bapConfig searchReq = do
  fromStation <- QStation.findById searchReq.fromStationId >>= fromMaybeM (StationNotFound searchReq.fromStationId.getId)
  toStation <- QStation.findById searchReq.toStationId >>= fromMaybeM (StationNotFound searchReq.toStationId.getId)
  let stations = mkStations fromStation toStation
  let routeStations = []
  mbQuote <- mkQuote fromStation toStation routeStations stations searchReq.vehicleType
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.searchTTLSec
  messageId <- generateGUID
  return $
    DOnSearch
      { bppSubscriberId = bapConfig.subscriberId,
        bppSubscriberUrl = showBaseUrl bapConfig.subscriberUrl,
        providerDescription = Nothing,
        providerId = bapConfig.uniqueKeyId,
        providerName = "Metro",
        quotes = maybeToList mbQuote,
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

    mkQuote fromStation toStation routeStations stations vehicleType = do
      let req =
            CallAPIFareByOriginDest.FareByOriginDestReq
              { origin = getStationCode fromStation.code,
                destination = getStationCode toStation.code,
                ticketType = "SJT"
              }
      mbAmount <- CallAPI.getFareByOriginDest providerConfig req
      let mbPrice =
            mbAmount <&> \amount ->
              Price
                { amountInt = round amount,
                  amount = amount,
                  currency = INR -- fix later
                }
      return $
        mbPrice <&> \price ->
          DQuote
            { bppItemId = "Metro",
              _type = DFRFSQuote.SingleJourney,
              discounts = [],
              ..
            }

init :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOnInit
init _merchant _merchantOperatingCity _providerConfig bapConfig (_mRiderName, _mRiderNumber) booking = do
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
      Spec.BPP -> throwError $ InternalError "BPP payments not supported"

confirm :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Merchant -> MerchantOperatingCity -> FRFSConfig -> ProviderConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
confirm _merchant _merchantOperatingCity _frfsConfig config bapConfig (_mRiderName, mRiderNumber) booking = do
  bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  let bppOrderId :: Text = T.pack $ "CUM" ++ show ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID)) -- This should be max 20 characters UUID (Using Transaction UUID)
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
      paymentTxnId <- booking.paymentTxnId & fromMaybeM (InternalError $ "Payment Transaction Id Missing")
      fromStation <- QStation.findById booking.fromStationId >>= fromMaybeM (StationNotFound booking.fromStationId.getId)
      toStation <- QStation.findById booking.toStationId >>= fromMaybeM (StationNotFound booking.toStationId.getId)
      let req =
            CallAPIQR.GenerateQRReq
              { origin = getStationCode fromStation.code,
                destination = getStationCode toStation.code,
                ticketType = "SJT", -- TODO: FIX THIS
                noOfTickets = booking.quantity,
                ticketFare = getMoney (maybe booking.price.amountInt (.amountInt) booking.finalPrice),
                customerMobileNo = fromMaybe "9999999999" mRiderNumber,
                uniqueTxnRefNo = bppOrderId,
                bankRefNo = paymentTxnId,
                paymentMode = "UPI" -- TODO: fix this
              }
      ticketsData <- CallAPI.generateQRTickets config req
      ticketsData `forM` \CallAPIQR.TicketInfo {..} -> do
        return $
          DTicket
            { qrData = qrBytes,
              bppFulfillmentId = "Metro Fulfillment",
              ticketNumber = ticketNumber,
              validTill = expiryTime,
              status = "UNCLAIMED", -- TODO: Fix this
              description = Nothing,
              qrRefreshAt = Nothing
            }

status :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Merchant -> MerchantOperatingCity -> ProviderConfig -> BecknConfig -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
status _merchantId _merchantOperatingCity config bapConfig booking = do
  tickets' <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  bppOrderId <- booking.bppOrderId & fromMaybeM (InternalError $ "Bpp Order Id Missing")
  tickets <-
    tickets' `forM` \DFRFSTicket.FRFSTicket {status = _status, ..} -> do
      ticketStatus <-
        if _status == DFRFSTicket.ACTIVE
          then do
            cmrlStatus <- (.ticketStatus) <$> CallAPI.getTicketStatus config (CallAPITicketStatus.TicketStatusReq ticketNumber)
            case cmrlStatus of
              "Used" -> return "CLAIMED"
              _ -> return "UNCLAIMED"
          else return "UNCLAIMED"
      return $
        DTicket
          { bppFulfillmentId = "Metro Fulfillment",
            status = ticketStatus,
            ..
          }
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
