{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.Order where

import Control.Lens ((^?), _head)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import qualified Data.UUID as UU
import Domain.Types.Extra.IntegratedBPPConfig
import Domain.Types.FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET hiding (Log)
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Encryption
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.CacheFlow
import Kernel.Utils.Common
import Servant hiding (throwError)
import SharedLogic.FRFSUtils
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error

data TicketReq = TicketReq
  { request :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketEncryptedRes = TicketEncryptedRes
  { response :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data OperatorData = OperatorData
  { operatorNameId :: Int,
    merchantOrderId :: T.Text,
    bankTransactionRefNumber :: T.Text,
    merchantId :: T.Text,
    ticketTypeId :: Int,
    paymentMode :: Int,
    paymentChannelId :: Int,
    transTypeId :: Int,
    zoneNumber :: Int,
    fareQuoteId :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketInfoPayload = TicketInfoPayload
  { grp_Size :: T.Text,
    src_Stn :: T.Text,
    dest_Stn :: T.Text,
    activation_Date :: T.Text,
    product_Id :: T.Text,
    service_Id :: T.Text,
    tkt_Fare :: T.Text,
    validity :: T.Text,
    duration :: T.Text,
    operatorData :: OperatorData
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Operator = Operator
  { opID :: T.Text,
    noOfTickets :: T.Text,
    validator_Info :: T.Text,
    ticketInfo :: [TicketInfoPayload]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data DynamicBlock = DynamicBlock
  { operators :: [Operator]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketBlock = TicketBlock
  { dynamic_Block :: DynamicBlock
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GenerateTicketPayload = GenerateTicketPayload
  { requester_ID :: T.Text,
    language :: T.Text,
    txn_Type :: T.Text,
    txn_Ref_No :: T.Text,
    txn_Date :: T.Text,
    pSP_Specific_Data :: T.Text,
    total_Fare :: T.Text,
    customer_Mobile :: T.Text,
    ticketBlock :: TicketBlock
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data QRPayload = QRPayload
  { qR_Signature :: T.Text,
    qR_SVC :: T.Text,
    qR_Tkt_Block :: T.Text
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON QRPayload where
  parseJSON = withObject "QRPayload" $ \v ->
    QRPayload
      <$> v .: "QR_Signature"
      <*> v .: "QR_SVC"
      <*> v .: "QR_Tkt_Block"

data TicketResponse = TicketResponse
  { qR_Payload :: QRPayload,
    qR_Tkt_Sl_No :: T.Text,
    qR_SHA256 :: T.Text,
    merchant_Order_Id :: T.Text,
    interchange_Status :: T.Text,
    interchange_Stations :: T.Text,
    platform_No :: T.Text,
    ticket_Generation_Time :: T.Text,
    ticket_Validity_Time :: T.Text
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON TicketResponse where
  parseJSON = withObject "TicketResponse" $ \v ->
    TicketResponse
      <$> v .: "QR_Payload"
      <*> v .: "QR_Tkt_Sl_No"
      <*> v .: "QR_SHA256"
      <*> v .: "Merchant_Order_Id"
      <*> v .: "Interchange_Status"
      <*> v .: "Interchange_Stations"
      <*> v .: "Platform_No"
      <*> v .: "Ticket_Generation_Time"
      <*> v .: "Ticket_Validity_Time"

data TicketRes = TicketRes
  { returnCode :: T.Text,
    returnMessage :: T.Text,
    ticket_Response :: [TicketResponse]
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON TicketRes where
  parseJSON = withObject "TicketRes" $ \v ->
    TicketRes
      <$> v .: "returnCode"
      <*> v .: "returnMessage"
      <*> v .: "Ticket_Response"

type TicketAPI =
  "api" :> "qr" :> "v1" :> "tickets" :> "generate"
    :> Header "Authorization" T.Text
    :> Header "X-ENC-ALGO" T.Text
    :> Header "X-ENC-KEY-INDEX" T.Text
    :> ReqBody '[JSON] TicketReq
    :> Post '[JSON] TicketEncryptedRes

ticketAPI :: Proxy TicketAPI
ticketAPI = Proxy

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => CMRLV2Config -> IntegratedBPPConfig -> FRFSTicketBooking -> [FRFSQuoteCategory] -> Maybe Text -> m ProviderOrder
createOrder config integratedBPPConfig booking quoteCategories mRiderNumber = do
  logInfo $ "[CMRLV2:Order] Starting createOrder for bookingId: " <> booking.id.getId
  orderId <- case booking.bppOrderId of
    Just oid -> return oid
    Nothing -> getBppOrderId booking
  logDebug $ "[CMRLV2:Order] OrderId: " <> orderId
  paymentTxnId <- booking.paymentTxnId & fromMaybeM (InternalError $ "Payment Transaction Id Missing")
  logDebug $ "[CMRLV2:Order] PaymentTxnId: " <> paymentTxnId
  fromStation <- OTPRest.getStationByGtfsIdAndStopCode booking.fromStationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> booking.fromStationCode)
  toStation <- OTPRest.getStationByGtfsIdAndStopCode booking.toStationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> booking.toStationCode)
  logDebug $ "[CMRLV2:Order] From: " <> fromStation.code <> " -> To: " <> toStation.code

  now <- getCurrentTime
  let travelDatetime = T.pack $ formatTime defaultTimeLocale "%d%m%Y%H%M%S" now
      fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
      singleAdultTicketPrice = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice.amountInt.getMoney)
      totalTicketQuantity = fareParameters.totalQuantity
      totalFare = fromMaybe 0 singleAdultTicketPrice * totalTicketQuantity
      singleTicketFare = fromMaybe 0 singleAdultTicketPrice

  let fareQuoteIdValue = booking.id.getId
  logDebug $ "[CMRLV2:Order] Using booking ID as fareQuoteId: " <> fareQuoteIdValue

  let operatorData =
        OperatorData
          { operatorNameId = config.operatorNameId,
            merchantOrderId = orderId,
            bankTransactionRefNumber = paymentTxnId,
            merchantId = config.merchantId,
            ticketTypeId = config.ticketTypeId,
            paymentMode = 102,
            paymentChannelId = 0,
            transTypeId = 100,
            zoneNumber = 20,
            fareQuoteId = fareQuoteIdValue
          }

      extractStationCode stationCode = fromMaybe stationCode $ drop 1 (T.splitOn "|" stationCode) ^? _head

      ticketInfoPayload =
        TicketInfoPayload
          { grp_Size = T.pack $ show totalTicketQuantity,
            src_Stn = extractStationCode fromStation.code,
            dest_Stn = extractStationCode toStation.code,
            activation_Date = travelDatetime,
            product_Id = T.pack $ show config.ticketTypeId,
            service_Id = "1",
            tkt_Fare = T.pack $ show singleTicketFare,
            validity = "100",
            duration = "180",
            operatorData = operatorData
          }

      operator =
        Operator
          { opID = T.pack $ show config.operatorNameId,
            noOfTickets = "1",
            validator_Info = "31",
            ticketInfo = [ticketInfoPayload]
          }

      payload =
        GenerateTicketPayload
          { requester_ID = config.merchantId,
            language = "0",
            txn_Type = "65",
            txn_Ref_No = orderId,
            txn_Date = travelDatetime,
            pSP_Specific_Data = "Mode=UPI;ServiceFee=0%",
            total_Fare = T.pack $ show totalFare,
            customer_Mobile = fromMaybe "9999999999" mRiderNumber,
            ticketBlock =
              TicketBlock
                { dynamic_Block =
                    DynamicBlock
                      { operators = [operator]
                      }
                }
          }

  logDebug $ "[CMRLV2:Order] TotalFare: " <> show totalFare <> ", Quantity: " <> show totalTicketQuantity
  logDebug $ "[CMRLV2:Order] Payload JSON (before encryption): " <> T.pack (show payload)
  logDebug $ "[CMRLV2:Order] Payload built, encrypting..."
  encKey <- decrypt config.encryptionKey
  let payloadText = TE.decodeUtf8 $ BL.toStrict $ encode payload
  logDebug $ "[CMRLV2:Order] Payload Text: " <> payloadText
  encryptedPayload <- encryptPayload payloadText encKey
  logDebug $ "[CMRLV2:Order] Encrypted Payload: " <> encryptedPayload
  logDebug $ "[CMRLV2:Order] Payload encrypted, calling CMRL API..."

  let eulerClient = \accessToken ->
        ET.client
          ticketAPI
          (Just $ "Bearer " <> accessToken)
          (Just "AES_CBC_PKCS5")
          (Just $ T.pack $ show config.encKeyIndex)
          (TicketReq encryptedPayload)

  encryptedResponse <- callCMRLV2API config eulerClient "generateTicket" ticketAPI
  logDebug $ "[CMRLV2:Order] Encrypted Response: " <> encryptedResponse.response
  decryptedResponseText <- case decryptPayload encryptedResponse.response encKey of
    Left err -> do
      logError $ "[CMRLV2:Order] Decryption failed: " <> T.pack err
      throwError $ InternalError $ "Decryption failed: " <> T.pack err
    Right txt -> return txt
  logDebug "[CMRLV2:Order] Response decrypted, parsing..."
  logDebug $ "[CMRLV2:Order] Decrypted Response: " <> decryptedResponseText
  ticketRes <- case eitherDecode (BL.fromStrict $ TE.encodeUtf8 decryptedResponseText) :: Either String TicketRes of
    Left err -> do
      logError $ "[CMRLV2:Order] Failed to decode ticket response: " <> T.pack err
      throwError $ InternalError $ "Failed to decode ticket response: " <> T.pack err
    Right res -> do
      logDebug $ "[CMRLV2:Order] Parsed response - returnCode: " <> res.returnCode <> ", returnMessage: " <> res.returnMessage
      return res

  when (ticketRes.returnCode /= "0") $ do
    logError $ "[CMRLV2:Order] Ticket generation failed: " <> ticketRes.returnMessage
    throwError $ InternalError $ "Ticket generation failed: " <> ticketRes.returnMessage

  logInfo $ "[CMRLV2:Order] Ticket generation successful, tickets count: " <> show (length ticketRes.ticket_Response)

  tickets <-
    ticketRes.ticket_Response `forM` \ticketResp -> do
      let qrPayload = ticketResp.qR_Payload
          qrData = "#" <> qrPayload.qR_Signature <> "#" <> qrPayload.qR_SVC <> "#" <> qrPayload.qR_Tkt_Block <> "#"

      validityTime <- case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack ticketResp.ticket_Validity_Time) of
        Just time -> return time
        Nothing -> throwError $ InternalError $ "Failed to parse ticket validity time: " <> ticketResp.ticket_Validity_Time

      return $
        ProviderTicket
          { ticketNumber = ticketResp.qR_Tkt_Sl_No,
            vehicleNumber = Nothing,
            qrData = qrData,
            qrStatus = "UNCLAIMED",
            qrValidity = validityTime,
            description = Nothing,
            qrRefreshAt = Nothing,
            commencingHours = Nothing
          }

  return ProviderOrder {..}

getBppOrderId :: (MonadFlow m) => FRFSTicketBooking -> m Text
getBppOrderId booking = do
  bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  let orderId = T.pack $ "CUM" ++ show ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID))
  return orderId
