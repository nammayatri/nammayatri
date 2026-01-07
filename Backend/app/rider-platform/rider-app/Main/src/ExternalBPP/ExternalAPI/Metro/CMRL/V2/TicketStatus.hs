{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.TicketStatus where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.Extra.IntegratedBPPConfig
import Domain.Types.FRFSTicketBooking
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Utils.Common
import Servant hiding (throwError)
import Tools.Error

data QRPayloadStatus = QRPayloadStatus
  { qR_Signature :: T.Text,
    qR_SVC :: T.Text,
    qR_Tkt_Block :: T.Text
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON QRPayloadStatus where
  parseJSON = withObject "QRPayloadStatus" $ \v ->
    QRPayloadStatus
      <$> v .: "QR_Signature"
      <*> v .: "QR_SVC"
      <*> v .: "QR_Tkt_Block"

data TicketStatusResult = TicketStatusResult
  { qR_Payload :: QRPayloadStatus,
    qR_Tkt_Sl_No :: T.Text,
    qR_SHA256 :: T.Text,
    merchant_Order_Id :: T.Text,
    interchange_Status :: T.Text,
    interchange_Stations :: T.Text,
    platform_No :: T.Text
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON TicketStatusResult where
  parseJSON = withObject "TicketStatusResult" $ \v ->
    TicketStatusResult
      <$> v .: "QR_Payload"
      <*> v .: "QR_Tkt_Sl_No"
      <*> v .: "QR_SHA256"
      <*> v .: "Merchant_Order_Id"
      <*> v .: "Interchange_Status"
      <*> v .: "Interchange_Stations"
      <*> v .: "Platform_No"

data TicketStatusRes = TicketStatusRes
  { returnCode :: T.Text,
    returnMessage :: T.Text,
    ticket_Response :: [TicketStatusResult]
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON TicketStatusRes where
  parseJSON = withObject "TicketStatusRes" $ \v ->
    TicketStatusRes
      <$> v .: "returnCode"
      <*> v .: "returnMessage"
      <*> v .: "Ticket_Response"

type TicketStatusAPI =
  "api" :> "qr" :> "v1" :> "tickets" :> Capture "merchantOrderId" T.Text :> "status"
    :> Header "Authorization" T.Text
    :> QueryParam' '[Required, Strict] "operatorNameId" Int
    :> QueryParam' '[Required, Strict] "merchantId" T.Text
    :> Get '[JSON] TicketStatusRes

ticketStatusAPI :: Proxy TicketStatusAPI
ticketStatusAPI = Proxy

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus config booking = do
  logInfo $ "[CMRLV2:TicketStatus] Getting ticket status for bookingId: " <> booking.id.getId
  bppOrderId <- case booking.bppOrderId of
    Just oid -> return oid
    Nothing -> do
      logError "[CMRLV2:TicketStatus] BPP Order ID not found"
      throwError $ InternalError "BPP Order ID not found"
  logDebug $ "[CMRLV2:TicketStatus] BppOrderId: " <> bppOrderId
  logDebug $ "[CMRLV2:TicketStatus] Request params - merchantOrderId: " <> bppOrderId <> ", operatorNameId: " <> T.pack (show config.operatorNameId) <> ", merchantId: " <> config.merchantId

  let eulerClient = \accessToken ->
        ET.client ticketStatusAPI bppOrderId (Just $ "Bearer " <> accessToken) config.operatorNameId config.merchantId

  ticketStatusRes <- callCMRLV2API config eulerClient "getTicketStatus" ticketStatusAPI
  logDebug $ "[CMRLV2:TicketStatus] Response - returnCode: " <> ticketStatusRes.returnCode <> ", returnMessage: " <> ticketStatusRes.returnMessage

  when (ticketStatusRes.returnCode /= "0") $ do
    logError $ "[CMRLV2:TicketStatus] Ticket status fetch failed: " <> ticketStatusRes.returnMessage
    throwError $ InternalError $ "Ticket status fetch failed: " <> ticketStatusRes.returnMessage

  logInfo $ "[CMRLV2:TicketStatus] Returning " <> show (length ticketStatusRes.ticket_Response) <> " tickets from API response"

  return $
    map
      ( \ticketResp ->
          let qrPayload = ticketResp.qR_Payload
              qrData = "#" <> qrPayload.qR_Signature <> "#" <> qrPayload.qR_SVC <> "#" <> qrPayload.qR_Tkt_Block <> "#"
           in ProviderTicket
                { ticketNumber = ticketResp.qR_Tkt_Sl_No,
                  vehicleNumber = Nothing,
                  qrData = qrData,
                  qrStatus = "ACTIVE",
                  qrValidity = booking.validTill,
                  description = Nothing,
                  qrRefreshAt = Nothing,
                  commencingHours = Nothing
                }
      )
      ticketStatusRes.ticket_Response
