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
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import Tools.Error

data QRPayloadStatus = QRPayloadStatus
  { qR_Signature :: T.Text,
    qR_SVC :: T.Text,
    qR_Tkt_Block :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketStatusResult = TicketStatusResult
  { qR_Payload :: QRPayloadStatus,
    qR_Tkt_Sl_No :: T.Text,
    qR_SHA256 :: T.Text,
    merchant_Order_Id :: T.Text,
    interchange_Status :: T.Text,
    interchange_Stations :: T.Text,
    platform_No :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketStatusRes = TicketStatusRes
  { returnCode :: T.Text,
    returnMessage :: T.Text,
    ticket_Response :: [TicketStatusResult]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

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

  tickets <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  logInfo $ "[CMRLV2:TicketStatus] Returning " <> show (length tickets) <> " tickets"

  return $
    map
      ( \ticket ->
          ProviderTicket
            { ticketNumber = ticket.ticketNumber,
              vehicleNumber = Nothing,
              qrData = ticket.qrData,
              qrStatus = T.pack $ show ticket.status,
              qrValidity = ticket.validTill,
              description = ticket.description,
              qrRefreshAt = ticket.qrRefreshAt,
              commencingHours = ticket.commencingHours
            }
      )
      tickets
