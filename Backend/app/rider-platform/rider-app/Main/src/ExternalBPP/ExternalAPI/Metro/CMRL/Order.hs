{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.Order where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.UUID as UU
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.Auth
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.CacheFlow
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Storage.Queries.Station as QStation
import Tools.Error

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => CMRLConfig -> FRFSTicketBooking -> Maybe Text -> m ProviderOrder
createOrder config booking mRiderNumber = do
  when (isJust booking.bppOrderId) $ throwError (InternalError $ "Order Already Created for Booking : " <> booking.id.getId)
  bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  let orderId = T.pack $ "CUM" ++ show ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID)) -- This should be max 20 characters UUID (Using Transaction UUID)
  paymentTxnId <- booking.paymentTxnId & fromMaybeM (InternalError $ "Payment Transaction Id Missing")
  fromStation <- QStation.findById booking.fromStationId >>= fromMaybeM (StationNotFound booking.fromStationId.getId)
  toStation <- QStation.findById booking.toStationId >>= fromMaybeM (StationNotFound booking.toStationId.getId)
  ticketsData <-
    generateQRTickets config $
      GenerateQRReq
        { origin = fromStation.code,
          destination = toStation.code,
          ticketType = "SJT", -- TODO: FIX THIS
          noOfTickets = booking.quantity,
          ticketFare = getMoney (maybe booking.price.amountInt (.amountInt) booking.finalPrice),
          customerMobileNo = fromMaybe "9999999999" mRiderNumber,
          uniqueTxnRefNo = orderId,
          bankRefNo = paymentTxnId,
          paymentMode = "UPI" -- TODO: fix this
        }
  tickets <-
    ticketsData `forM` \TicketInfo {..} -> do
      return $
        ProviderTicket
          { ticketNumber = ticketNumber,
            qrData = qrBytes,
            qrStatus = "UNCLAIMED",
            qrValidity = expiryTime,
            description = Nothing,
            qrRefreshAt = Nothing
          }
  return ProviderOrder {..}

data GenerateQRReq = GenerateQRReq
  { origin :: T.Text,
    destination :: T.Text,
    ticketType :: T.Text,
    noOfTickets :: Int,
    ticketFare :: Int,
    customerMobileNo :: T.Text,
    uniqueTxnRefNo :: T.Text,
    bankRefNo :: T.Text,
    paymentMode :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketInfo = TicketInfo
  { ticketNumber :: T.Text,
    qrBytes :: T.Text,
    bookingTime :: T.Text,
    expiryTime :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GenerateQRRes = GenerateQRRes
  { statusCode :: Int,
    message :: T.Text,
    result :: [TicketInfo]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type GenerateQRAPI =
  "cumta" :> "generateqrticket"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] GenerateQRReq
    :> Post '[JSON] GenerateQRRes

generateQRAPI :: Proxy GenerateQRAPI
generateQRAPI = Proxy

generateQRTickets :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CMRLConfig -> GenerateQRReq -> m [TicketInfo]
generateQRTickets config qrReq = do
  let modifiedQrReq = qrReq {origin = getStationCode qrReq.origin, destination = getStationCode qrReq.destination}
  accessToken <- getAuthToken config
  qrResponse <-
    callAPI config.networkHostUrl (ET.client generateQRAPI (Just $ "Bearer " <> accessToken) modifiedQrReq) "generateQRTickets" generateQRAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_GENERATE_QR_TICKET_API") config.networkHostUrl)
  return qrResponse.result
  where
    getStationCode :: Text -> Text
    getStationCode stationCode = fromMaybe stationCode (listToMaybe $ T.splitOn "|" stationCode)
