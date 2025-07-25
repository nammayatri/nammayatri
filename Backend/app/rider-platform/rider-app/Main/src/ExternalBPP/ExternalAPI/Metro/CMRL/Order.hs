{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.Order where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.UUID as UU
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET hiding (Log)
import ExternalBPP.ExternalAPI.Metro.CMRL.Auth
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.UpdateQrReceivedStatus as UpdateQr
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.CacheFlow
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c) => CMRLConfig -> IntegratedBPPConfig -> FRFSTicketBooking -> Maybe Text -> m ProviderOrder
createOrder config integratedBPPConfig booking mRiderNumber = do
  orderId <- case booking.bppOrderId of
    Just oid -> return oid
    Nothing -> getBppOrderId booking
  paymentTxnId <- booking.paymentTxnId & fromMaybeM (InternalError $ "Payment Transaction Id Missing")
  fromStation <- OTPRest.getStationByGtfsIdAndStopCode booking.fromStationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> booking.fromStationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  toStation <- OTPRest.getStationByGtfsIdAndStopCode booking.toStationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> booking.toStationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  ticketsData <-
    generateQRTickets config $
      GenerateQRReq
        { origin = fromStation.code,
          destination = toStation.code,
          ticketType = "SJT", -- TODO: FIX THIS
          noOfTickets = 1, -- Always set to 1 as per requirement
          ticketFare = getMoney (maybe booking.price.amountInt (.amountInt) booking.finalPrice) `div` max 1 booking.quantity,
          customerMobileNo = fromMaybe "9999999999" mRiderNumber,
          uniqueTxnRefNo = orderId,
          bankRefNo = paymentTxnId,
          paymentMode = "UPI",
          appType = cmrlAppType,
          paxCount = booking.quantity, -- Number of tickets
          qrTypeCode = "FQR"
        }
  tickets <-
    ticketsData `forM` \TicketInfo {..} -> do
      return $
        ProviderTicket
          { ticketNumber = ticketNumber,
            vehicleNumber = Nothing,
            qrData = qrBytes,
            qrStatus = "UNCLAIMED",
            qrValidity = expiryTime,
            description = Nothing,
            qrRefreshAt = Nothing
          }
  return ProviderOrder {..}

getBppOrderId :: (MonadFlow m) => FRFSTicketBooking -> m Text
getBppOrderId booking = do
  bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  let orderId = T.pack $ "CUM" ++ show ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID)) -- This should be max 20 characters UUID (Using Transaction UUID)
  return orderId

data GenerateQRReq = GenerateQRReq
  { origin :: T.Text,
    destination :: T.Text,
    ticketType :: T.Text,
    noOfTickets :: Int,
    ticketFare :: Int,
    customerMobileNo :: T.Text,
    uniqueTxnRefNo :: T.Text,
    bankRefNo :: T.Text,
    paymentMode :: T.Text,
    appType :: T.Text,
    paxCount :: Int,
    qrTypeCode :: T.Text
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
  "CmrlThirdParty" :> "generateqrticket"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] GenerateQRReq
    :> Post '[JSON] GenerateQRRes

generateQRAPI :: Proxy GenerateQRAPI
generateQRAPI = Proxy

generateQRTickets :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CMRLConfig -> GenerateQRReq -> m [TicketInfo]
generateQRTickets config qrReq = do
  let modifiedQrReq = qrReq {origin = getStationCode qrReq.origin, destination = getStationCode qrReq.destination}
      eulerClient = \accessToken -> ET.client generateQRAPI (Just $ "Bearer " <> accessToken) modifiedQrReq
  result <- try @_ @SomeException $ callCMRLAPI config eulerClient "generateQRTickets" generateQRAPI
  case result of
    Left err -> do
      let updateReq =
            UpdateQr.UpdateQrReceivedStatusReq
              { UpdateQr.txnRefNo = qrReq.uniqueTxnRefNo,
                UpdateQr.appType = "CMRL_CUM_IQR",
                UpdateQr.isFailure = True,
                UpdateQr.failureReason = T.pack (show err)
              }
      _ <- UpdateQr.updateQrReceivedStatus config updateReq
      throwError $ InternalError $ "Failed to fetch QR ticket: " <> T.pack (show err)
    Right qrResponse -> return qrResponse.result
  where
    getStationCode :: Text -> Text
    getStationCode stationCode = fromMaybe stationCode (listToMaybe $ T.splitOn "|" stationCode)
