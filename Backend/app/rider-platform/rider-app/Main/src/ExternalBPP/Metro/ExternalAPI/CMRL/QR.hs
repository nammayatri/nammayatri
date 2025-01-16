{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.Metro.ExternalAPI.CMRL.QR where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.Metro.ExternalAPI.CMRL.Auth
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.CacheFlow
import Kernel.Utils.Common
import Servant
import Tools.Error

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
  accessToken <- getAuthToken config
  qrResponse <-
    callAPI config.networkHostUrl (ET.client generateQRAPI (Just $ "Bearer " <> accessToken) qrReq) "generateQRTickets" generateQRAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_GENERATE_QR_TICKET_API") config.networkHostUrl)
  return qrResponse.result
