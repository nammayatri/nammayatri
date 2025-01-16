{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.Metro.ExternalAPI.CMRL.TicketStatus where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.Metro.ExternalAPI.CMRL.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant
import Tools.Error

data TicketStatusReq = TicketStatusReq
  { ticketNo :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketStatusResult = TicketStatusResult
  { bookingRefNo :: T.Text,
    bookingTs :: T.Text,
    bookingTypeCode :: T.Text,
    passengerCount :: Int,
    maxTapInCount :: Int,
    maxTapOutCount :: Int,
    tapInCount :: Int,
    tapOutCount :: Int,
    tapInTime :: Maybe T.Text,
    tapInStation :: Maybe T.Text,
    ticketNumber :: T.Text,
    ticketAmount :: Int,
    ticketStatus :: T.Text,
    ticketExpired :: Bool,
    expiryTs :: T.Text,
    originStopCode :: T.Text,
    originStopName :: T.Text,
    destinationStopCode :: T.Text,
    destinationStopName :: T.Text,
    paymentMode :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketStatusRes = TicketStatusRes
  { statusCode :: Int,
    message :: T.Text,
    result :: TicketStatusResult
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type TicketStatusAPI =
  "cumta" :> "ticketStatus"
    :> Header "Authorization" T.Text
    :> MandatoryQueryParam "ticketNo" T.Text
    :> Get '[JSON] TicketStatusRes

ticketStatusAPI :: Proxy TicketStatusAPI
ticketStatusAPI = Proxy

getTicketStatus :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => CMRLConfig -> TicketStatusReq -> m TicketStatusResult
getTicketStatus config req = do
  accessToken <- getAuthToken config
  response <-
    callAPI config.networkHostUrl (ET.client ticketStatusAPI (Just $ "Bearer " <> accessToken) req.ticketNo) "getTicketStatus" ticketStatusAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_TICKET_STATUS_API") config.networkHostUrl)
  return response.result
