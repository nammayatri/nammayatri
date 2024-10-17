{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.CMRL.ExternalAPI.TicketStatus where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import GHC.Generics (Generic)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Servant
import Tools.Error

data TicketStatusReq = TicketStatusReq
  { ticketNo :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON TicketStatusReq where
  toJSON = genericToJSON defaultOptions

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
  deriving (Generic, Show)

instance FromJSON TicketStatusResult where
  parseJSON = genericParseJSON defaultOptions

data TicketStatusRes = TicketStatusRes
  { statusCode :: Int,
    message :: T.Text,
    result :: TicketStatusResult
  }
  deriving (Generic, Show)

instance FromJSON TicketStatusRes where
  parseJSON = genericParseJSON defaultOptions

type TicketStatusAPI =
  "cumta" :> "ticketStatus"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] TicketStatusReq
    :> Get '[JSON] TicketStatusRes

ticketStatusAPI :: Proxy TicketStatusAPI
ticketStatusAPI = Proxy

getTicketStatus :: (CoreMetrics m, MonadFlow m) => T.Text -> T.Text -> TicketStatusReq -> m TicketStatusResult
getTicketStatus host accessToken req = do
  response <-
    callAPI host (ET.client ticketStatusAPI (Just $ "Bearer " <> accessToken) req) "getTicketStatus" ticketStatusAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_TICKET_STATUS_API") host)
  return response.result
