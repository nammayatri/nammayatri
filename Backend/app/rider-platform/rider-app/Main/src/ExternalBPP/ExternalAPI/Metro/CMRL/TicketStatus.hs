{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.TicketStatus where

import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Types.FRFSTicket as Ticket
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.Auth
import ExternalBPP.ExternalAPI.Types
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Utils.Common
import Servant
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
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

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => CMRLConfig -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus config booking = do
  tickets <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  updatedTickets <-
    mapM
      ( \ticket -> do
          if ticket.status == Ticket.ACTIVE
            then do
              accessToken <- getAuthToken config
              ticketStatus <-
                callAPI config.networkHostUrl (ET.client ticketStatusAPI (Just $ "Bearer " <> accessToken) ticket.ticketNumber) "getTicketStatus" ticketStatusAPI
                  >>= fromEitherM (ExternalAPICallError (Just "CMRL_TICKET_STATUS_API") config.networkHostUrl)
              let qrStatus = mkTicketStatus ticketStatus.result
              return $
                Just $
                  ProviderTicket
                    { ticketNumber = ticket.ticketNumber,
                      qrData = ticket.qrData,
                      qrStatus,
                      qrValidity = ticket.validTill,
                      description = ticket.description,
                      qrRefreshAt = ticket.qrRefreshAt
                    }
            else pure Nothing
      )
      tickets
  return $ catMaybes updatedTickets
  where
    mkTicketStatus ticket =
      case ticket.ticketStatus of
        "Used" -> "CLAIMED"
        _ -> "UNCLAIMED"
