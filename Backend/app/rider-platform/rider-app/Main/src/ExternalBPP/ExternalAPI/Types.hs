module ExternalBPP.ExternalAPI.Types where

import Kernel.Prelude
import Kernel.Types.Price

-- Encrypted QR code generation
data TicketPayload = TicketPayload
  { transactionId :: Text,
    fromRouteProviderCode :: Text,
    toRouteProviderCode :: Text,
    adultQuantity :: Int,
    childQuantity :: Int,
    vehicleTypeProviderCode :: Text,
    expiry :: Text,
    ticketNumber :: Text,
    ticketAmount :: Money,
    refreshAt :: Maybe UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Read)

data ProviderTicket = ProviderTicket
  { ticketNumber :: Text,
    description :: Maybe Text,
    qrData :: Text,
    qrStatus :: Text,
    qrValidity :: UTCTime,
    qrRefreshAt :: Maybe UTCTime
  }

data ProviderOrder = ProviderOrder
  { orderId :: Text,
    tickets :: [ProviderTicket]
  }
