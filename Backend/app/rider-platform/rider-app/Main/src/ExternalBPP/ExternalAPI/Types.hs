module ExternalBPP.ExternalAPI.Types where

import Kernel.Prelude
import Kernel.Types.Price

-- Encrypted QR code generation
data TicketPayload = TicketPayload
  { fromRouteProviderCode :: Text,
    toRouteProviderCode :: Text,
    adultQuantity :: Int,
    childQuantity :: Int,
    vehicleTypeProviderCode :: Text,
    ticketNumber :: Text,
    ticketAmount :: Money,
    expiry :: Text,
    expiryIST :: UTCTime,
    refreshAt :: Maybe UTCTime,
    otpCode :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, Show, Read, Eq)

data ProviderTicket = ProviderTicket
  { ticketNumber :: Text,
    vehicleNumber :: Maybe Text,
    description :: Maybe Text,
    qrData :: Text,
    qrStatus :: Text,
    qrValidity :: UTCTime,
    qrRefreshAt :: Maybe UTCTime,
    commencingHours :: Maybe Int
  }

data ProviderOrder = ProviderOrder
  { orderId :: Text,
    tickets :: [ProviderTicket]
  }
