module ExternalBPP.Bus.ExternalAPI.Types where

import Kernel.Prelude

data ProviderTicket = ProviderTicket
  { ticketNumber :: Text,
    qrData :: Text,
    qrStatus :: Text,
    qrValidity :: UTCTime
  }

data ProviderOrder = ProviderOrder
  { orderId :: Text,
    tickets :: [ProviderTicket]
  }
