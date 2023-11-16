module Domain.Types.Tickets.TicketService where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id

data AttendeeType = Adult | Kid | CameraUnit
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data TicketServicePrice = TicketServicePrice
  { ticketServiceId :: Text,
    attendeeType :: Text,
    pricePerUnit :: HighPrecMoney
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data TicketService = TicketService
  { id :: Id TicketService,
    placesId :: Text,
    service :: Text,
    maxVerification :: Int,
    openTimings :: Maybe TimeOfDay,
    closeTimings :: Maybe TimeOfDay,
    prices :: [TicketServicePrice]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
