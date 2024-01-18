{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FRFSTicketService where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicket
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus
import qualified Domain.Types.FRFSTrip
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Station
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data FRFSBookingPaymentAPI = FRFSBookingPaymentAPI
  { status :: API.Types.UI.FRFSTicketService.FRFSBookingPaymentStatusAPI
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSBookingPaymentStatusAPI = PENDING | SUCCESS | FAILURE | REFUNDED
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data FRFSBookingStatusAPIRes = FRFSBookingStatusAPIRes
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    bookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    payment :: Kernel.Prelude.Maybe API.Types.UI.FRFSTicketService.FRFSBookingPaymentAPI,
    price :: Kernel.Types.Common.HighPrecMoney,
    quantity :: Kernel.Prelude.Int,
    stations :: [API.Types.UI.FRFSTicketService.FRFSStationAPI],
    status :: Domain.Types.FRFSTicketBookingStatus.FRFSTicketBookingStatus,
    ticket :: [API.Types.UI.FRFSTicketService.FRFSTicketAPI],
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSQuoteAPI = FRFSQuoteAPI
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    price :: Kernel.Types.Common.HighPrecMoney,
    quantity :: Kernel.Prelude.Int,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    stations :: [API.Types.UI.FRFSTicketService.FRFSStationAPI],
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIReq = FRFSSearchAPIReq
  { from :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    quantity :: Kernel.Prelude.Int,
    to :: Kernel.Types.Id.Id Domain.Types.Station.Station
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIRes = FRFSSearchAPIRes
  { searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSStationAPI = FRFSStationAPI
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    code :: Kernel.Prelude.Text,
    color :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    name :: Kernel.Prelude.Text,
    stationType :: Kernel.Prelude.Maybe Domain.Types.FRFSTrip.StationType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSTicketAPI = FRFSTicketAPI
  { qrData :: Kernel.Prelude.Text,
    status :: Domain.Types.FRFSTicket.FRFSTicketStatus,
    ticketNumber :: Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
