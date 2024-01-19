{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FRFSTicketService where

import qualified Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicket
import qualified Domain.Types.FRFSTicketBooking
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

data FRFSQuoteAPIRes = FRFSQuoteAPIRes
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    price :: Kernel.Types.Common.HighPrecMoney,
    quantity :: Kernel.Prelude.Int,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    stations :: [API.Types.UI.FRFSTicketService.FRFSStationAPI],
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: Domain.Types.Station.FRFSVehicleType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIReq = FRFSSearchAPIReq
  { fromStationCode :: Data.Text.Text,
    quantity :: Kernel.Prelude.Int,
    toStationCode :: Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIRes = FRFSSearchAPIRes
  { searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSStationAPI = FRFSStationAPI
  { address :: Data.Maybe.Maybe Data.Text.Text,
    code :: Data.Text.Text,
    color :: Data.Maybe.Maybe Data.Text.Text,
    lat :: Data.Maybe.Maybe Kernel.Prelude.Double,
    lon :: Data.Maybe.Maybe Kernel.Prelude.Double,
    name :: Data.Text.Text,
    stationType :: Data.Maybe.Maybe Domain.Types.FRFSTrip.StationType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSTicketAPI = FRFSTicketAPI
  { qrData :: Data.Text.Text,
    status :: Domain.Types.FRFSTicket.FRFSTicketStatus,
    ticketNumber :: Data.Text.Text,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSTicketBookingStatusAPIRes = FRFSTicketBookingStatusAPIRes
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    bookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    payment :: Data.Maybe.Maybe API.Types.UI.FRFSTicketService.FRFSBookingPaymentAPI,
    price :: Kernel.Types.Common.HighPrecMoney,
    quantity :: Kernel.Prelude.Int,
    stations :: [API.Types.UI.FRFSTicketService.FRFSStationAPI],
    status :: Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus,
    tickets :: [API.Types.UI.FRFSTicketService.FRFSTicketAPI],
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: Domain.Types.Station.FRFSVehicleType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
