{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FRFSTicketService where

import qualified BecknV2.FRFS.Enums
import qualified Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicket
import qualified Domain.Types.FRFSTicketBooking
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.JourneyPlannerTypes
import Servant
import Tools.Auth

data AutocompleteRes = AutocompleteRes {routes :: [API.Types.UI.FRFSTicketService.FRFSRouteAPI], stops :: [API.Types.UI.FRFSTicketService.FRFSStationAPI]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSBookingPaymentAPI = FRFSBookingPaymentAPI
  { paymentOrder :: Data.Maybe.Maybe Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp,
    status :: API.Types.UI.FRFSTicketService.FRFSBookingPaymentStatusAPI,
    transactionId :: Data.Maybe.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSBookingPaymentStatusAPI
  = NEW
  | PENDING
  | SUCCESS
  | FAILURE
  | REFUND_PENDING
  | REFUNDED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSCanCancelStatus = FRFSCanCancelStatus
  { cancellationCharges :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    isCancellable :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    refundAmount :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSCancelStatus = FRFSCancelStatus {cancellationCharges :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney, refundAmount :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSConfigAPIRes = FRFSConfigAPIRes
  { bookingEndTime :: Kernel.Prelude.UTCTime,
    bookingStartTime :: Kernel.Prelude.UTCTime,
    customDates :: [Data.Text.Text],
    customEndTime :: Data.Text.Text,
    discount :: Kernel.Prelude.Int,
    freeTicketInterval :: Data.Maybe.Maybe Kernel.Prelude.Int,
    isCancellationAllowed :: Kernel.Prelude.Bool,
    isEventOngoing :: Kernel.Prelude.Bool,
    maxFreeTicketCashback :: Data.Maybe.Maybe Kernel.Prelude.Int,
    metroStationTtl :: Kernel.Prelude.Int,
    oneWayTicketLimit :: Kernel.Prelude.Int,
    roundTripTicketLimit :: Kernel.Prelude.Int,
    ticketsBookedInEvent :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSQuoteAPIRes = FRFSQuoteAPIRes
  { _route :: Data.Maybe.Maybe API.Types.UI.FRFSTicketService.FRFSRouteAPI,
    _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    discountedTickets :: Data.Maybe.Maybe Kernel.Prelude.Int,
    eventDiscountAmount :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    price :: Kernel.Types.Common.HighPrecMoney,
    priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    quantity :: Kernel.Prelude.Int,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    serviceTierDescription :: Data.Maybe.Maybe Data.Text.Text,
    serviceTierLongName :: Data.Maybe.Maybe Data.Text.Text,
    serviceTierShortName :: Data.Maybe.Maybe Data.Text.Text,
    serviceTierType :: Data.Maybe.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    stations :: [API.Types.UI.FRFSTicketService.FRFSStationAPI],
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSRouteAPI = FRFSRouteAPI {code :: Data.Text.Text, endPoint :: Kernel.External.Maps.Types.LatLong, longName :: Data.Text.Text, shortName :: Data.Text.Text, startPoint :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIReq = FRFSSearchAPIReq
  { fromStationCode :: Data.Text.Text,
    journeySearchData :: Data.Maybe.Maybe Lib.JourneyPlannerTypes.JourneySearchData,
    quantity :: Kernel.Prelude.Int,
    routeCode :: Data.Maybe.Maybe Data.Text.Text,
    toStationCode :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIRes = FRFSSearchAPIRes {searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSStationAPI = FRFSStationAPI
  { address :: Data.Maybe.Maybe Data.Text.Text,
    code :: Data.Text.Text,
    color :: Data.Maybe.Maybe Data.Text.Text,
    distance :: Data.Maybe.Maybe Kernel.Types.Common.Meters,
    lat :: Data.Maybe.Maybe Kernel.Prelude.Double,
    lon :: Data.Maybe.Maybe Kernel.Prelude.Double,
    name :: Data.Text.Text,
    sequenceNum :: Data.Maybe.Maybe Kernel.Prelude.Int,
    stationType :: Data.Maybe.Maybe API.Types.UI.FRFSTicketService.StationType
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketAPI = FRFSTicketAPI {createdAt :: Kernel.Prelude.UTCTime, qrData :: Data.Text.Text, status :: Domain.Types.FRFSTicket.FRFSTicketStatus, ticketNumber :: Data.Text.Text, validTill :: Kernel.Prelude.UTCTime}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketBookingStatusAPIRes = FRFSTicketBookingStatusAPIRes
  { _route :: Data.Maybe.Maybe API.Types.UI.FRFSTicketService.FRFSRouteAPI,
    _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    bookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    city :: Kernel.Types.Beckn.Context.City,
    createdAt :: Kernel.Prelude.UTCTime,
    discountedTickets :: Data.Maybe.Maybe Kernel.Prelude.Int,
    eventDiscountAmount :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    payment :: Data.Maybe.Maybe API.Types.UI.FRFSTicketService.FRFSBookingPaymentAPI,
    price :: Kernel.Types.Common.HighPrecMoney,
    priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    quantity :: Kernel.Prelude.Int,
    serviceTierDescription :: Data.Maybe.Maybe Data.Text.Text,
    serviceTierLongName :: Data.Maybe.Maybe Data.Text.Text,
    serviceTierShortName :: Data.Maybe.Maybe Data.Text.Text,
    serviceTierType :: Data.Maybe.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    stations :: [API.Types.UI.FRFSTicketService.FRFSStationAPI],
    status :: Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus,
    tickets :: [API.Types.UI.FRFSTicketService.FRFSTicketAPI],
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StationType
  = START
  | END
  | TRANSIT
  | INTERMEDIATE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
