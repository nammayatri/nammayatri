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
import qualified Domain.Types.RecentLocation
import qualified Domain.Types.StationType
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Lib.JourneyLeg.Types
import Servant
import Tools.Auth

data AutocompleteRes = AutocompleteRes {routes :: [FRFSRouteAPI], stops :: [FRFSStationAPI]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingFareAcceptedReq = BookingFareAcceptedReq {isFareAccepted :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSBookingFeedbackReq
  = BookingFareAccepted BookingFareAcceptedReq
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSBookingPaymentAPI = FRFSBookingPaymentAPI
  { paymentOrder :: Data.Maybe.Maybe Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp,
    status :: FRFSBookingPaymentStatusAPI,
    transactionId :: Data.Maybe.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
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

data FRFSDiscountReq = FRFSDiscountReq {code :: Data.Text.Text, quantity :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSDiscountRes = FRFSDiscountRes
  { code :: Data.Text.Text,
    description :: Data.Text.Text,
    eligibility :: Kernel.Prelude.Bool,
    price :: Kernel.Types.Common.PriceAPIEntity,
    title :: Data.Text.Text,
    tnc :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSDiscoverySearchAPIReq = FRFSDiscoverySearchAPIReq {city :: Kernel.Types.Beckn.Context.City, vehicleType :: BecknV2.FRFS.Enums.VehicleCategory}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSQuoteAPIRes = FRFSQuoteAPIRes
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    discountedTickets :: Data.Maybe.Maybe Kernel.Prelude.Int,
    discounts :: Data.Maybe.Maybe [FRFSDiscountRes],
    eventDiscountAmount :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    price :: Kernel.Types.Common.HighPrecMoney,
    priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    quantity :: Kernel.Prelude.Int,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    routeStations :: Data.Maybe.Maybe [FRFSRouteStationsAPI],
    stations :: [FRFSStationAPI],
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSQuoteConfirmReq = FRFSQuoteConfirmReq {discounts :: [FRFSDiscountReq]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSRouteAPI = FRFSRouteAPI
  { code :: Data.Text.Text,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    longName :: Data.Text.Text,
    shortName :: Data.Text.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    stops :: Data.Maybe.Maybe [FRFSStationAPI],
    timeBounds :: Data.Maybe.Maybe Kernel.Types.TimeBound.TimeBound,
    totalStops :: Data.Maybe.Maybe Kernel.Prelude.Int,
    waypoints :: Data.Maybe.Maybe [Kernel.External.Maps.Types.LatLong]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSRouteStationsAPI = FRFSRouteStationsAPI
  { code :: Data.Text.Text,
    color :: Data.Maybe.Maybe Data.Text.Text,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    longName :: Data.Text.Text,
    priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    sequenceNum :: Data.Maybe.Maybe Kernel.Prelude.Int,
    shortName :: Data.Text.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    stations :: [FRFSStationAPI],
    travelTime :: Data.Maybe.Maybe Kernel.Types.Common.Seconds,
    vehicleServiceTier :: Data.Maybe.Maybe FRFSVehicleServiceTierAPI
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIReq = FRFSSearchAPIReq
  { fromStationCode :: Data.Text.Text,
    journeySearchData :: Data.Maybe.Maybe Lib.JourneyLeg.Types.JourneySearchData,
    quantity :: Kernel.Prelude.Int,
    recentLocationId :: Data.Maybe.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
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
    name :: Data.Maybe.Maybe Data.Text.Text,
    routeCodes :: Data.Maybe.Maybe [Data.Text.Text],
    sequenceNum :: Data.Maybe.Maybe Kernel.Prelude.Int,
    stationType :: Data.Maybe.Maybe Domain.Types.StationType.StationType,
    towards :: Data.Maybe.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketAPI = FRFSTicketAPI
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Data.Maybe.Maybe Data.Text.Text,
    qrData :: Data.Text.Text,
    scannedByVehicleNumber :: Data.Maybe.Maybe Data.Text.Text,
    status :: Domain.Types.FRFSTicket.FRFSTicketStatus,
    ticketNumber :: Data.Text.Text,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketBookingStatusAPIRes = FRFSTicketBookingStatusAPIRes
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    bookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    city :: Kernel.Types.Beckn.Context.City,
    createdAt :: Kernel.Prelude.UTCTime,
    discountedTickets :: Data.Maybe.Maybe Kernel.Prelude.Int,
    discounts :: Data.Maybe.Maybe [FRFSDiscountRes],
    eventDiscountAmount :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    googleWalletJWTUrl :: Data.Maybe.Maybe Data.Text.Text,
    isFareChanged :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    payment :: Data.Maybe.Maybe FRFSBookingPaymentAPI,
    price :: Kernel.Types.Common.HighPrecMoney,
    priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    quantity :: Kernel.Prelude.Int,
    routeStations :: Data.Maybe.Maybe [FRFSRouteStationsAPI],
    stations :: [FRFSStationAPI],
    status :: Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus,
    tickets :: [FRFSTicketAPI],
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketVerifyReq = FRFSTicketVerifyReq {qrData :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSVehicleServiceTierAPI = FRFSVehicleServiceTierAPI {_type :: BecknV2.FRFS.Enums.ServiceTierType, description :: Data.Text.Text, longName :: Data.Text.Text, providerCode :: Data.Text.Text, shortName :: Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
