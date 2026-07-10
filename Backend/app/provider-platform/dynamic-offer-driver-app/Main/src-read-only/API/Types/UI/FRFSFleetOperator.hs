{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FRFSFleetOperator where

import qualified BecknV2.FRFS.Enums
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.FRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus
import qualified Domain.Types.FleetOperatorTripAction
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.TimeBound
import Servant
import Tools.Auth

data CrisSdkResponse = CrisSdkResponse {bookAuthCode :: Data.Text.Text, latency :: Kernel.Prelude.Maybe Kernel.Prelude.Int, osBuildVersion :: Data.Text.Text, osType :: Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSBookingPaymentAPI = FRFSBookingPaymentAPI
  { paymentOrder :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp,
    status :: FRFSBookingPaymentStatusAPI,
    transactionId :: Kernel.Prelude.Maybe Data.Text.Text
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
  | REFUND_FAILED
  | REFUND_INITIATED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSBookingSearchReq = FRFSBookingSearchReq {feedKey :: Data.Text.Text, operatorBadgeToken :: Data.Text.Text, searchReq :: FRFSSearchAPIReq, vehicleType :: BecknV2.FRFS.Enums.VehicleCategory}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSCategorySelectionReq = FRFSCategorySelectionReq {quantity :: Kernel.Prelude.Int, quoteCategoryId :: Data.Text.Text, seatIds :: Kernel.Prelude.Maybe [Data.Text.Text]}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSQuoteAPIRes = FRFSQuoteAPIRes
  { _type :: FRFSQuoteType,
    categories :: [Data.Aeson.Value],
    discountedTickets :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    eventDiscountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    integratedBppConfigId :: Data.Text.Text,
    observingFailures :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    offer :: Kernel.Prelude.Maybe Data.Aeson.Value,
    price :: Kernel.Types.Common.HighPrecMoney,
    priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    quantity :: Kernel.Prelude.Int,
    quoteId :: Data.Text.Text,
    routeCode :: Kernel.Prelude.Maybe Data.Text.Text,
    routeStations :: Kernel.Prelude.Maybe [Data.Aeson.Value],
    serviceTierName :: Kernel.Prelude.Maybe Data.Text.Text,
    serviceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    stations :: [FRFSStationAPI],
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSQuoteCategoryAPIEntity = FRFSQuoteCategoryAPIEntity
  { bppItemId :: Data.Text.Text,
    categoryMetadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    finalPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    offeredPrice :: Kernel.Types.Common.PriceAPIEntity,
    price :: Kernel.Types.Common.PriceAPIEntity,
    seatIds :: Kernel.Prelude.Maybe [Data.Text.Text],
    seatLabels :: Kernel.Prelude.Maybe [Data.Text.Text],
    selectedQuantity :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSQuoteConfirmReq = FRFSQuoteConfirmReq
  { childTicketQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    crisSdkResponse :: Kernel.Prelude.Maybe CrisSdkResponse,
    enableOffer :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    forceAutoAssignSeat :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    offered :: Kernel.Prelude.Maybe [FRFSCategorySelectionReq],
    ticketQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    tripId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSQuoteType
  = SingleJourney
  | ReturnJourney
  | Pass
  | SpecialFareSingleJourney
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSRouteAPI = FRFSRouteAPI
  { code :: Data.Text.Text,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    integratedBppConfigId :: Data.Text.Text,
    longName :: Data.Text.Text,
    shortName :: Data.Text.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    stops :: Kernel.Prelude.Maybe [FRFSStationAPI],
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    totalStops :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    waypoints :: Kernel.Prelude.Maybe [Kernel.External.Maps.Types.LatLong]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIReq = FRFSSearchAPIReq
  { fromStationCode :: Data.Text.Text,
    quantity :: Kernel.Prelude.Int,
    routeCode :: Kernel.Prelude.Maybe Data.Text.Text,
    searchAsParentStops :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    serviceTier :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    toStationCode :: Data.Text.Text,
    vehicleNumber :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIRes = FRFSSearchAPIRes {quotes :: [FRFSQuoteAPIRes], searchId :: Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSStationAPI = FRFSStationAPI
  { address :: Kernel.Prelude.Maybe Data.Text.Text,
    code :: Data.Text.Text,
    color :: Kernel.Prelude.Maybe Data.Text.Text,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    integratedBppConfigId :: Data.Text.Text,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    name :: Kernel.Prelude.Maybe Data.Text.Text,
    parentStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    routeCodes :: Kernel.Prelude.Maybe [Data.Text.Text],
    sequenceNum :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stationType :: Kernel.Prelude.Maybe Data.Text.Text,
    timeTakenToTravelUpcomingStop :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    towards :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketAPI = FRFSTicketAPI
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Data.Text.Text,
    isReturnTicket :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    qrData :: Data.Text.Text,
    scannedByVehicleNumber :: Kernel.Prelude.Maybe Data.Text.Text,
    status :: Domain.Types.FRFSTicketStatus.FRFSTicketStatus,
    ticketNumber :: Data.Text.Text,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketBookingStatusAPIRes = FRFSTicketBookingStatusAPIRes
  { _type :: FRFSQuoteType,
    bookingId :: Data.Text.Text,
    city :: Kernel.Types.Beckn.Context.City,
    createdAt :: Kernel.Prelude.UTCTime,
    discountedTickets :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    eventDiscountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    googleWalletJWTUrl :: Kernel.Prelude.Maybe Data.Text.Text,
    integratedBppConfigId :: Data.Text.Text,
    isFareChanged :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    payment :: Kernel.Prelude.Maybe FRFSBookingPaymentAPI,
    price :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    priceWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    quantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    quoteCategories :: [FRFSQuoteCategoryAPIEntity],
    routeStations :: Kernel.Prelude.Maybe [Data.Aeson.Value],
    stations :: [FRFSStationAPI],
    status :: Domain.Types.FRFSTicketBookingStatus.FRFSTicketBookingStatus,
    tickets :: [FRFSTicketAPI],
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTripPassengerManifestResp = FRFSTripPassengerManifestResp {manifest :: [PassengerStopManifest]}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOperatorCurrentOperationReq = FleetOperatorCurrentOperationReq {gimsConductorId :: Kernel.Prelude.Maybe Data.Text.Text, gimsDriverId :: Kernel.Prelude.Maybe Data.Text.Text, vehicleNumber :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOperatorCurrentOperationResp = FleetOperatorCurrentOperationResp
  { current :: Kernel.Prelude.Maybe OperatorTripInfo,
    gimsConductorId :: Kernel.Prelude.Maybe Data.Text.Text,
    gimsDriverId :: Kernel.Prelude.Maybe Data.Text.Text,
    gtfsId :: Data.Text.Text,
    history :: [OperatorTripInfo],
    upcoming :: [OperatorTripInfo],
    vehicleNumber :: Data.Text.Text,
    waybillNo :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOperatorTripActionReq = FleetOperatorTripActionReq
  { action :: Domain.Types.FleetOperatorTripAction.FleetOperatorTripAction,
    gimsConductorId :: Kernel.Prelude.Maybe Data.Text.Text,
    gimsDriverId :: Kernel.Prelude.Maybe Data.Text.Text,
    vehicleNumber :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOperatorTripActionResp = FleetOperatorTripActionResp {currentTripNumber :: Kernel.Prelude.Int, hasUpcomingTrips :: Kernel.Prelude.Bool}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OperatorTripInfo = OperatorTripInfo
  { dutyDate :: Kernel.Prelude.Maybe Data.Text.Text,
    endTime :: Kernel.Prelude.Maybe Data.Text.Text,
    isActiveTrip :: Kernel.Prelude.Bool,
    routeId :: Data.Text.Text,
    routeName :: Data.Text.Text,
    routeNumber :: Data.Text.Text,
    startTime :: Kernel.Prelude.Maybe Data.Text.Text,
    tripNumber :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassengerInfo = PassengerInfo
  { bookingId :: Data.Text.Text,
    checkedIn :: Kernel.Prelude.Bool,
    conductorToken :: Kernel.Prelude.Maybe Data.Text.Text,
    name :: Data.Text.Text,
    personId :: Data.Text.Text,
    phone :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassengerStopManifest = PassengerStopManifest {alightingPassengers :: [PassengerInfo], boardingPassengers :: [PassengerInfo], stopCode :: Data.Text.Text, stopName :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
