{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FRFSTicketService where

import qualified API.Types.UI.RiderLocation
import qualified BecknV2.FRFS.Enums
import qualified Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSQuoteCategory
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus
import qualified Domain.Types.IntegratedBPPConfig
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
import Servant
import Tools.Auth

data AutocompleteRes = AutocompleteRes {routes :: [FRFSRouteAPI], stops :: [FRFSStationAPI]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingFareAcceptedReq = BookingFareAcceptedReq {isFareAccepted :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingFeedbackReq = BookingFeedbackReq {feedbackDetails :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CategoryInfoResponse = CategoryInfoResponse
  { categoryFinalPrice :: Data.Maybe.Maybe Kernel.Types.Common.PriceAPIEntity,
    categoryId :: Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory,
    categoryMeta :: Data.Maybe.Maybe Domain.Types.FRFSQuoteCategory.QuoteCategoryMetadata,
    categoryName :: Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType,
    categoryOfferedPrice :: Kernel.Types.Common.PriceAPIEntity,
    categoryPrice :: Kernel.Types.Common.PriceAPIEntity,
    categorySelectedQuantity :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSBookingFeedbackReq
  = BookingFareAccepted BookingFareAcceptedReq
  | BookingFeedback BookingFeedbackReq
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
  | REFUND_FAILED
  | REFUND_INITIATED
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

data FRFSCategorySelectionReq = FRFSCategorySelectionReq {quantity :: Kernel.Prelude.Int, quoteCategoryId :: Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory}
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
    providerId :: Data.Maybe.Maybe Data.Text.Text,
    roundTripTicketLimit :: Kernel.Prelude.Int,
    ticketsBookedInEvent :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
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

data FRFSPossibleStopsReq = FRFSPossibleStopsReq {stationCodes :: [Data.Text.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSQuoteAPIRes = FRFSQuoteAPIRes
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    categories :: [CategoryInfoResponse],
    discountedTickets :: Data.Maybe.Maybe Kernel.Prelude.Int,
    eventDiscountAmount :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
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

data FRFSQuoteCategoryAPIEntity = FRFSQuoteCategoryAPIEntity
  { bppItemId :: Data.Text.Text,
    categoryMetadata :: Data.Maybe.Maybe FRFSTicketCategoryMetadataAPIEntity,
    finalPrice :: Data.Maybe.Maybe Kernel.Types.Common.PriceAPIEntity,
    offeredPrice :: Kernel.Types.Common.PriceAPIEntity,
    price :: Kernel.Types.Common.PriceAPIEntity,
    selectedQuantity :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSQuoteConfirmReq = FRFSQuoteConfirmReq {childTicketQuantity :: Data.Maybe.Maybe Kernel.Prelude.Int, offered :: Data.Maybe.Maybe [FRFSCategorySelectionReq], ticketQuantity :: Data.Maybe.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSRouteAPI = FRFSRouteAPI
  { code :: Data.Text.Text,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
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
  { busLocationData :: Data.Maybe.Maybe [API.Types.UI.RiderLocation.BusLocation],
    fromStationCode :: Data.Text.Text,
    quantity :: Kernel.Prelude.Int,
    recentLocationId :: Data.Maybe.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    routeCode :: Data.Maybe.Maybe Data.Text.Text,
    searchAsParentStops :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    serviceTier :: Data.Maybe.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    toStationCode :: Data.Text.Text,
    vehicleNumber :: Data.Maybe.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIRes = FRFSSearchAPIRes {quotes :: [FRFSQuoteAPIRes], searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSStationAPI = FRFSStationAPI
  { address :: Data.Maybe.Maybe Data.Text.Text,
    code :: Data.Text.Text,
    color :: Data.Maybe.Maybe Data.Text.Text,
    distance :: Data.Maybe.Maybe Kernel.Types.Common.Meters,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    lat :: Data.Maybe.Maybe Kernel.Prelude.Double,
    lon :: Data.Maybe.Maybe Kernel.Prelude.Double,
    name :: Data.Maybe.Maybe Data.Text.Text,
    parentStopCode :: Data.Maybe.Maybe Data.Text.Text,
    routeCodes :: Data.Maybe.Maybe [Data.Text.Text],
    sequenceNum :: Data.Maybe.Maybe Kernel.Prelude.Int,
    stationType :: Data.Maybe.Maybe Domain.Types.StationType.StationType,
    timeTakenToTravelUpcomingStop :: Data.Maybe.Maybe Kernel.Types.Common.Seconds,
    towards :: Data.Maybe.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketAPI = FRFSTicketAPI
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Data.Maybe.Maybe Data.Text.Text,
    qrData :: Data.Text.Text,
    scannedByVehicleNumber :: Data.Maybe.Maybe Data.Text.Text,
    status :: Domain.Types.FRFSTicketStatus.FRFSTicketStatus,
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
    eventDiscountAmount :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    googleWalletJWTUrl :: Data.Maybe.Maybe Data.Text.Text,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    isFareChanged :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    payment :: Data.Maybe.Maybe FRFSBookingPaymentAPI,
    price :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    priceWithCurrency :: Data.Maybe.Maybe Kernel.Types.Common.PriceAPIEntity,
    quantity :: Data.Maybe.Maybe Kernel.Prelude.Int,
    quoteCategories :: [FRFSQuoteCategoryAPIEntity],
    routeStations :: Data.Maybe.Maybe [FRFSRouteStationsAPI],
    stations :: [FRFSStationAPI],
    status :: Domain.Types.FRFSTicketBookingStatus.FRFSTicketBookingStatus,
    tickets :: [FRFSTicketAPI],
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketCategoryMetadataAPIEntity = FRFSTicketCategoryMetadataAPIEntity {category :: Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType, description :: Data.Text.Text, title :: Data.Text.Text, tnc :: Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketCategoryRes = FRFSTicketCategoryRes
  { category :: Data.Text.Text,
    code :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    eligibility :: Kernel.Prelude.Bool,
    offeredPrice :: Kernel.Types.Common.PriceAPIEntity,
    price :: Kernel.Types.Common.PriceAPIEntity,
    quoteCategoryId :: Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory,
    title :: Data.Text.Text,
    tnc :: Data.Maybe.Maybe Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSTicketVerifyReq = FRFSTicketVerifyReq {qrData :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSVehicleServiceTierAPI = FRFSVehicleServiceTierAPI
  { _type :: BecknV2.FRFS.Enums.ServiceTierType,
    description :: Data.Text.Text,
    isAirConditioned :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    longName :: Data.Text.Text,
    providerCode :: Data.Text.Text,
    shortName :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
