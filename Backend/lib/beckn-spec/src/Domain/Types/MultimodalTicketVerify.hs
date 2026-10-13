{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Ticket-verify request/response tree, mirrored from rider-app's Lib.JourneyModule.Types and
-- placed here so rider-app can adopt it and the two copies stop being able to drift. rider-app
-- still serializes its own LegInfo today, so changes there must be reflected here.
module Domain.Types.MultimodalTicketVerify where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.JourneyBookingStatus
import qualified Domain.Types.JourneyLegStatus
import qualified Domain.Types.StationType
import qualified Domain.Types.TrackingStatus
import qualified Domain.Types.Trip
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.MultiModal.Interface.Types
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Common (Distance, HighPrecMoney, Meters, Price, PriceAPIEntity, Seconds)
import qualified Kernel.Types.Common
import Kernel.Types.Id (Id)
import qualified Kernel.Types.Id

-- Phantom tags: these ids cross a service boundary as opaque strings, so driver-app needs the
-- tag but never the entity.
data JourneyLeg

data FRFSTicketBooking

data Pass

data PurchasedPassPayment

data Refunds

data FRFSQuote

data FRFSQuoteCategory

data Seat

data IntegratedBPPConfig

data Merchant

data MerchantOperatingCity

data Person

data Provider = CMRL | MTC | DIRECT | CRIS
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- Request side. Hand-written instances: the JSON keys differ from the field names
-- ("type", "CMRL", "MTC", "BookingId", "data"), so a derived instance would reject real QRs.
data BookingDataV2 = BookingDataV2
  { bookingId :: Text,
    isRoundTrip :: Bool,
    ticketData :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON BookingDataV2 where
  parseJSON = withObject "BookingDataV2" $ \o -> do
    bookingId <- o .: "BookingId"
    isRoundTrip <- o .: "isRoundTrip"
    ticketData <- o .: "data"
    return $ BookingDataV2 bookingId isRoundTrip ticketData

instance ToJSON BookingDataV2 where
  toJSON (BookingDataV2 bookingId isRoundTrip ticketData) =
    object
      [ "BookingId" .= bookingId,
        "isRoundTrip" .= isRoundTrip,
        "data" .= ticketData
      ]

data UnifiedTicketQRV2 = UnifiedTicketQRV2
  { version :: Text,
    _type :: Text,
    txnId :: Text,
    createdAt :: UTCTime,
    cmrl :: [BookingDataV2],
    mtc :: [BookingDataV2]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON UnifiedTicketQRV2 where
  parseJSON = withObject "UnifiedTicketQRV2" $ \o -> do
    version <- o .: "version"
    _type <- o .: "type"
    txnId <- o .: "txnId"
    createdAt <- o .: "createdAt"
    cmrl <- o .: "CMRL"
    mtc <- o .: "MTC"
    return $ UnifiedTicketQRV2 version _type txnId createdAt cmrl mtc

instance ToJSON UnifiedTicketQRV2 where
  toJSON (UnifiedTicketQRV2 version _type txnId createdAt cmrl mtc) =
    object
      [ "version" .= version,
        "type" .= _type,
        "txnId" .= txnId,
        "createdAt" .= createdAt,
        "CMRL" .= cmrl,
        "MTC" .= mtc
      ]

data FRFSStationAPI = FRFSStationAPI
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    code :: Kernel.Prelude.Text,
    color :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    integratedBppConfigId :: Kernel.Types.Id.Id IntegratedBPPConfig,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    parentStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeCodes :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    routeDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sequenceNum :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stationType :: Kernel.Prelude.Maybe Domain.Types.StationType.StationType,
    timeTakenToTravelUpcomingStop :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    towards :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSDiscountRes = FRFSDiscountRes
  { code :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    eligibility :: Kernel.Prelude.Bool,
    price :: Kernel.Types.Common.PriceAPIEntity,
    title :: Kernel.Prelude.Text,
    tnc :: Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSPassOptionAPIEntity = FRFSPassOptionAPIEntity
  { availableTripCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxTicketQuantityPerOverride :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    overriddenTotalPrice :: Kernel.Types.Common.PriceAPIEntity,
    overriddenUnitPrice :: Kernel.Types.Common.PriceAPIEntity,
    passId :: Kernel.Types.Id.Id Pass,
    passName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    purchasedPassPaymentId :: Kernel.Types.Id.Id PurchasedPassPayment,
    unlimitedTripCount :: Kernel.Prelude.Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QuoteCategoryMetadata = QuoteCategoryMetadata
  { categoryOrder :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    code :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text,
    tnc :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CategoryInfoResponse = CategoryInfoResponse
  { categoryFinalPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    categoryId :: Kernel.Types.Id.Id FRFSQuoteCategory,
    categoryMeta :: Kernel.Prelude.Maybe QuoteCategoryMetadata,
    categoryName :: Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType,
    categoryOfferedPrice :: Kernel.Types.Common.PriceAPIEntity,
    categoryPrice :: Kernel.Types.Common.PriceAPIEntity,
    categorySelectedQuantity :: Kernel.Prelude.Int,
    seatIds :: Kernel.Prelude.Maybe [Kernel.Types.Id.Id Seat],
    seatLabels :: Kernel.Prelude.Maybe [Kernel.Prelude.Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CategoryBookingDetails = CategoryBookingDetails
  { categoryName :: Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType,
    categorySelectedQuantity :: Int,
    categoryFinalPrice :: Maybe PriceAPIEntity
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SeatSelectionType = UNRESERVED | AUTO_ASSIGNED | USER_SELECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data QREncoding = LATIN1
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq, ToSchema)

data LegRefundInfo = LegRefundInfo
  { id :: Id Refunds,
    amount :: HighPrecMoney,
    status :: Payment.RefundStatus,
    arn :: Maybe Text,
    completedAt :: Maybe UTCTime,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegServiceTier = LegServiceTier
  { serviceTierName :: Text,
    serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType,
    serviceTierDescription :: Text,
    routeCode :: Maybe Text,
    via :: Maybe Text,
    trainTypeCode :: Maybe Text,
    ticketTypeCode :: Maybe Text,
    fare :: Maybe PriceAPIEntity,
    quoteId :: Id FRFSQuote
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegRouteInfo = LegRouteInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    routeCode :: Text,
    subOrder :: Maybe Int,
    platformNumber :: Maybe Text,
    lineColor :: Maybe Text,
    lineColorCode :: Maybe Text,
    trainNumber :: Maybe Text,
    journeyStatus :: Maybe Domain.Types.JourneyLegStatus.JourneyLegStatus,
    trackingStatus :: Domain.Types.TrackingStatus.TrackingStatus,
    trackingStatusLastUpdatedAt :: UTCTime,
    frequency :: Maybe Seconds,
    legStartTime :: Maybe UTCTime,
    legEndTime :: Maybe UTCTime,
    allAvailableRoutes :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroLegExtraInfo = MetroLegExtraInfo
  { routeInfo :: [LegRouteInfo],
    bppOrderId :: Maybe Text,
    bookingId :: Maybe (Id FRFSTicketBooking),
    tickets :: Maybe [Text],
    ticketValidity :: Maybe [UTCTime],
    ticketsCreatedAt :: Maybe [UTCTime],
    providerName :: Maybe Text,
    ticketNo :: Maybe [Text],
    adultTicketQuantity :: Maybe Int,
    childTicketQuantity :: Maybe Int,
    refund :: Maybe LegRefundInfo,
    refunds :: [LegRefundInfo],
    categories :: [CategoryInfoResponse],
    categoryBookingDetails :: Maybe [CategoryBookingDetails], -- TODO :: To be deprecated once UI starts consuming `categories` instead as this is redundant data.
    qrEncoding :: Maybe QREncoding
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BusLegExtraInfo = BusLegExtraInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    routeCode :: Text,
    bookingId :: Maybe (Id FRFSTicketBooking),
    tickets :: Maybe [Text],
    ticketValidity :: Maybe [UTCTime],
    ticketsCreatedAt :: Maybe [UTCTime],
    routeName :: Maybe Text,
    providerName :: Maybe Text,
    selectedServiceTier :: Maybe LegServiceTier,
    frequency :: Maybe Seconds,
    alternateShortNames :: [Text],
    ticketNo :: Maybe [Text],
    adultTicketQuantity :: Maybe Int,
    childTicketQuantity :: Maybe Int,
    refund :: Maybe LegRefundInfo,
    refunds :: [LegRefundInfo],
    trackingStatus :: Domain.Types.TrackingStatus.TrackingStatus,
    trackingStatusLastUpdatedAt :: UTCTime,
    fleetNo :: Maybe Text,
    legStartTime :: Maybe UTCTime,
    legEndTime :: Maybe UTCTime,
    discounts :: Maybe [FRFSDiscountRes],
    categories :: [CategoryInfoResponse],
    categoryBookingDetails :: Maybe [CategoryBookingDetails], -- TODO :: To be deprecated once UI starts consuming `categories` instead as this is redundant data.
    busConductorId :: Maybe Text,
    busDriverId :: Maybe Text,
    busTagNumber :: Maybe Text,
    tripId :: Maybe Text,
    tripStartTime :: Maybe [UTCTime],
    bookedStopETA :: Maybe [UTCTime],
    driverName :: Maybe Text,
    driverMobileNumber :: Maybe Text,
    seatSelectionType :: Maybe SeatSelectionType
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubwayLegExtraInfo = SubwayLegExtraInfo
  { routeInfo :: [LegRouteInfo],
    bookingId :: Maybe (Id FRFSTicketBooking),
    tickets :: Maybe [Text],
    ticketValidity :: Maybe [UTCTime],
    ticketsCreatedAt :: Maybe [UTCTime],
    ticketValidityHours :: [Int],
    providerName :: Maybe Text,
    providerRouteId :: Maybe Text,
    deviceId :: Maybe Text,
    ticketTypeCode :: Maybe Text,
    selectedServiceTier :: Maybe LegServiceTier,
    ticketNo :: Maybe [Text],
    adultTicketQuantity :: Maybe Int,
    childTicketQuantity :: Maybe Int,
    refund :: Maybe LegRefundInfo,
    refunds :: [LegRefundInfo],
    categories :: [CategoryInfoResponse],
    categoryBookingDetails :: Maybe [CategoryBookingDetails] -- TODO :: To be deprecated once UI starts consuming `categories` instead as this is redundant data.
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- Only the FRFS arms: mkLegInfoFromFrfsBooking cases on METRO/BUS/SUBWAY, and the Walk/Taxi
-- arms are built by other journey builders this endpoint never calls.
data LegExtraInfo = Metro MetroLegExtraInfo | Bus BusLegExtraInfo | Subway SubwayLegExtraInfo
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AppliedPassInfo = AppliedPassInfo
  { purchasedPassPaymentId :: Id PurchasedPassPayment,
    passId :: Id Pass,
    passName :: Maybe Text,
    overriddenTotalFare :: Maybe PriceAPIEntity
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegInfo = LegInfo
  { journeyLegId :: Id JourneyLeg,
    applicablePasses :: [FRFSPassOptionAPIEntity],
    appliedPass :: Maybe AppliedPassInfo,
    skipBooking :: Bool,
    bookingAllowed :: Bool,
    pricingId :: Maybe Text,
    searchId :: Text,
    travelMode :: Domain.Types.Trip.MultimodalTravelMode,
    startTime :: UTCTime,
    order :: Int,
    status :: Domain.Types.JourneyLegStatus.JourneyLegStatus, -- TODO :: To be Deprecated, remove this once UI starts consuming `legStatus` instead.
    bookingStatus :: Domain.Types.JourneyBookingStatus.JourneyBookingStatus,
    estimatedDuration :: Maybe Seconds,
    estimatedMinFare :: Maybe PriceAPIEntity,
    estimatedMaxFare :: Maybe PriceAPIEntity,
    estimatedChildFare :: Maybe PriceAPIEntity,
    estimatedTotalFare :: Maybe PriceAPIEntity,
    estimatedDistance :: Maybe Distance,
    legExtraInfo :: LegExtraInfo,
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    personId :: Id Person,
    actualDistance :: Maybe Distance,
    totalFare :: Maybe PriceAPIEntity,
    entrance :: Maybe Kernel.External.MultiModal.Interface.Types.MultiModalLegGate,
    exit :: Maybe Kernel.External.MultiModal.Interface.Types.MultiModalLegGate,
    validTill :: Maybe UTCTime,
    hasApplicablePasses :: Maybe Bool,
    observingFailures :: Maybe Bool,
    isCancellable :: Maybe Bool,
    isReschedulable :: Maybe Bool,
    remainingRescheduleCount :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultimodalTicketVerifyResp = MultimodalTicketVerifyResp
  { provider :: Provider,
    legInfo :: [LegInfo]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
