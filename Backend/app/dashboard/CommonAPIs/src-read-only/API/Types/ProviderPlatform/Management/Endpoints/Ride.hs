{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Ride where

import qualified Dashboard.Common
import qualified Dashboard.Common.Booking
import qualified Dashboard.Common.Ride
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Ticket.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data ActualRoute = ActualRoute
  { lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    timestamp :: Kernel.Prelude.UTCTime,
    accuracy :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rideStatus :: Kernel.Prelude.Maybe Status
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingStatus
  = UPCOMING
  | UPCOMING_6HRS
  | ONGOING
  | ONGOING_6HRS
  | COMPLETED
  | CANCELLED
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  | ByFleetOwner
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeprecatedTripCategory
  = OneWay
  | Rental
  | RideShare
  | InterCity
  | CrossCity
  | Ambulance
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverEdaKafka = DriverEdaKafka
  { driver_id :: Kernel.Prelude.String,
    rid :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    ts :: Kernel.Prelude.String,
    acc :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    rideStatus :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    mid :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    updated_at :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    created_at :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    on_ride :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    active :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    partition_date :: Kernel.Prelude.String,
    date :: Kernel.Prelude.String
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FParamsAmbulanceDetails = FParamsAmbulanceDetails
  { platformFee :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    sgst :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    cgst :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    distBasedFare :: Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FParamsInterCityDetails = FParamsInterCityDetails
  { timeFare :: Kernel.Types.Common.PriceAPIEntity,
    distanceFare :: Kernel.Types.Common.PriceAPIEntity,
    pickupCharge :: Kernel.Types.Common.PriceAPIEntity,
    extraDistanceFare :: Kernel.Types.Common.PriceAPIEntity,
    extraTimeFare :: Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FParamsProgressiveDetails = FParamsProgressiveDetails
  { deadKmFare :: Kernel.Types.Common.Money,
    extraKmFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    deadKmFareWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    extraKmFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    rideDurationFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FParamsRentalDetails = FParamsRentalDetails
  { deadKmFare :: Kernel.Types.Common.PriceAPIEntity,
    timeFare :: Kernel.Types.Common.PriceAPIEntity,
    distanceFare :: Kernel.Types.Common.PriceAPIEntity,
    extraDistance :: Kernel.Types.Common.Meters,
    extraDistanceWithUnit :: Kernel.Types.Common.Distance,
    extraDuration :: Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FParamsSlabDetails = FParamsSlabDetails
  { platformFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    sgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    sgstWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    cgstWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FareBreakUp = FareBreakUp
  { driverSelectedFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    customerExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    serviceCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    govtCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    baseFare :: Kernel.Types.Common.Money,
    waitingCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    rideExtraTimeFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    nightShiftCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    driverSelectedFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    customerExtraFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    serviceChargeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    govtChargesWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    baseFareWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    waitingChargeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    rideExtraTimeFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    nightShiftChargeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    nightShiftRateIfApplies :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    fareParametersDetails :: FareParametersDetails,
    customerCancellationDues :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    congestionCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    customerCancellationDuesWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    tollChargesWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    congestionChargeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FareBreakUpRes = FareBreakUpRes {estimatedFareBreakUp :: Kernel.Prelude.Maybe FareBreakUp, actualFareBreakUp :: Kernel.Prelude.Maybe FareBreakUp}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FareBreakUpRes where
  hideSecrets = Kernel.Prelude.identity

data FareParametersDetails
  = ProgressiveDetails FParamsProgressiveDetails
  | SlabDetails FParamsSlabDetails
  | RentalDetails FParamsRentalDetails
  | InterCityDetails FParamsInterCityDetails
  | AmbulanceDetails FParamsAmbulanceDetails
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationAPIEntity = LocationAPIEntity
  { lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultipleRideCancelItem = MultipleRideCancelItem {rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride, reasonCode :: Dashboard.Common.Booking.CancellationReasonCode, additionalInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultipleRideCancelReq = MultipleRideCancelReq {rides :: [MultipleRideCancelItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MultipleRideCancelReq where
  hideSecrets = Kernel.Prelude.identity

type MultipleRideCancelResp = Dashboard.Common.Ride.MultipleRideSyncResp

data MultipleRideData = MultipleRideData {rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride, newStatus :: RideStatus, message :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultipleRideEndItem = MultipleRideEndItem {rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride, point :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultipleRideEndReq = MultipleRideEndReq {rides :: [MultipleRideEndItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MultipleRideEndReq where
  hideSecrets = Kernel.Prelude.identity

type MultipleRideEndResp = Dashboard.Common.Ride.MultipleRideSyncResp

newtype MultipleRideSyncReq = MultipleRideSyncReq {rideIds :: [Kernel.Types.Id.Id Dashboard.Common.Ride]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MultipleRideSyncReq where
  hideSecrets = Kernel.Prelude.identity

newtype MultipleRideSyncRes = MultipleRideSyncRes {list :: [Kernel.Prelude.Either Kernel.Prelude.Text MultipleRideData]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MultipleRideSyncRes where
  hideSecrets = Kernel.Prelude.identity

data RideInfo = RideInfo
  { rideShortId :: Kernel.Types.Id.ShortId Dashboard.Common.Ride,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhoneNo :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Text,
    status :: BookingStatus,
    rideCreatedAt :: Kernel.Prelude.UTCTime,
    pickupLocationLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    pickupLocationLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    pickupLocationStreet :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupLocationCity :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupLocationState :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupLocationCountry :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupLocationBuilding :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupLocationAreaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupLocationArea :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropLocationLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    dropLocationLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    dropLocationStreet :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropLocationCity :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropLocationState :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropLocationCountry :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropLocationBuilding :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropLocationAreaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropLocationArea :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    fareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    personId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    classification :: Kernel.External.Ticket.Interface.Types.Classification
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

data RideInfoRes = RideInfoRes
  { rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhoneNo :: Kernel.Prelude.Text,
    rideOtp :: Kernel.Prelude.Text,
    customerPickupLocation :: LocationAPIEntity,
    customerDropLocation :: Kernel.Prelude.Maybe LocationAPIEntity,
    actualDropLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    isDestinationEdited :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Text,
    driverStartLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    driverCurrentLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    rideBookingTime :: Kernel.Prelude.UTCTime,
    estimatedDriverArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    actualDriverArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripCategory :: DeprecatedTripCategory,
    tripCategoryV2 :: Domain.Types.TripCategory,
    scheduledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideDistanceEstimated :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    rideDistanceActual :: Kernel.Types.Common.Meters,
    chargeableDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    maxEstimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    rideDistanceEstimatedWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    rideDistanceActualWithUnit :: Kernel.Types.Common.Distance,
    chargeableDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    maxEstimatedDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    estimatedRideDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    pickupDropOutsideOfThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    estimatedFare :: Kernel.Types.Common.Money,
    actualFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    driverOfferedFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    estimatedFareWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    actualFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    driverOfferedFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    pickupDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    rideDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    bookingStatus :: BookingStatus,
    cancelledTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    cancelledBy :: Kernel.Prelude.Maybe CancellationSource,
    cancellationReason :: Kernel.Prelude.Maybe Dashboard.Common.Booking.CancellationReasonCode,
    driverInitiatedCallCount :: Kernel.Prelude.Int,
    bookingToRideStartDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    distanceCalculationFailed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverDeviatedFromRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    vehicleVariant :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    vehicleServiceTierName :: Kernel.Prelude.Text,
    nextStopLocation :: Kernel.Prelude.Maybe LocationAPIEntity,
    lastStopLocation :: Kernel.Prelude.Maybe LocationAPIEntity,
    stopInformation :: Kernel.Prelude.Maybe [StopInformation],
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mbDefaultServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideCity :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideCreatedAt :: Kernel.Prelude.UTCTime,
    rideStatus :: RideStatus,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    deliveryParcelImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimatedReservedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    isPetRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    cancellationPenaltyAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cancellationPenaltyWaivedReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListItem = RideListItem
  { rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    rideShortId :: Kernel.Types.Id.ShortId Dashboard.Common.Ride,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhoneNo :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripCategory :: DeprecatedTripCategory,
    tripCategoryV2 :: Domain.Types.TripCategory,
    vehicleNo :: Kernel.Prelude.Text,
    fareDiff :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    fareDiffWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    bookingStatus :: BookingStatus,
    rideCreatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListItemV2 = RideListItemV2
  { rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    rideShortId :: Kernel.Types.Id.ShortId Dashboard.Common.Ride,
    driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideStatus :: RideStatus,
    rideCreatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListRes = RideListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, rides :: [RideListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListResV2 = RideListResV2 {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, rides :: [RideListItemV2]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype RideRouteRes = RideRouteRes {actualRoute :: [ActualRoute]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RideRouteRes where
  hideSecrets = Kernel.Prelude.identity

data RideStatus
  = RIDE_UPCOMING
  | RIDE_NEW
  | RIDE_INPROGRESS
  | RIDE_COMPLETED
  | RIDE_CANCELLED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data RideSyncRes = RideSyncRes {newStatus :: RideStatus, message :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RideSyncRes where
  hideSecrets = Kernel.Prelude.identity

data Status
  = ON_RIDE
  | ON_PICKUP
  | IDLE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StopInformation = StopInformation
  { stopId :: Kernel.Prelude.Text,
    stopLocId :: Kernel.Prelude.Text,
    stopOrder :: Kernel.Prelude.Int,
    waitingTimeStart :: Kernel.Prelude.UTCTime,
    waitingTimeEnd :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    stopStartLatLng :: Kernel.External.Maps.Types.LatLong,
    stopEndLatLng :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype TicketRideListRes = TicketRideListRes {rides :: [RideInfo]}
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

data WaiverRideCancellationPenaltyReq = WaiverRideCancellationPenaltyReq {reason :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("ride" :> (GetRideListHelper :<|> GetRideAgentList :<|> GetRideListV2 :<|> PostRideEndMultiple :<|> PostRideCancelMultiple :<|> GetRideInfo :<|> PostRideSync :<|> PostRideSyncMultiple :<|> PostRideRoute :<|> GetRideKaptureList :<|> GetRideFareBreakUp :<|> PostRideWaiverRideCancellationPenalty))

type GetRideList =
  ( "list" :> QueryParam "bookingStatus" BookingStatus :> QueryParam "currency" Kernel.Types.Common.Currency
      :> QueryParam
           "customerPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam "driverPhoneNo" Kernel.Prelude.Text
      :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "rideShortId"
           (Kernel.Types.Id.ShortId Dashboard.Common.Ride)
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           RideListRes
  )

type GetRideListHelper =
  ( Capture "requestorId" Kernel.Prelude.Text :> "list" :> QueryParam "bookingStatus" BookingStatus
      :> QueryParam
           "currency"
           Kernel.Types.Common.Currency
      :> QueryParam "customerPhoneNo" Kernel.Prelude.Text
      :> QueryParam "driverPhoneNo" Kernel.Prelude.Text
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "rideShortId"
           (Kernel.Types.Id.ShortId Dashboard.Common.Ride)
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           RideListRes
  )

type GetRideAgentList =
  ( "agent" :> "list" :> QueryParam "bookingStatus" BookingStatus :> QueryParam "currency" Kernel.Types.Common.Currency
      :> QueryParam
           "customerPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam "driverPhoneNo" Kernel.Prelude.Text
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "rideShortId"
           (Kernel.Types.Id.ShortId Dashboard.Common.Ride)
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "vehicleNo"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           RideListRes
  )

type GetRideListV2 =
  ( "listV2" :> QueryParam "currency" Kernel.Types.Common.Currency :> QueryParam "customerPhoneNo" Kernel.Prelude.Text
      :> QueryParam
           "driverPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "rideShortId"
           (Kernel.Types.Id.ShortId Dashboard.Common.Ride)
      :> QueryParam
           "rideStatus"
           RideStatus
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           RideListResV2
  )

type PostRideEndMultiple = ("end" :> ReqBody '[JSON] MultipleRideEndReq :> Post '[JSON] MultipleRideEndResp)

type PostRideCancelMultiple = ("cancel" :> ReqBody '[JSON] MultipleRideCancelReq :> Post '[JSON] MultipleRideCancelResp)

type GetRideInfo = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "info" :> Get '[JSON] RideInfoRes)

type PostRideSync = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "sync" :> Post '[JSON] RideSyncRes)

type PostRideSyncMultiple = ("sync" :> ReqBody '[JSON] MultipleRideSyncReq :> Post '[JSON] MultipleRideSyncRes)

type PostRideRoute = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "route" :> Post '[JSON] RideRouteRes)

type GetRideKaptureList =
  ( "kapture" :> "list" :> QueryParam "rideShortId" (Kernel.Types.Id.ShortId Dashboard.Common.Ride) :> QueryParam "countryCode" Kernel.Prelude.Text
      :> QueryParam
           "phoneNumber"
           Kernel.Prelude.Text
      :> QueryParam "supportPhoneNumber" Kernel.Prelude.Text
      :> Get '[JSON] TicketRideListRes
  )

type GetRideFareBreakUp = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "fareBreakUp" :> Get '[JSON] FareBreakUpRes)

type PostRideWaiverRideCancellationPenalty =
  ( Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "waiverRideCancellationPenalty"
      :> ReqBody
           '[JSON]
           WaiverRideCancellationPenaltyReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data RideAPIs = RideAPIs
  { getRideList :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe BookingStatus -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient RideListRes,
    getRideAgentList :: Kernel.Prelude.Maybe BookingStatus -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient RideListRes,
    getRideListV2 :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe RideStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient RideListResV2,
    postRideEndMultiple :: MultipleRideEndReq -> EulerHS.Types.EulerClient MultipleRideEndResp,
    postRideCancelMultiple :: MultipleRideCancelReq -> EulerHS.Types.EulerClient MultipleRideCancelResp,
    getRideInfo :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient RideInfoRes,
    postRideSync :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient RideSyncRes,
    postRideSyncMultiple :: MultipleRideSyncReq -> EulerHS.Types.EulerClient MultipleRideSyncRes,
    postRideRoute :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient RideRouteRes,
    getRideKaptureList :: Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient TicketRideListRes,
    getRideFareBreakUp :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient FareBreakUpRes,
    postRideWaiverRideCancellationPenalty :: Kernel.Types.Id.Id Dashboard.Common.Ride -> WaiverRideCancellationPenaltyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkRideAPIs :: (Client EulerHS.Types.EulerClient API -> RideAPIs)
mkRideAPIs rideClient = (RideAPIs {..})
  where
    getRideList :<|> getRideAgentList :<|> getRideListV2 :<|> postRideEndMultiple :<|> postRideCancelMultiple :<|> getRideInfo :<|> postRideSync :<|> postRideSyncMultiple :<|> postRideRoute :<|> getRideKaptureList :<|> getRideFareBreakUp :<|> postRideWaiverRideCancellationPenalty = rideClient

data RideUserActionType
  = GET_RIDE_LIST
  | GET_RIDE_AGENT_LIST
  | GET_RIDE_LIST_V2
  | POST_RIDE_END_MULTIPLE
  | POST_RIDE_CANCEL_MULTIPLE
  | GET_RIDE_INFO
  | POST_RIDE_SYNC
  | POST_RIDE_SYNC_MULTIPLE
  | POST_RIDE_ROUTE
  | GET_RIDE_KAPTURE_LIST
  | GET_RIDE_FARE_BREAK_UP
  | POST_RIDE_WAIVER_RIDE_CANCELLATION_PENALTY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''RideUserActionType])
