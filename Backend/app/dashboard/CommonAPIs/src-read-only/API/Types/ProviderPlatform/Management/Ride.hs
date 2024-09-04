{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Ride where

import qualified Dashboard.Common
import qualified Dashboard.Common.Booking
import qualified Dashboard.Common.Ride
import Data.OpenApi (ToSchema)
import qualified Domain.Types
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Ticket.Interface.Types
import qualified Kernel.Prelude
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
    rideStatus :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.Status
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

data LocationAPIEntity = LocationAPIEntity
  { lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    area :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultipleRideCancelItem = MultipleRideCancelItem {rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride, reasonCode :: Dashboard.Common.Booking.CancellationReasonCode, additionalInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultipleRideCancelReq = MultipleRideCancelReq {rides :: [API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MultipleRideCancelReq where
  hideSecrets = Kernel.Prelude.identity

type MultipleRideCancelResp = Dashboard.Common.Ride.MultipleRideSyncResp

data MultipleRideData = MultipleRideData {rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride, newStatus :: API.Types.ProviderPlatform.Management.Ride.RideStatus, message :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultipleRideEndItem = MultipleRideEndItem {rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride, point :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultipleRideEndReq = MultipleRideEndReq {rides :: [API.Types.ProviderPlatform.Management.Ride.MultipleRideEndItem]}
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

newtype MultipleRideSyncRes = MultipleRideSyncRes {list :: [Kernel.Prelude.Either Kernel.Prelude.Text API.Types.ProviderPlatform.Management.Ride.MultipleRideData]}
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
    status :: API.Types.ProviderPlatform.Management.Ride.BookingStatus,
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
    customerPickupLocation :: API.Types.ProviderPlatform.Management.Ride.LocationAPIEntity,
    customerDropLocation :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.LocationAPIEntity,
    actualDropLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
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
    tripCategory :: API.Types.ProviderPlatform.Management.Ride.DeprecatedTripCategory,
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
    bookingStatus :: API.Types.ProviderPlatform.Management.Ride.BookingStatus,
    cancelledTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    cancelledBy :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.CancellationSource,
    cancellationReason :: Kernel.Prelude.Maybe Dashboard.Common.Booking.CancellationReasonCode,
    driverInitiatedCallCount :: Kernel.Prelude.Int,
    bookingToRideStartDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    distanceCalculationFailed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverDeviatedFromRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    vehicleVariant :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    vehicleServiceTierName :: Kernel.Prelude.Text,
    nextStopLocation :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.LocationAPIEntity,
    lastStopLocation :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.LocationAPIEntity,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mbDefaultServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideCity :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideCreatedAt :: Kernel.Prelude.UTCTime,
    stopLocations :: [API.Types.ProviderPlatform.Management.Ride.LocationAPIEntity]
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
    tripCategory :: API.Types.ProviderPlatform.Management.Ride.DeprecatedTripCategory,
    tripCategoryV2 :: Domain.Types.TripCategory,
    vehicleNo :: Kernel.Prelude.Text,
    fareDiff :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    fareDiffWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    bookingStatus :: API.Types.ProviderPlatform.Management.Ride.BookingStatus,
    rideCreatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListRes = RideListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, rides :: [API.Types.ProviderPlatform.Management.Ride.RideListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype RideRouteRes = RideRouteRes {actualRoute :: [API.Types.ProviderPlatform.Management.Ride.ActualRoute]}
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
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideSyncRes = RideSyncRes {newStatus :: API.Types.ProviderPlatform.Management.Ride.RideStatus, message :: Kernel.Prelude.Text}
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

newtype TicketRideListRes = TicketRideListRes {rides :: [API.Types.ProviderPlatform.Management.Ride.RideInfo]}
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

type API = ("ride" :> (GetRideList :<|> PostRideEndMultiple :<|> PostRideCancelMultiple :<|> GetRideInfo :<|> PostRideSync :<|> PostRideSyncMultiple :<|> PostRideRoute :<|> GetRideKaptureList))

type GetRideList =
  ( "list" :> QueryParam "bookingStatus" API.Types.ProviderPlatform.Management.Ride.BookingStatus :> QueryParam "currency" Kernel.Types.Common.Currency
      :> QueryParam
           "customerPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam "driverPhoneNo" Kernel.Prelude.Text
      :> QueryParam
           "fareDiff"
           Kernel.Types.Common.HighPrecMoney
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
           API.Types.ProviderPlatform.Management.Ride.RideListRes
  )

type PostRideEndMultiple = ("end" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Ride.MultipleRideEndReq :> Post '[JSON] API.Types.ProviderPlatform.Management.Ride.MultipleRideEndResp)

type PostRideCancelMultiple =
  ( "cancel" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelReq
      :> Post
           '[JSON]
           API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelResp
  )

type GetRideInfo = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "info" :> Get '[JSON] API.Types.ProviderPlatform.Management.Ride.RideInfoRes)

type PostRideSync = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "sync" :> Post '[JSON] API.Types.ProviderPlatform.Management.Ride.RideSyncRes)

type PostRideSyncMultiple =
  ( "sync" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncReq
      :> Post
           '[JSON]
           API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncRes
  )

type PostRideRoute = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "route" :> Post '[JSON] API.Types.ProviderPlatform.Management.Ride.RideRouteRes)

type GetRideKaptureList =
  ( "kapture" :> "list" :> QueryParam "rideShortId" (Kernel.Types.Id.ShortId Dashboard.Common.Ride) :> QueryParam "countryCode" Kernel.Prelude.Text
      :> QueryParam
           "phoneNumber"
           Kernel.Prelude.Text
      :> QueryParam "supportPhoneNumber" Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Ride.TicketRideListRes
  )

data RideAPIs = RideAPIs
  { getRideList :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.BookingStatus -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Ride.RideListRes,
    postRideEndMultiple :: API.Types.ProviderPlatform.Management.Ride.MultipleRideEndReq -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Ride.MultipleRideEndResp,
    postRideCancelMultiple :: API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelReq -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelResp,
    getRideInfo :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Ride.RideInfoRes,
    postRideSync :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Ride.RideSyncRes,
    postRideSyncMultiple :: API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncReq -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncRes,
    postRideRoute :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Ride.RideRouteRes,
    getRideKaptureList :: Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Ride.TicketRideListRes
  }

mkRideAPIs :: (Client EulerHS.Types.EulerClient API -> RideAPIs)
mkRideAPIs rideClient = (RideAPIs {..})
  where
    getRideList :<|> postRideEndMultiple :<|> postRideCancelMultiple :<|> getRideInfo :<|> postRideSync :<|> postRideSyncMultiple :<|> postRideRoute :<|> getRideKaptureList = rideClient

data RideEndpointDSL
  = GetRideListEndpoint
  | PostRideEndMultipleEndpoint
  | PostRideCancelMultipleEndpoint
  | GetRideInfoEndpoint
  | PostRideSyncEndpoint
  | PostRideSyncMultipleEndpoint
  | PostRideRouteEndpoint
  | GetRideKaptureListEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
