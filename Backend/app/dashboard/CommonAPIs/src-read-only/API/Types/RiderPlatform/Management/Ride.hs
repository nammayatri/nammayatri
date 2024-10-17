{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Ride where

import qualified Dashboard.Common
import qualified Dashboard.Common.Booking
import qualified Dashboard.Common.Ride
import qualified Dashboard.RiderPlatform.Ride
import Data.OpenApi (ToSchema)
import qualified Domain.Types
import qualified Domain.Types.Booking
import qualified Domain.Types.CancellationReason
import qualified Domain.Types.VehicleVariant
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Centesimal
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Distance
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Kernel.Types.Price
import qualified Kernel.Types.Time
import Servant
import Servant.Client

data BookingCancelledReq = BookingCancelledReq
  { bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    cancellationReasonCode :: Dashboard.Common.Booking.CancellationReasonCode,
    cancellationStage :: Domain.Types.CancellationReason.CancellationStage,
    additionalInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EstimateBreakup = EstimateBreakup {title :: Kernel.Prelude.Text, price :: API.Types.RiderPlatform.Management.Ride.EstimateBreakupPrice}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype EstimateBreakupPrice = EstimateBreakupPrice {value :: Kernel.Types.Price.PriceAPIEntity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FareBreakup = FareBreakup {amount :: Kernel.Types.Common.Price, description :: Kernel.Prelude.Text, entityId :: Kernel.Prelude.Text, entityType :: API.Types.RiderPlatform.Management.Ride.FareBreakupEntityType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FareBreakupEntityType
  = BOOKING_UPDATE_REQUEST
  | BOOKING
  | RIDE
  | INITIAL_BOOKING
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultipleRideCancelReq = MultipleRideCancelReq {multipleRideCancelInfo :: [API.Types.RiderPlatform.Management.Ride.BookingCancelledReq]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MultipleRideCancelReq where
  hideSecrets = Kernel.Prelude.identity

newtype MultipleRideItem = MultipleRideItem {rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultipleRideSyncReq = MultipleRideSyncReq {rides :: [API.Types.RiderPlatform.Management.Ride.MultipleRideItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MultipleRideSyncReq where
  hideSecrets = Kernel.Prelude.identity

data RideInfoRes = RideInfoRes
  { rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    rideStatus :: API.Types.RiderPlatform.Management.Ride.RideStatus,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideOtp :: Kernel.Prelude.Text,
    customerPickupLocation :: Dashboard.RiderPlatform.Ride.Location,
    customerDropLocation :: Kernel.Prelude.Maybe Dashboard.RiderPlatform.Ride.Location,
    driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverRegisteredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleNo :: Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Text,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
    vehicleServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideBookingTime :: Kernel.Prelude.UTCTime,
    actualDriverArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideDistanceEstimated :: Kernel.Prelude.Maybe Kernel.Types.Distance.HighPrecMeters,
    rideDistanceActual :: Kernel.Prelude.Maybe Kernel.Types.Distance.HighPrecMeters,
    chargeableDistance :: Kernel.Prelude.Maybe Kernel.Types.Distance.HighPrecMeters,
    rideDistanceEstimatedWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Distance.Distance,
    rideDistanceActualWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Distance.Distance,
    chargeableDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Distance.Distance,
    estimatedFare :: Kernel.Types.Price.Money,
    actualFare :: Kernel.Prelude.Maybe Kernel.Types.Price.Money,
    estimatedFareWithCurrency :: Kernel.Types.Price.PriceAPIEntity,
    actualFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Price.PriceAPIEntity,
    estimatedRideDuration :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    rideDuration :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    cancelledTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    cancelledBy :: Kernel.Prelude.Maybe API.Types.RiderPlatform.Management.Ride.CancellationSource,
    nextStopLocation :: Kernel.Prelude.Maybe Dashboard.RiderPlatform.Ride.Location,
    rideScheduledAt :: Kernel.Prelude.UTCTime,
    fareProductType :: Domain.Types.FareProductType,
    tripCategory :: Domain.Types.TripCategory,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimateFareBP :: Kernel.Prelude.Maybe [API.Types.RiderPlatform.Management.Ride.EstimateBreakup],
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Distance.HighPrecMeters,
    computedPrice :: Kernel.Prelude.Maybe Kernel.Types.Price.HighPrecMoney,
    fareBreakup :: [API.Types.RiderPlatform.Management.Ride.FareBreakup],
    rideCreatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListItem = RideListItem
  { rideShortId :: Kernel.Types.Id.ShortId Dashboard.Common.Ride,
    rideCreatedAt :: Kernel.Prelude.UTCTime,
    rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Text,
    bookingStatus :: Dashboard.RiderPlatform.Ride.BookingStatus,
    nextStopLocation :: Kernel.Prelude.Maybe Dashboard.RiderPlatform.Ride.Location,
    rideScheduledAt :: Kernel.Prelude.UTCTime,
    fareProductType :: Domain.Types.FareProductType,
    tripCategory :: Domain.Types.TripCategory,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListRes = RideListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, rides :: [API.Types.RiderPlatform.Management.Ride.RideListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideStatus
  = UPCOMING_RIDE
  | NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ShareRideInfoRes = ShareRideInfoRes
  { id :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    status :: API.Types.RiderPlatform.Management.Ride.RideStatus,
    driverName :: Kernel.Prelude.Text,
    driverNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Centesimal.Centesimal,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Text,
    vehicleColor :: Kernel.Prelude.Text,
    trackingUrl :: Kernel.Prelude.Maybe Kernel.Prelude.BaseUrl,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Distance.HighPrecMeters,
    estimatedDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Distance.Distance,
    rideStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    userFirstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    userLastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromLocation :: Dashboard.RiderPlatform.Ride.Location,
    toLocation :: Kernel.Prelude.Maybe Dashboard.RiderPlatform.Ride.Location,
    sosStatus :: Kernel.Prelude.Maybe API.Types.RiderPlatform.Management.Ride.SosStatus,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
    nextStopLocation :: Kernel.Prelude.Maybe Dashboard.RiderPlatform.Ride.Location,
    rideScheduledAt :: Kernel.Prelude.UTCTime,
    fareProductType :: Domain.Types.FareProductType,
    tripCategory :: Domain.Types.TripCategory,
    estimatedEndTimeRange :: Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime, Kernel.Prelude.UTCTime),
    destinationReachedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosStatus
  = NotResolved
  | Pending
  | Resolved
  | MockPending
  | MockResolved
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("ride" :> (GetRideList :<|> GetRideRideinfo :<|> GetRideInfo :<|> GetRideRideInfo :<|> GetRideTripRoute :<|> GetRidePickupRoute :<|> PostRideSync :<|> PostRideCancel))

type GetRideList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "bookingStatus"
           Dashboard.RiderPlatform.Ride.BookingStatus
      :> QueryParam "rideShortId" (Kernel.Types.Id.ShortId Dashboard.Common.Ride)
      :> QueryParam
           "customerPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam
           "driverPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           API.Types.RiderPlatform.Management.Ride.RideListRes
  )

type GetRideRideinfo = ("rideinfo" :> Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> Get '[JSON] API.Types.RiderPlatform.Management.Ride.RideInfoRes)

type GetRideInfo = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "info" :> Get '[JSON] API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)

type GetRideRideInfo = (Capture "rideShortId" (Kernel.Types.Id.ShortId Dashboard.Common.Ride) :> "rideInfo" :> Get '[JSON] API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)

type GetRideTripRoute =
  ( "trip" :> "route" :> Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> MandatoryQueryParam "lat" Kernel.Prelude.Double
      :> MandatoryQueryParam
           "lon"
           Kernel.Prelude.Double
      :> Get '[JSON] Kernel.External.Maps.GetRoutesResp
  )

type GetRidePickupRoute =
  ( "pickup" :> "route" :> Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> MandatoryQueryParam "lat" Kernel.Prelude.Double
      :> MandatoryQueryParam
           "lon"
           Kernel.Prelude.Double
      :> Get '[JSON] Kernel.External.Maps.GetRoutesResp
  )

type PostRideSync = ("sync" :> ReqBody '[JSON] API.Types.RiderPlatform.Management.Ride.MultipleRideSyncReq :> Post '[JSON] Dashboard.Common.Ride.MultipleRideSyncResp)

type PostRideCancel = ("cancel" :> ReqBody '[JSON] API.Types.RiderPlatform.Management.Ride.MultipleRideCancelReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data RideAPIs = RideAPIs
  { getRideList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Dashboard.RiderPlatform.Ride.BookingStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient API.Types.RiderPlatform.Management.Ride.RideListRes,
    getRideRideinfo :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient API.Types.RiderPlatform.Management.Ride.RideInfoRes,
    getRideInfo :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes,
    getRideRideInfo :: Kernel.Types.Id.ShortId Dashboard.Common.Ride -> EulerHS.Types.EulerClient API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes,
    getRideTripRoute :: Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> EulerHS.Types.EulerClient Kernel.External.Maps.GetRoutesResp,
    getRidePickupRoute :: Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> EulerHS.Types.EulerClient Kernel.External.Maps.GetRoutesResp,
    postRideSync :: API.Types.RiderPlatform.Management.Ride.MultipleRideSyncReq -> EulerHS.Types.EulerClient Dashboard.Common.Ride.MultipleRideSyncResp,
    postRideCancel :: API.Types.RiderPlatform.Management.Ride.MultipleRideCancelReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkRideAPIs :: (Client EulerHS.Types.EulerClient API -> RideAPIs)
mkRideAPIs rideClient = (RideAPIs {..})
  where
    getRideList :<|> getRideRideinfo :<|> getRideInfo :<|> getRideRideInfo :<|> getRideTripRoute :<|> getRidePickupRoute :<|> postRideSync :<|> postRideCancel = rideClient

data RideEndpointDSL
  = GetRideListEndpoint
  | GetRideRideinfoEndpoint
  | GetRideInfoEndpoint
  | GetRideRideInfoEndpoint
  | GetRideTripRouteEndpoint
  | GetRidePickupRouteEndpoint
  | PostRideSyncEndpoint
  | PostRideCancelEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
