{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Ride where

import qualified Dashboard.Common
import qualified Dashboard.Common.Booking
import qualified Dashboard.Common.Ride
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types
import qualified Domain.Types.VehicleVariant
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps
import qualified Kernel.External.Ticket.Interface.Types
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
  { bookingId :: Kernel.Types.Id.Id Dashboard.Common.Booking,
    cancellationReasonCode :: Dashboard.Common.Booking.CancellationReasonCode,
    cancellationStage :: CancellationStage,
    additionalInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingStatus
  = UPCOMING
  | UPCOMING_6HRS
  | ONGOING
  | ONGOING_6HRS
  | RCOMPLETED
  | RCANCELLED
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CancellationChargesWaiveOffRes = CancellationChargesWaiveOffRes
  { rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    waivedOffAmount :: Kernel.Prelude.Maybe Kernel.Types.Price.HighPrecMoney,
    waivedOffAmountWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Price.PriceAPIEntity,
    waivedOffSuccess :: Kernel.Prelude.Bool
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

data CancellationStage
  = OnSearch
  | OnInit
  | OnConfirm
  | OnAssign
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EstimateBreakup = EstimateBreakup {title :: Kernel.Prelude.Text, price :: EstimateBreakupPrice}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype EstimateBreakupPrice = EstimateBreakupPrice {value :: Kernel.Types.Price.PriceAPIEntity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FareBreakup = FareBreakup {amount :: Kernel.Types.Common.Price, description :: Kernel.Prelude.Text, entityId :: Kernel.Prelude.Text, entityType :: FareBreakupEntityType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FareBreakupEntityType
  = BOOKING_UPDATE_REQUEST
  | BOOKING
  | RIDE
  | INITIAL_BOOKING
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Location = Location
  { id :: Kernel.Types.Id.Id Location,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    address :: LocationAddress,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationAddress = LocationAddress
  { street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    area :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultipleRideCancelReq = MultipleRideCancelReq {multipleRideCancelInfo :: [BookingCancelledReq]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MultipleRideCancelReq where
  hideSecrets = Kernel.Prelude.identity

newtype MultipleRideItem = MultipleRideItem {rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultipleRideSyncReq = MultipleRideSyncReq {rides :: [MultipleRideItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MultipleRideSyncReq where
  hideSecrets = Kernel.Prelude.identity

data RideInfo = RideInfo
  { rideShortId :: Kernel.Types.Id.ShortId Dashboard.Common.Ride,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
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
    fare :: Kernel.Prelude.Maybe Kernel.Types.Price.Money,
    fareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Price.PriceAPIEntity,
    personId :: Kernel.Types.Id.Id Dashboard.Common.Customer,
    nextStopLocation :: Kernel.Prelude.Maybe Location,
    rideScheduledAt :: Kernel.Prelude.UTCTime,
    fareProductType :: Domain.Types.FareProductType,
    tripCategory :: Domain.Types.TripCategory,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    classification :: Kernel.External.Ticket.Interface.Types.Classification
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

data RideInfoRes = RideInfoRes
  { rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    bookingId :: Kernel.Types.Id.Id Dashboard.Common.Booking,
    rideStatus :: RideStatus,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideOtp :: Kernel.Prelude.Text,
    customerPickupLocation :: Location,
    customerDropLocation :: Kernel.Prelude.Maybe Location,
    driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverAlternatePhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
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
    cancelledBy :: Kernel.Prelude.Maybe CancellationSource,
    nextStopLocation :: Kernel.Prelude.Maybe Location,
    rideScheduledAt :: Kernel.Prelude.UTCTime,
    fareProductType :: Domain.Types.FareProductType,
    tripCategory :: Domain.Types.TripCategory,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimateFareBP :: Kernel.Prelude.Maybe [EstimateBreakup],
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Distance.HighPrecMeters,
    computedPrice :: Kernel.Prelude.Maybe Kernel.Types.Price.HighPrecMoney,
    fareBreakup :: [FareBreakup],
    rideCreatedAt :: Kernel.Prelude.UTCTime,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isSafetyPlus :: Kernel.Prelude.Bool,
    isAirConditioned :: Kernel.Prelude.Bool,
    rideSosId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Sos)
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
    bookingStatus :: BookingStatus,
    nextStopLocation :: Kernel.Prelude.Maybe Location,
    rideScheduledAt :: Kernel.Prelude.UTCTime,
    fareProductType :: Domain.Types.FareProductType,
    tripCategory :: Domain.Types.TripCategory,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isSafetyPlus :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListRes = RideListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, rides :: [RideListItem]}
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
    bookingId :: Kernel.Types.Id.Id Dashboard.Common.Booking,
    status :: RideStatus,
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
    fromLocation :: Location,
    toLocation :: Kernel.Prelude.Maybe Location,
    sosStatus :: Kernel.Prelude.Maybe SosStatus,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
    nextStopLocation :: Kernel.Prelude.Maybe Location,
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

newtype TicketRideListRes = TicketRideListRes {rides :: [RideInfo]}
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

type API = ("ride" :> (GetRideList :<|> GetRideInfo :<|> CancellationChargesWaiveOff :<|> GetShareRideInfo :<|> GetShareRideInfoByShortId :<|> GetRideTripRoute :<|> GetRidePickupRoute :<|> PostRideSyncMultiple :<|> PostRideCancelMultiple :<|> GetRideKaptureList))

type GetRideList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "bookingStatus" BookingStatus
      :> QueryParam
           "rideShortId"
           (Kernel.Types.Id.ShortId Dashboard.Common.Ride)
      :> QueryParam "customerPhoneNo" Kernel.Prelude.Text
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
           RideListRes
  )

type GetRideInfo = ("rideinfo" :> Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> Get '[JSON] RideInfoRes)

type CancellationChargesWaiveOff = ("cancellationChargesWaiveOff" :> Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> Get '[JSON] CancellationChargesWaiveOffRes)

type GetShareRideInfo = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "info" :> Get '[JSON] ShareRideInfoRes)

type GetShareRideInfoByShortId = (Capture "rideShortId" (Kernel.Types.Id.ShortId Dashboard.Common.Ride) :> "rideInfo" :> Get '[JSON] ShareRideInfoRes)

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

type PostRideSyncMultiple = ("sync" :> ReqBody '[JSON] MultipleRideSyncReq :> Post '[JSON] Dashboard.Common.Ride.MultipleRideSyncResp)

type PostRideCancelMultiple = ("cancel" :> ReqBody '[JSON] MultipleRideCancelReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetRideKaptureList =
  ( "kapture" :> "list" :> QueryParam "rideShortId" (Kernel.Types.Id.ShortId Dashboard.Common.Ride) :> QueryParam "countryCode" Kernel.Prelude.Text
      :> QueryParam
           "phoneNumber"
           Kernel.Prelude.Text
      :> QueryParam "supportPhoneNumber" Kernel.Prelude.Text
      :> Get '[JSON] TicketRideListRes
  )

data RideAPIs = RideAPIs
  { getRideList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe BookingStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient RideListRes,
    getRideInfo :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient RideInfoRes,
    cancellationChargesWaiveOff :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient CancellationChargesWaiveOffRes,
    getShareRideInfo :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient ShareRideInfoRes,
    getShareRideInfoByShortId :: Kernel.Types.Id.ShortId Dashboard.Common.Ride -> EulerHS.Types.EulerClient ShareRideInfoRes,
    getRideTripRoute :: Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> EulerHS.Types.EulerClient Kernel.External.Maps.GetRoutesResp,
    getRidePickupRoute :: Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> EulerHS.Types.EulerClient Kernel.External.Maps.GetRoutesResp,
    postRideSyncMultiple :: MultipleRideSyncReq -> EulerHS.Types.EulerClient Dashboard.Common.Ride.MultipleRideSyncResp,
    postRideCancelMultiple :: MultipleRideCancelReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getRideKaptureList :: Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient TicketRideListRes
  }

mkRideAPIs :: (Client EulerHS.Types.EulerClient API -> RideAPIs)
mkRideAPIs rideClient = (RideAPIs {..})
  where
    getRideList :<|> getRideInfo :<|> cancellationChargesWaiveOff :<|> getShareRideInfo :<|> getShareRideInfoByShortId :<|> getRideTripRoute :<|> getRidePickupRoute :<|> postRideSyncMultiple :<|> postRideCancelMultiple :<|> getRideKaptureList = rideClient

data RideUserActionType
  = GET_RIDE_LIST
  | GET_RIDE_INFO
  | CANCELLATION_CHARGES_WAIVE_OFF
  | GET_SHARE_RIDE_INFO
  | GET_SHARE_RIDE_INFO_BY_SHORT_ID
  | GET_RIDE_TRIP_ROUTE
  | GET_RIDE_PICKUP_ROUTE
  | POST_RIDE_SYNC_MULTIPLE
  | POST_RIDE_CANCEL_MULTIPLE
  | GET_RIDE_KAPTURE_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''RideUserActionType])
