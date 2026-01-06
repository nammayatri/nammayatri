{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.Driver where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified Domain.Types.Alert
import qualified Domain.Types.Alert.AlertRequestData
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.Alert.AlertRequestType
import qualified Domain.Types.FleetBadgeType
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Kernel.Utils.TH
import Servant
import Servant.Client

newtype APISuccessWithUnprocessedEntities = APISuccessWithUnprocessedEntities {unprocessedEntities :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ActualRoute = ActualRoute {lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double, timestamp :: Kernel.Prelude.UTCTime, accuracy :: Kernel.Prelude.Maybe Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AddVehicleReq = AddVehicleReq
  { registrationNo :: Kernel.Prelude.Text,
    vehicleClass :: Kernel.Prelude.Text,
    capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    colour :: Kernel.Prelude.Text,
    energyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    model :: Kernel.Prelude.Text,
    make :: Kernel.Prelude.Text,
    airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Image),
    vehicleCategory :: Kernel.Prelude.Maybe Dashboard.Common.VehicleCategory,
    oxygen :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    ventilator :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    dateOfRegistration :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    mYManufacturing :: Kernel.Prelude.Maybe Data.Time.Day,
    vehicleModelYear :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleTags :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    fuelType :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AddVehicleReq where
  hideSecrets = Kernel.Prelude.identity

data Address = Address {point :: Kernel.External.Maps.Types.LatLong, address :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AllTimeFleetAnalyticsRes = AllTimeFleetAnalyticsRes
  { activeVehicle :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    completedRides :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    totalActiveDrivers :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    currentOnlineDrivers :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    averageDriverRatings :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ApproveDriverReq = ApproveDriverReq {driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver, approve :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ApproveDriverReq where
  hideSecrets = Kernel.Prelude.identity

data AssignScheduledBookingReq = AssignScheduledBookingReq {bookingId :: Kernel.Prelude.Text, driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver, clientId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AssignScheduledBookingReq where
  hideSecrets = Kernel.Prelude.identity

data BusRideInfo = BusRideInfo
  { busNumber :: Kernel.Prelude.Text,
    destination :: Kernel.External.Maps.Types.LatLong,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    groupId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    routeLongName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    source :: Kernel.External.Maps.Types.LatLong
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets BusRideInfo where
  hideSecrets = Kernel.Prelude.identity

data CarRideInfo = CarRideInfo
  { minDistanceBetweenTwoPoints :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    pickupLocation :: Kernel.External.Maps.Types.LatLong,
    rideStops :: Kernel.Prelude.Maybe [Kernel.External.Maps.Types.LatLong]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CarRideInfo where
  hideSecrets = Kernel.Prelude.identity

data CreateDriverBusRouteMappingReq = CreateDriverBusRouteMappingReq {file :: Kernel.Prelude.FilePath, fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateDriverBusRouteMappingReq where
  hideSecrets = Kernel.Prelude.identity

data CreateDriversReq = CreateDriversReq {file :: Kernel.Prelude.FilePath, fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateDriversReq where
  hideSecrets = Kernel.Prelude.identity

data CreateVehiclesReq = CreateVehiclesReq {file :: Kernel.Prelude.FilePath, fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text, requestorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateVehiclesReq where
  hideSecrets = Kernel.Prelude.identity

data DriveVehicleAssociationListItem = DriveVehicleAssociationListItem
  { driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    conductorName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe DriverMode,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    completedRides :: Kernel.Prelude.Int,
    vehicleType :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    earning :: Kernel.Prelude.Int,
    isDriverOnRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDriverOnPickup :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDriverActive :: Kernel.Prelude.Bool,
    isRcAssociated :: Kernel.Prelude.Bool,
    verificationDocsStatus :: Kernel.Prelude.Maybe VerificationDocsStatus,
    upcomingRouteCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriveVehicleAssociationListItemT = DriveVehicleAssociationListItemT
  { driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    conductorName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe DriverMode,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    completedRides :: Kernel.Prelude.Int,
    vehicleType :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    earning :: Kernel.Prelude.Int,
    isDriverOnRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDriverOnPickup :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDriverActive :: Kernel.Prelude.Bool,
    isRcAssociated :: Kernel.Prelude.Bool,
    verificationDocsStatus :: Kernel.Prelude.Maybe VerificationDocsStatus,
    upcomingRouteCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOwnerId :: Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Text,
    requestReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    associatedOn :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverDetails = DriverDetails
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverDetails where
  hideSecrets = Kernel.Prelude.identity

data DriverDetailsReq = DriverDetailsReq {driverIds :: [Kernel.Types.Id.Id Dashboard.Common.Driver]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverDetailsRes = DriverDetailsRes
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dob :: Kernel.Prelude.Maybe Data.Time.Day,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverDetailsRes where
  hideSecrets = Kernel.Prelude.identity

data DriverDetailsResp = DriverDetailsResp {drivers :: [DriverDetails]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverInfo = DriverInfo
  { routeCode :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    rideStatus :: RideStatus,
    routeLongName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    point :: Kernel.External.Maps.Types.LatLong,
    driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverInfoT = DriverInfoT
  { routeCode :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    rideStatus :: RideStatus,
    routeLongName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    point :: Kernel.External.Maps.Types.LatLong,
    fleetOwnerId :: Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLocation = DriverLocation
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    timeStamp :: Kernel.Prelude.UTCTime,
    accuracy :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLocationListReq = DriverLocationListReq
  { driverIds :: [Kernel.Types.Id.Id Dashboard.Common.Driver],
    firstDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    limit :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLocationListResp = DriverLocationListResp {driverLocations :: [DriverLocation]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverMode
  = ONLINE
  | OFFLINE
  | SILENT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data DriverRequestDetails = DriverRequestDetails
  { requestData :: Domain.Types.Alert.AlertRequestData.AlertRequestData,
    title :: Kernel.Prelude.Text,
    body :: Kernel.Prelude.Text,
    raisedAt :: Kernel.Prelude.UTCTime,
    status :: Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestStatus,
    approvalRequestId :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Text,
    tripTransactionId :: Kernel.Prelude.Text,
    isViolated :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverRequestDetailsT = DriverRequestDetailsT
  { requestData :: Domain.Types.Alert.AlertRequestData.AlertRequestData,
    title :: Kernel.Prelude.Text,
    body :: Kernel.Prelude.Text,
    raisedAt :: Kernel.Prelude.UTCTime,
    status :: Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestStatus,
    approvalRequestId :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Text,
    tripTransactionId :: Kernel.Prelude.Text,
    isViolated :: Kernel.Prelude.Bool,
    fleetOwnerId :: Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype DriverRequestResp = DriverRequestResp {requests :: [DriverRequestDetails]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype DriverRequestRespT = DriverRequestRespT {requests :: [DriverRequestDetailsT]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverStatusRes = DriverStatusRes
  { online :: Kernel.Prelude.Int,
    offline :: Kernel.Prelude.Int,
    silent :: Kernel.Prelude.Int,
    toPickup :: Kernel.Prelude.Int,
    onRide :: Kernel.Prelude.Int,
    active :: Kernel.Prelude.Int,
    inactive :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DrivertoVehicleAssociationRes = DrivertoVehicleAssociationRes {fleetOwnerId :: Kernel.Prelude.Text, listItem :: [DriveVehicleAssociationListItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DrivertoVehicleAssociationResT = DrivertoVehicleAssociationResT {fleetOwnerId :: Kernel.Prelude.Text, listItem :: [DriveVehicleAssociationListItemT], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EarningFleetAnalyticsRes = EarningFleetAnalyticsRes
  { grossEarnings :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    platformFees :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    netEarnings :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    grossEarningsPerHour :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    netEarningsPerHour :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EstimateRouteReq = EstimateRouteReq {start :: Kernel.External.Maps.Types.LatLong, end :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EstimatedRouteDetails = EstimatedRouteDetails
  { polyline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    duration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FilteredFleetAnalyticsRes = FilteredFleetAnalyticsRes
  { totalEarnings :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    completedRides :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    totalDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    totalRideRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    acceptedRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    pulledRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rejectedRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    driverCancelled :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    customerCancelled :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetAnalyticsRes
  = Filtered FilteredFleetAnalyticsRes
  | Earnings EarningFleetAnalyticsRes
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetAnalyticsResponseType
  = FILTERED
  | EARNINGS
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

newtype FleetBadgeRes = FleetBadgeRes {fleetBadgeInfos :: [FleetBadgesAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetBadgeResT = FleetBadgeResT {fleetBadgeInfos :: [FleetBadgesAPIEntityT]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBadgesAPIEntity = FleetBadgesAPIEntity {badgeName :: Kernel.Prelude.Text, isActive :: Kernel.Prelude.Bool, badgeType :: Domain.Types.FleetBadgeType.FleetBadgeType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBadgesAPIEntityT = FleetBadgesAPIEntityT
  { badgeName :: Kernel.Prelude.Text,
    isActive :: Kernel.Prelude.Bool,
    badgeType :: Domain.Types.FleetBadgeType.FleetBadgeType,
    fleetOwnerId :: Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBookingAPIEntity = FleetBookingAPIEntity
  { id :: Kernel.Prelude.Text,
    fromLocation :: LocationAPIEntity,
    toLocation :: Kernel.Prelude.Maybe LocationAPIEntity,
    estimatedFare :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    startTime :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Dashboard.Common.ServiceTierType,
    vehicleServiceTierName :: Kernel.Prelude.Text,
    tripCategory :: Dashboard.Common.TripCategory,
    distanceToPickup :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    isScheduled :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBookingAssignmentItem = FleetBookingAssignmentItem
  { bookingId :: Kernel.Prelude.Text,
    serviceId :: Kernel.Prelude.Text,
    serviceName :: Kernel.Prelude.Text,
    placeName :: Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Text,
    amount :: Kernel.Prelude.Double,
    visitDate :: Kernel.Prelude.Maybe Data.Time.Day,
    createdAt :: Kernel.Prelude.UTCTime,
    fleetOwnerId :: Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Text,
    paymentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBookingAssignmentsResponse = FleetBookingAssignmentsResponse {bookings :: [FleetBookingAssignmentItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBookingItem = FleetBookingItem
  { bookingId :: Kernel.Prelude.Text,
    serviceId :: Kernel.Prelude.Text,
    ticketBookingShortId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketBookingServiceShortId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceName :: Kernel.Prelude.Text,
    placeName :: Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Text,
    amount :: Kernel.Prelude.Double,
    bookedSeats :: Kernel.Prelude.Int,
    status :: Kernel.Prelude.Text,
    visitDate :: Kernel.Prelude.Maybe Data.Time.Day,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    fleetOwnerId :: Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Text,
    paymentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerMobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBookingsInformationResponse = FleetBookingsInformationResponse {bookings :: [FleetBookingItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetConfig = FleetConfig
  { allowAutomaticRoundTripAssignment :: Kernel.Prelude.Bool,
    allowEndingMidRoute :: Kernel.Prelude.Bool,
    allowStartRideFromQR :: Kernel.Prelude.Bool,
    endRideDistanceThreshold :: Kernel.Types.Common.HighPrecMeters,
    rideEndApproval :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetConfig where
  hideSecrets = Kernel.Prelude.identity

data FleetDashboardAnalyticsCacheReq = FleetDashboardAnalyticsCacheReq
  { fleetOwnerId :: Kernel.Prelude.Text,
    activeDriverCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    activeVehicleCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    currentOnlineDriver :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetDriverEarningsStatsRes = FleetDriverEarningsStatsRes
  { driverName :: Kernel.Prelude.Text,
    totalEarningGross :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    inAppEarningGross :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cashEarningGross :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformFeeTotal :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalEarningNet :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    inAppEarningNet :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cashEarningNet :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetDriverListStatsSortOn
  = TOTAL_RATING_COUNT
  | TOTAL_RATING_SCORE
  | DRIVER_FIRST_SUBSCRIPTION
  | INSPECTION_COMPLETED
  | REJECTED_REQUEST_COUNT
  | PULLED_REQUEST_COUNT
  | ACCEPTATION_REQUEST_COUNT
  | TOTAL_REQUEST_COUNT
  | CUSTOMER_CANCELLATION_COUNT
  | DRIVER_CANCELLATION_COUNT
  | TOTAL_DISTANCE
  | TOTAL_COMPLETED_RIDES
  | ONLINE_TOTAL_EARNING
  | CASH_TOTAL_EARNING
  | CASH_PLATFORM_FEES
  | ONLINE_PLATFORM_FEES
  | ONLINE_DURATION
  | RIDE_DURATION
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data FleetDriverMetricsStatsRes = FleetDriverMetricsStatsRes
  { driverName :: Kernel.Prelude.Text,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    acceptedRideRequests :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rejectedRideRequests :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passedRideRequests :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    acceptanceRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    completedRides :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    driverCanceledRides :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    customerCanceledRides :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    completionRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    onlineDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    rideDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    utilization :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    earnings :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    earningPerKm :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetDriverStatsListRes = FleetDriverStatsListRes {driverStats :: [FleetDriverStatsRes], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetDriverStatsRes
  = EarningsList FleetDriverEarningsStatsRes
  | MetricsList FleetDriverMetricsStatsRes
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetDriverStatsResponseType
  = EARNINGS_LIST
  | METRICS_LIST
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data FleetDriversAPIEntity = FleetDriversAPIEntity
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetDriversAPIEntityT = FleetDriversAPIEntityT
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    fleetOwnerId :: Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetEarningListRes = FleetEarningListRes {fleetEarningRes :: [FleetEarningRes], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetEarningRes = FleetEarningRes
  { totalRides :: Kernel.Prelude.Int,
    totalEarning :: Kernel.Prelude.Int,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver),
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe DriverMode,
    vehicleType :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    totalDuration :: TotalDuration,
    distanceTravelled :: Kernel.Prelude.Double,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cancelledRides :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetGroup = FleetGroup {level :: Kernel.Prelude.Int, parentGroupCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, groupCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetListDriverRes = FleetListDriverRes {fleetDriversInfos :: [FleetDriversAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetListDriverResT = FleetListDriverResT {fleetDriversInfos :: [FleetDriversAPIEntityT]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerInfoRes = FleetOwnerInfoRes
  { id :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blocked :: Kernel.Prelude.Bool,
    enabled :: Kernel.Prelude.Bool,
    fleetType :: Kernel.Prelude.Text,
    verified :: Kernel.Prelude.Bool,
    approvedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    roleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referredByOperatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isEligibleForSubscription :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    gstNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    aadhaarNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetConfig :: Kernel.Prelude.Maybe FleetConfig,
    operatorName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    operatorContact :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referralCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registeredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    businessLicenseNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetDob :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    stripeAddress :: Kernel.Prelude.Maybe Dashboard.Common.Driver.StripeAddress,
    stripeIdNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetOwnerInfoRes where
  hideSecrets = Kernel.Prelude.identity

data FleetOwnerListAPIEntity = FleetOwnerListAPIEntity
  { fleetOwnerId :: Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Text,
    fleetGroup :: Kernel.Prelude.Maybe FleetGroup,
    groups :: [FleetGroup],
    order :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    enabled :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetOwnerListRes = FleetOwnerListRes {ownersList :: [FleetOwnerListAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetScheduledBooking = FleetScheduledBooking {bookingDetails :: FleetBookingAPIEntity, fareDetails :: [RateCardItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetScheduledBookingListRes = FleetScheduledBookingListRes {bookings :: [FleetScheduledBooking]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetTotalEarningResponse = FleetTotalEarningResponse
  { totalRides :: Kernel.Prelude.Int,
    totalEarning :: Kernel.Prelude.Int,
    totalVehicle :: Kernel.Prelude.Int,
    conversionRate :: Kernel.Prelude.Double,
    cancellationRate :: Kernel.Prelude.Double,
    cancelledRides :: Kernel.Prelude.Int,
    totalDistanceTravelled :: Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetVehicleStatsItem = FleetVehicleStatsItem
  { vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleManufacturer :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rcId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    totalEarnings :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    completedRides :: Kernel.Prelude.Int,
    rideDistance :: Kernel.Types.Common.Meters,
    rideDuration :: Kernel.Types.Common.Seconds,
    earningPerKm :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetVehicleStatsRes = FleetVehicleStatsRes {fleetOwnerId :: Kernel.Prelude.Text, listItem :: [FleetVehicleStatsItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetVehicleStatus
  = Active
  | InActive
  | Valid
  | Invalid
  | Pending
  | OnRide
  | TripAssigned
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data LinkRCWithDriverForFleetReq = LinkRCWithDriverForFleetReq {driverMobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, driverMobileNumber :: Kernel.Prelude.Text, vehicleRegistrationNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets LinkRCWithDriverForFleetReq where
  hideSecrets = Kernel.Prelude.identity

newtype ListVehicleRes = ListVehicleRes {vehicles :: [VehicleAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype ListVehicleResT = ListVehicleResT {vehicles :: [VehicleAPIEntityT]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationAPIEntity = LocationAPIEntity
  { id :: Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    instructions :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    extras :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultiOwnerSelect = MultiOwnerSelect {fleetOwnerIds :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriverDetails = NearbyDriverDetails
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    point :: Kernel.External.Maps.Types.LatLong,
    rideStatus :: Kernel.Prelude.Maybe RideStatus,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideInfo :: Kernel.Prelude.Maybe RideInfo
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriverDetailsT = NearbyDriverDetailsT
  { fleetOwnerId :: Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    point :: Kernel.External.Maps.Types.LatLong,
    fleetOwnerName :: Kernel.Prelude.Text,
    rideStatus :: Kernel.Prelude.Maybe RideStatus,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideInfo :: Kernel.Prelude.Maybe RideInfo
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriverReq = NearbyDriverReq {point :: Kernel.External.Maps.Types.LatLong, radius :: Kernel.Prelude.Int, vehicleVariantList :: Kernel.Prelude.Maybe [Dashboard.Common.VehicleVariant]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets NearbyDriverReq where
  hideSecrets = Kernel.Prelude.identity

data NearbyDriverResp = NearbyDriverResp {drivers :: [DriverInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriverRespT = NearbyDriverRespT {drivers :: [DriverInfoT]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriversReqV2 = NearbyDriversReqV2 {point :: Kernel.External.Maps.Types.LatLong, radius :: Kernel.Prelude.Int, vehicleVariantList :: [Dashboard.Common.VehicleVariant]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets NearbyDriversReqV2 where
  hideSecrets = Kernel.Prelude.identity

data NearbyDriversRespTV2 = NearbyDriversRespTV2 {drivers :: [NearbyDriverDetailsT]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriversRespV2 = NearbyDriversRespV2 {drivers :: [NearbyDriverDetails]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OnboardedDriver = OnboardedDriver {driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver, mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OnboardedDriversAndUnlinkedVehiclesRes = OnboardedDriversAndUnlinkedVehiclesRes {onboardedDrivers :: [OnboardedDriver], unlinkedVehicles :: [UnlinkedVehicle]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PilotRideInfo = PilotRideInfo
  { destination :: Kernel.External.Maps.Types.LatLong,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dutyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    groupId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pilotNumber :: Kernel.Prelude.Text,
    scheduledTripTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    source :: Kernel.External.Maps.Types.LatLong,
    startAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vipName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RCStatusReq = RCStatusReq
  { rcNo :: Kernel.Prelude.Text,
    isActivate :: Kernel.Prelude.Bool,
    serviceName :: Kernel.Prelude.Maybe Dashboard.Common.Driver.ServiceNames,
    planToAssociate :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RCStatusReq where
  hideSecrets = Kernel.Prelude.identity

data RateCardItem = RateCardItem {title :: Kernel.Prelude.Text, price :: Kernel.Types.Common.Money, priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RequestRespondReq = RequestRespondReq {status :: Domain.Types.Alert.AlertRequestStatus, approvalRequestId :: Kernel.Prelude.Text, reason :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RequestRespondReq where
  hideSecrets = Kernel.Prelude.identity

data RideInfo
  = Bus BusRideInfo
  | Car CarRideInfo
  | Pilot PilotRideInfo
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideStatus
  = ON_PICKUP
  | ON_RIDE
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RoundTripDetail = RoundTripDetail {frequency :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteAPIResp = RouteAPIResp {listItem :: [RouteRespItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RouteAPIResp where
  hideSecrets = Kernel.Prelude.identity

data RouteDetails = RouteDetails
  { code :: Kernel.Prelude.Text,
    shortName :: Kernel.Prelude.Text,
    longName :: Kernel.Prelude.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    stops :: [StopInfo],
    waypoints :: Kernel.Prelude.Maybe [Kernel.External.Maps.Types.LatLong],
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RouteDetails where
  hideSecrets = Kernel.Prelude.identity

data RouteRespItem = RouteRespItem
  { code :: Kernel.Prelude.Text,
    shortName :: Kernel.Prelude.Text,
    longName :: Kernel.Prelude.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    totalStops :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    waypoints :: Kernel.Prelude.Maybe [Kernel.External.Maps.Types.LatLong],
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RouteRespItem where
  hideSecrets = Kernel.Prelude.identity

data SortOn
  = COMPLETED_RIDES
  | CANCELLED_RIDES
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data StopInfo = StopInfo {code :: Kernel.Prelude.Text, name :: Kernel.Prelude.Text, point :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets StopInfo where
  hideSecrets = Kernel.Prelude.identity

data TotalDuration = TotalDuration {hours :: Kernel.Prelude.Int, minutes :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TrackDriverLocation = TrackDriverLocation {driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver, point :: Kernel.External.Maps.Types.LatLong, lastUpdatedAt :: Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype TrackDriverLocationsReq = TrackDriverLocationsReq {driverIds :: [Kernel.Types.Id.Id Dashboard.Common.Driver]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype TrackDriverLocationsRes = TrackDriverLocationsRes {driverLocations :: [TrackDriverLocation]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripDetails = TripDetails
  { routeCode :: Kernel.Prelude.Text,
    roundTrip :: Kernel.Prelude.Maybe RoundTripDetail,
    scheduledTripTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vipName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dutyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startAddress :: Kernel.Prelude.Maybe Address,
    endAddress :: Kernel.Prelude.Maybe Address,
    tripType :: Kernel.Prelude.Maybe TripType,
    estimatedRouteDetails :: Kernel.Prelude.Maybe EstimatedRouteDetails
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripPlannerReq = TripPlannerReq
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    badgeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverBadgeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    conductorBadgeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pilotBadgeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    trips :: [TripDetails]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TripPlannerReq where
  hideSecrets = Kernel.Prelude.identity

data TripStatus
  = TRIP_ASSIGNED
  | CANCELLED
  | IN_PROGRESS
  | PAUSED
  | COMPLETED
  | UPCOMING
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data TripTransactionDetail = TripTransactionDetail
  { tripTransactionId :: Kernel.Types.Id.Id Dashboard.Common.TripTransaction,
    routeCode :: Kernel.Prelude.Text,
    tripStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripStatus :: TripStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripTransactionDetailT = TripTransactionDetailT
  { tripTransactionId :: Kernel.Types.Id.Id Dashboard.Common.TripTransaction,
    routeCode :: Kernel.Prelude.Text,
    tripStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripStatus :: TripStatus,
    fleetOwnerId :: Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Text,
    tripType :: Kernel.Prelude.Maybe TripType,
    scheduledTripTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    dutyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vipName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startAddress :: Kernel.Prelude.Maybe Address,
    endAddress :: Kernel.Prelude.Maybe Address,
    estimatedRouteDetails :: Kernel.Prelude.Maybe EstimatedRouteDetails
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripTransactionResp = TripTransactionResp {trips :: [TripTransactionDetail], totalTrips :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripTransactionRespT = TripTransactionRespT {trips :: [TripTransactionDetailT], totalTrips :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripTransactionWaypointsRes = TripTransactionWaypointsRes {waypoints :: [ActualRoute], estimatedRouteDetails :: Kernel.Prelude.Maybe EstimatedRouteDetails}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TripTransactionWaypointsRes where
  hideSecrets = Kernel.Prelude.identity

data TripType
  = PILOT
  | WIMB
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data UnlinkedVehicle = UnlinkedVehicle {vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text, rcId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateDriverReq = UpdateDriverReq
  { firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dob :: Kernel.Prelude.Maybe Data.Time.Day,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateDriverReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateFleetOwnerInfoReq = UpdateFleetOwnerInfoReq
  { firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetDob :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    stripeAddress :: Kernel.Prelude.Maybe Dashboard.Common.Driver.StripeAddress,
    stripeIdNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetType :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateFleetOwnerInfoReq where
  hideSecrets = Kernel.Prelude.identity

data VehicleAPIEntity = VehicleAPIEntity
  { variant :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    model :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    color :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registrationNo :: Kernel.Prelude.Text,
    isActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleAPIEntityT = VehicleAPIEntityT
  { variant :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    model :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    color :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registrationNo :: Kernel.Prelude.Text,
    isActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    fleetOwnerId :: Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerificationDocsStatus = VerificationDocsStatus
  { vehicleRegistrationCertificate :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehiclePermit :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehicleFitness :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehicleInsurance :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehiclePUC :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehicleLeft :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehicleRight :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehicleFront :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehicleFrontInterior :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehicleBackInterior :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehicleBack :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    odometer :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    driverLicense :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    panCard :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    aadhaarCard :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyFleetJoiningOtpReq = VerifyFleetJoiningOtpReq
  { mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    otp :: Kernel.Prelude.Text,
    deviceToken :: Kernel.Prelude.Maybe Kernel.External.Notification.FCM.Types.FCMRecipientToken
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VerifyFleetJoiningOtpReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("driver" :> (GetDriverFleetAccessList :<|> PostDriverFleetAccessSelect :<|> PostDriverFleetV2AccessSelect :<|> PostDriverFleetV2AccessMultiOwnerIdSelect :<|> PostDriverFleetAddVehiclesHelper :<|> PostDriverFleetAddVehicleHelper :<|> GetDriverFleetGetDriverRequests :<|> PostDriverFleetDriverRequestRespondHelper :<|> PostDriverFleetAddRCWithoutDriverHelper :<|> GetDriverFleetGetAllVehicle :<|> GetDriverFleetGetAllDriver :<|> GetDriverFleetGetAllBadge :<|> PostDriverFleetUnlinkHelper :<|> PostDriverFleetRemoveVehicleHelper :<|> PostDriverFleetRemoveDriverHelper :<|> GetDriverFleetTotalEarningHelper :<|> GetDriverFleetVehicleEarningHelper :<|> GetDriverFleetDriverEarningHelper :<|> GetDriverFleetBookingsHelper :<|> GetDriverFleetAssignmentsHelper :<|> GetDriverFleetGetFleetDriverVehicleAssociationHelper :<|> GetDriverFleetDriverListStatsHelper :<|> GetDriverFleetDriverAssociation :<|> GetDriverFleetVehicleAssociation :<|> PostDriverFleetVehicleDriverRCstatusHelper :<|> PostDriverUpdateFleetOwnerInfoHelper :<|> GetDriverFleetOwnerInfo :<|> GetDriverFleetOperatorInfoHelper :<|> PostDriverFleetDriverSendJoiningOtpHelper :<|> PostDriverFleetDriverVerifyJoiningOtpHelper :<|> GetDriverFleetRoutesHelper :<|> GetDriverFleetPossibleRoutesHelper :<|> PostDriverFleetTripPlannerHelper :<|> GetDriverFleetTripTransactions :<|> PostDriverFleetAddDriversHelper :<|> PostDriverFleetAddDriverBusRouteMappingHelper :<|> PostDriverFleetLinkRCWithDriverHelper :<|> PostDriverDashboardFleetWmbTripEndHelper :<|> GetDriverDashboardFleetTripWaypointsHelper :<|> GetDriverFleetWmbRouteDetailsHelper :<|> PostDriverFleetGetNearbyDriversHelper :<|> PostDriverDashboardFleetTrackDriverHelper :<|> GetDriverDashboardInternalHelperGetFleetOwnerId :<|> GetDriverDashboardInternalHelperGetFleetOwnerIds :<|> GetDriverFleetStatusHelper :<|> PostDriverFleetLocationListHelper :<|> PostDriverFleetGetDriverDetailsHelper :<|> PostDriverFleetGetNearbyDriversV2Helper :<|> GetDriverFleetDashboardAnalyticsAllTimeHelper :<|> GetDriverFleetDashboardAnalyticsHelper :<|> PostDriverFleetDashboardAnalyticsCache :<|> PostDriverDashboardFleetEstimateRouteHelper :<|> PostDriverFleetApproveDriverHelper :<|> PostDriverFleetDriverUpdateHelper :<|> GetDriverFleetDriverDetailsHelper :<|> PostDriverFleetTripTransactionsV2 :<|> GetDriverFleetVehicleListStatsHelper :<|> GetDriverFleetDriverOnboardedDriversAndUnlinkedVehiclesHelper :<|> GetDriverFleetScheduledBookingListHelper :<|> PostDriverFleetScheduledBookingAssignHelper))

type GetDriverFleetAccessList = ("fleet" :> "access" :> "list" :> QueryParam "fleetMemberId" Kernel.Prelude.Text :> Get '[JSON] FleetOwnerListRes)

type PostDriverFleetAccessSelect =
  ( "fleet" :> "access" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "select" :> QueryParam "fleetMemberId" Kernel.Prelude.Text
      :> QueryParam
           "onlySingle"
           Kernel.Prelude.Bool
      :> MandatoryQueryParam "enable" Kernel.Prelude.Bool
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetV2AccessSelect =
  ( "fleet" :> "v2" :> "access" :> "select" :> QueryParam "fleetMemberId" Kernel.Prelude.Text
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam "groupCode" Kernel.Prelude.Text
      :> QueryParam "onlyCurrent" Kernel.Prelude.Bool
      :> MandatoryQueryParam
           "enable"
           Kernel.Prelude.Bool
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetV2AccessMultiOwnerIdSelect =
  ( "fleet" :> "v2" :> "access" :> "multiOwnerId" :> "select" :> QueryParam "fleetMemberId" Kernel.Prelude.Text
      :> QueryParam
           "onlyCurrent"
           Kernel.Prelude.Bool
      :> MandatoryQueryParam "enable" Kernel.Prelude.Bool
      :> ReqBody '[JSON] MultiOwnerSelect
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddVehicles =
  ( "fleet" :> "addVehicles" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           CreateVehiclesReq
      :> Post '[JSON] APISuccessWithUnprocessedEntities
  )

type PostDriverFleetAddVehiclesHelper =
  ( "fleet" :> "addVehicles" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp CreateVehiclesReq
      :> Post
           '[JSON]
           APISuccessWithUnprocessedEntities
  )

type PostDriverFleetAddVehicle =
  ( Capture "mobileNo" Kernel.Prelude.Text :> "fleet" :> "addVehicle" :> QueryParam "countryCode" Kernel.Prelude.Text
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam "mbRole" Dashboard.Common.Role
      :> ReqBody '[JSON] AddVehicleReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddVehicleHelper =
  ( Capture "mobileNo" Kernel.Prelude.Text :> Capture "requestorId" Kernel.Prelude.Text :> "fleet" :> "addVehicle"
      :> QueryParam
           "mobileCountryCode"
           Kernel.Prelude.Text
      :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam "mbRole" Dashboard.Common.Role
      :> ReqBody
           '[JSON]
           AddVehicleReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetGetDriverRequests =
  ( "fleet" :> "getDriverRequests" :> QueryParam "mbFrom" Kernel.Prelude.UTCTime :> QueryParam "mbTo" Kernel.Prelude.UTCTime
      :> QueryParam
           "mbRequestType"
           Domain.Types.Alert.AlertRequestType.AlertRequestType
      :> QueryParam
           "mbRouteCode"
           Kernel.Prelude.Text
      :> QueryParam
           "mbDriverId"
           Kernel.Prelude.Text
      :> QueryParam
           "mbBadgeName"
           Kernel.Prelude.Text
      :> QueryParam
           "mbFleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam
           "mbalertStatus"
           Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus
      :> QueryParam
           "mbLimit"
           Kernel.Prelude.Int
      :> QueryParam
           "mbOffset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           DriverRequestRespT
  )

type PostDriverFleetRespondDriverRequest =
  ( "fleet" :> "respond" :> "driverRequest" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] RequestRespondReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetDriverRequestRespondHelper =
  ( "fleet" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "driverRequest" :> "respond" :> ReqBody '[JSON] RequestRespondReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddRCWithoutDriver =
  ( "fleet" :> "addRC" :> "withoutDriver" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddRCWithoutDriverHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "addRC" :> "withoutDriver"
      :> ReqBody
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetGetAllVehicle =
  ( "fleet" :> "getAllVehicle" :> QueryParam "mblimit" Kernel.Prelude.Int :> QueryParam "mboffset" Kernel.Prelude.Int
      :> QueryParam
           "mbRegNumberString"
           Kernel.Prelude.Text
      :> QueryParam "mbFleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "mbIsActive"
           Kernel.Prelude.Bool
      :> QueryParam
           "mbMemberPersonId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           ListVehicleResT
  )

type GetDriverFleetGetAllDriver =
  ( "fleet" :> "getAllDriver" :> QueryParam "mblimit" Kernel.Prelude.Int :> QueryParam "mboffset" Kernel.Prelude.Int
      :> QueryParam
           "mbMobileNumberString"
           Kernel.Prelude.Text
      :> QueryParam "mbNameString" Kernel.Prelude.Text
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> QueryParam
           "mbFleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam
           "mbIsActive"
           Kernel.Prelude.Bool
      :> QueryParam
           "mbMemberPersonId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           FleetListDriverResT
  )

type GetDriverFleetGetAllBadge =
  ( "fleet" :> "getAllBadge" :> QueryParam "mblimit" Kernel.Prelude.Int :> QueryParam "mboffset" Kernel.Prelude.Int
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> QueryParam "mbFleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "mbBadgeType"
           Domain.Types.FleetBadgeType.FleetBadgeType
      :> QueryParam
           "mbIsActive"
           Kernel.Prelude.Bool
      :> QueryParam
           "mbMemberPersonId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           FleetBadgeResT
  )

type PostDriverFleetUnlink =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "unlink"
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetUnlinkHelper =
  ( Capture "requestorId" Kernel.Prelude.Text :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> Capture
           "vehicleNo"
           Kernel.Prelude.Text
      :> "fleet"
      :> "unlink"
      :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveVehicle =
  ( Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "remove" :> "vehicle" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveVehicleHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "remove" :> "vehicle"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveDriver =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "fleet" :> "remove" :> "driver"
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveDriverHelper =
  ( Capture "requestorId" Kernel.Prelude.Text
      :> Capture
           "driverId"
           (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> "fleet"
      :> "remove"
      :> "driver"
      :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetTotalEarning = ("fleet" :> "totalEarning" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "to" Kernel.Prelude.UTCTime :> Get '[JSON] FleetTotalEarningResponse)

type GetDriverFleetTotalEarningHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "totalEarning" :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get '[JSON] FleetTotalEarningResponse
  )

type GetDriverFleetVehicleEarning =
  ( "fleet" :> "vehicleEarning" :> QueryParam "vehicleNo" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           FleetEarningListRes
  )

type GetDriverFleetVehicleEarningHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "vehicleEarning" :> QueryParam "vehicleNo" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           FleetEarningListRes
  )

type GetDriverFleetDriverEarning =
  ( "fleet" :> "driverEarning" :> QueryParam "mobileCountryCode" Kernel.Prelude.Text :> QueryParam "mobileNo" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "sortDesc"
           Kernel.Prelude.Bool
      :> QueryParam
           "sortOn"
           SortOn
      :> Get
           '[JSON]
           FleetEarningListRes
  )

type GetDriverFleetDriverEarningHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "driverEarning" :> QueryParam "mobileCountryCode" Kernel.Prelude.Text
      :> QueryParam
           "mobileNo"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "sortDesc"
           Kernel.Prelude.Bool
      :> QueryParam
           "SortOn"
           SortOn
      :> Get
           '[JSON]
           FleetEarningListRes
  )

type GetDriverFleetBookings =
  ( "fleet" :> "bookings" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam "status" Kernel.Prelude.Text
      :> QueryParam
           "vehicleNo"
           Kernel.Prelude.Text
      :> QueryParam
           "searchByFleetOwnerId"
           Kernel.Prelude.Bool
      :> QueryParam
           "searchByTicketPlaceId"
           Kernel.Prelude.Bool
      :> Get
           '[JSON]
           FleetBookingsInformationResponse
  )

type GetDriverFleetBookingsHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "bookings" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> QueryParam
           "status"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNo"
           Kernel.Prelude.Text
      :> QueryParam
           "searchByFleetOwnerId"
           Kernel.Prelude.Bool
      :> QueryParam
           "searchByTicketPlaceId"
           Kernel.Prelude.Bool
      :> Get
           '[JSON]
           FleetBookingsInformationResponse
  )

type GetDriverFleetAssignments =
  ( "fleet" :> "assignments" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> QueryParam "vehicleNo" Kernel.Prelude.Text
      :> QueryParam
           "mainAssignmentId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           FleetBookingAssignmentsResponse
  )

type GetDriverFleetAssignmentsHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "assignments" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> QueryParam
           "vehicleNo"
           Kernel.Prelude.Text
      :> QueryParam
           "mainAssignmentId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           FleetBookingAssignmentsResponse
  )

type GetDriverFleetDriverVehicleAssociation =
  ( "fleet" :> "driverVehicleAssociation" :> QueryParam "Limit" Kernel.Prelude.Int :> QueryParam "Offset" Kernel.Prelude.Int
      :> QueryParam
           "countryCode"
           Kernel.Prelude.Text
      :> QueryParam "phoneNo" Kernel.Prelude.Text
      :> QueryParam
           "vehicleNo"
           Kernel.Prelude.Text
      :> QueryParam
           "includeStats"
           Kernel.Prelude.Bool
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           DrivertoVehicleAssociationRes
  )

type GetDriverFleetGetFleetDriverVehicleAssociationHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "getFleetDriverVehicleAssociation"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam "countryCode" Kernel.Prelude.Text
      :> QueryParam
           "phoneNo"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNo"
           Kernel.Prelude.Text
      :> QueryParam
           "includeStats"
           Kernel.Prelude.Bool
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           DrivertoVehicleAssociationRes
  )

type GetDriverFleetDriverListStats =
  ( "fleet" :> "driver" :> "list" :> "stats" :> QueryParam "from" Data.Time.Day :> QueryParam "to" Data.Time.Day
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "sortDesc"
           Kernel.Prelude.Bool
      :> QueryParam
           "SortOn"
           FleetDriverListStatsSortOn
      :> QueryParam
           "responseType"
           FleetDriverStatsResponseType
      :> Get
           '[JSON]
           FleetDriverStatsListRes
  )

type GetDriverFleetDriverListStatsHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "driver" :> "list" :> "stats" :> QueryParam "from" Data.Time.Day
      :> QueryParam
           "to"
           Data.Time.Day
      :> QueryParam "search" Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "sortDesc"
           Kernel.Prelude.Bool
      :> QueryParam
           "SortOn"
           FleetDriverListStatsSortOn
      :> QueryParam
           "responseType"
           FleetDriverStatsResponseType
      :> Get
           '[JSON]
           FleetDriverStatsListRes
  )

type GetDriverFleetDriverAssociation =
  ( "fleet" :> "driverAssociation" :> QueryParam "isActive" Kernel.Prelude.Bool :> QueryParam "Limit" Kernel.Prelude.Int
      :> QueryParam
           "Offset"
           Kernel.Prelude.Int
      :> QueryParam "countryCode" Kernel.Prelude.Text
      :> QueryParam
           "phoneNo"
           Kernel.Prelude.Text
      :> QueryParam
           "includeStats"
           Kernel.Prelude.Bool
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "status"
           DriverMode
      :> QueryParam
           "name"
           Kernel.Prelude.Text
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam
           "mbRequestorId"
           Kernel.Prelude.Text
      :> QueryParam
           "hasFleetMemberHierarchy"
           Kernel.Prelude.Bool
      :> QueryParam
           "isRequestorFleerOwner"
           Kernel.Prelude.Bool
      :> QueryParam
           "hasRequestReason"
           Kernel.Prelude.Bool
      :> Get
           '[JSON]
           DrivertoVehicleAssociationResT
  )

type GetDriverFleetVehicleAssociation =
  ( "fleet" :> "vehicleAssociation" :> QueryParam "Limit" Kernel.Prelude.Int :> QueryParam "Offset" Kernel.Prelude.Int
      :> QueryParam
           "vehicleNo"
           Kernel.Prelude.Text
      :> QueryParam "includeStats" Kernel.Prelude.Bool
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "status"
           FleetVehicleStatus
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> QueryParam
           "statusAwareVehicleNo"
           Kernel.Prelude.Text
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam
           "mbRequestorId"
           Kernel.Prelude.Text
      :> QueryParam
           "hasFleetMemberHierarchy"
           Kernel.Prelude.Bool
      :> QueryParam
           "isRequestorFleerOwner"
           Kernel.Prelude.Bool
      :> Get
           '[JSON]
           DrivertoVehicleAssociationResT
  )

type PostDriverFleetVehicleDriverRcStatus =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "fleet" :> "vehicleDriverRCstatus"
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> ReqBody '[JSON] RCStatusReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetVehicleDriverRCstatusHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> Capture
           "requestorId"
           Kernel.Prelude.Text
      :> "fleet"
      :> "vehicleDriverRCstatus"
      :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> ReqBody '[JSON] RCStatusReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateFleetOwnerInfo =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateFleetOwnerInfo" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           UpdateFleetOwnerInfoReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateFleetOwnerInfoHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateFleetOwnerInfo" :> ReqBody '[JSON] UpdateFleetOwnerInfoReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetOwnerInfo = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "fleetOwnerInfo" :> Get '[JSON] FleetOwnerInfoRes)

type GetDriverFleetOperatorInfo =
  ( "fleetOperatorInfo" :> QueryParam "mobileCountryCode" Kernel.Prelude.Text :> QueryParam "mobileNumber" Kernel.Prelude.Text
      :> QueryParam
           "personId"
           Kernel.Prelude.Text
      :> QueryParam "role" Kernel.Prelude.Text
      :> Get '[JSON] FleetOwnerInfoRes
  )

type GetDriverFleetOperatorInfoHelper =
  ( "fleetOperatorInfo" :> QueryParam "mobileCountryCode" Kernel.Prelude.Text :> QueryParam "mobileNumber" Kernel.Prelude.Text
      :> QueryParam
           "personId"
           Kernel.Prelude.Text
      :> QueryParam "role" Kernel.Prelude.Text
      :> Get '[JSON] FleetOwnerInfoRes
  )

type PostDriverFleetSendJoiningOtp =
  ( "fleet" :> "sendJoiningOtp" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post '[JSON] Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
  )

type PostDriverFleetDriverSendJoiningOtpHelper =
  ( Capture "fleetOwnerName" Kernel.Prelude.Text :> "fleet" :> "driver" :> "sendJoiningOtp"
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
  )

type PostDriverFleetVerifyJoiningOtp =
  ( "fleet" :> "verifyJoiningOtp" :> QueryParam "authId" Kernel.Prelude.Text :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           VerifyFleetJoiningOtpReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetDriverVerifyJoiningOtpHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "driver" :> "verifyJoiningOtp"
      :> QueryParam
           "authId"
           Kernel.Prelude.Text
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody '[JSON] VerifyFleetJoiningOtpReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetRoutes =
  ( "fleet" :> "routes" :> QueryParam "mbCurrentLocation" Kernel.External.Maps.Types.LatLong :> QueryParam "mbSearchString" Kernel.Prelude.Text
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "limit" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           RouteAPIResp
  )

type GetDriverFleetRoutesHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "routes" :> QueryParam "mbCurrentLocation" Kernel.External.Maps.Types.LatLong
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "limit" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           RouteAPIResp
  )

type GetDriverFleetPossibleRoutes =
  ( "fleet" :> "possibleRoutes" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> MandatoryQueryParam "startStopCode" Kernel.Prelude.Text
      :> Get
           '[JSON]
           RouteAPIResp
  )

type GetDriverFleetPossibleRoutesHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "possibleRoutes" :> MandatoryQueryParam "startStopCode" Kernel.Prelude.Text
      :> Get
           '[JSON]
           RouteAPIResp
  )

type PostDriverFleetTripPlanner = ("fleet" :> "tripPlanner" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] TripPlannerReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverFleetTripPlannerHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "tripPlanner" :> ReqBody '[JSON] TripPlannerReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetTripTransactions =
  ( "fleet" :> "trip" :> "transactions" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> QueryParam
           "mbFrom"
           Kernel.Prelude.UTCTime
      :> QueryParam "mbTo" Kernel.Prelude.UTCTime
      :> QueryParam
           "mbVehicleNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam
           "mbMemberPersonId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "limit"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           TripTransactionRespT
  )

type PostDriverFleetAddDrivers =
  ( "fleet" :> "addDrivers" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           CreateDriversReq
      :> Post '[JSON] APISuccessWithUnprocessedEntities
  )

type PostDriverFleetAddDriversHelper =
  ( "fleet" :> "addDrivers" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           CreateDriversReq
      :> Post '[JSON] APISuccessWithUnprocessedEntities
  )

type PostDriverFleetAddDriverBusRouteMapping =
  ( "fleet" :> "addDriverBusRouteMapping"
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp CreateDriverBusRouteMappingReq
      :> Post '[JSON] APISuccessWithUnprocessedEntities
  )

type PostDriverFleetAddDriverBusRouteMappingHelper =
  ( "fleet" :> "addDriverBusRouteMapping"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           CreateDriverBusRouteMappingReq
      :> Post '[JSON] APISuccessWithUnprocessedEntities
  )

type PostDriverFleetLinkRCWithDriver =
  ( "fleet" :> "linkRCWithDriver" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] LinkRCWithDriverForFleetReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetLinkRCWithDriverHelper =
  ( "fleet" :> "linkRCWithDriver" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           LinkRCWithDriverForFleetReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverDashboardFleetWmbTripEnd =
  ( "dashboard" :> "fleet" :> "wmb" :> "trip"
      :> Capture
           "tripTransactionId"
           (Kernel.Types.Id.Id Dashboard.Common.TripTransaction)
      :> "end"
      :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam "terminationSource" Dashboard.Common.ActionSource
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverDashboardFleetWmbTripEndHelper =
  ( "dashboard" :> "fleet" :> "wmb" :> "trip"
      :> Capture
           "tripTransactionId"
           (Kernel.Types.Id.Id Dashboard.Common.TripTransaction)
      :> Capture "fleetOwnerId" Kernel.Prelude.Text
      :> "end"
      :> QueryParam "terminationSource" Dashboard.Common.ActionSource
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverDashboardFleetTripWaypoints =
  ( "dashboard" :> "fleet" :> "trip" :> Capture "tripTransactionId" (Kernel.Types.Id.Id Dashboard.Common.TripTransaction)
      :> Capture
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> "waypoints"
      :> QueryParam "liteMode" Kernel.Prelude.Bool
      :> QueryParam
           "memberPersonId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           TripTransactionWaypointsRes
  )

type GetDriverDashboardFleetTripWaypointsHelper =
  ( "dashboard" :> "fleet" :> "trip" :> Capture "tripTransactionId" (Kernel.Types.Id.Id Dashboard.Common.TripTransaction)
      :> Capture
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> "waypoints"
      :> QueryParam "liteMode" Kernel.Prelude.Bool
      :> QueryParam
           "memberPersonId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           TripTransactionWaypointsRes
  )

type GetDriverFleetWmbRouteDetails =
  ( "fleet" :> "wmb" :> "route" :> Capture "routeCode" Kernel.Prelude.Text :> "details" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> Get
           '[JSON]
           RouteDetails
  )

type GetDriverFleetWmbRouteDetailsHelper =
  ( "fleet" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "wmb" :> "route" :> Capture "routeCode" Kernel.Prelude.Text :> "details"
      :> Get
           '[JSON]
           RouteDetails
  )

type PostDriverFleetGetNearbyDrivers = ("fleet" :> "getNearbyDrivers" :> ReqBody '[JSON] NearbyDriverReq :> Post '[JSON] NearbyDriverRespT)

type PostDriverFleetGetNearbyDriversHelper = ("fleet" :> "getNearbyDrivers" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] NearbyDriverReq :> Post '[JSON] NearbyDriverResp)

type PostDriverDashboardFleetTrackDriver =
  ( "dashboard" :> "fleet" :> "track" :> "driver" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           TrackDriverLocationsReq
      :> Post '[JSON] TrackDriverLocationsRes
  )

type PostDriverDashboardFleetTrackDriverHelper =
  ( "dashboard" :> "fleet" :> "track" :> "driver" :> Capture "fleetOwnerId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           TrackDriverLocationsReq
      :> Post '[JSON] TrackDriverLocationsRes
  )

type GetDriverDashboardInternalHelperGetFleetOwnerId =
  ( "dashboard" :> "internal" :> "helper" :> "getFleetOwnerId"
      :> QueryParam
           "mbFleetOwnerId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "memberPersonId" Kernel.Prelude.Text
      :> Get '[JSON] Kernel.Prelude.Text
  )

type GetDriverDashboardInternalHelperGetFleetOwnerIds =
  ( "dashboard" :> "internal" :> "helper" :> "getFleetOwnerIds"
      :> QueryParam
           "mbFleetOwnerId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "memberPersonId" Kernel.Prelude.Text
      :> Get '[JSON] [(Kernel.Prelude.Text, Kernel.Prelude.Text)]
  )

type GetDriverFleetStatus = ("fleet" :> "status" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> Get '[JSON] DriverStatusRes)

type GetDriverFleetStatusHelper = (Capture "requestorId" Kernel.Prelude.Text :> "fleet" :> "status" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> Get '[JSON] DriverStatusRes)

type PostDriverFleetLocationList = ("fleet" :> "locationList" :> ReqBody '[JSON] DriverLocationListReq :> Post '[JSON] DriverLocationListResp)

type PostDriverFleetLocationListHelper = ("fleet" :> "locationList" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] DriverLocationListReq :> Post '[JSON] DriverLocationListResp)

type PostDriverFleetGetDriverDetails = ("fleet" :> "getDriverDetails" :> ReqBody '[JSON] DriverDetailsReq :> Post '[JSON] DriverDetailsResp)

type PostDriverFleetGetDriverDetailsHelper = ("fleet" :> "getDriverDetails" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] DriverDetailsReq :> Post '[JSON] DriverDetailsResp)

type PostDriverFleetGetNearbyDriversV2 = ("fleet" :> "getNearbyDriversV2" :> ReqBody '[JSON] NearbyDriversReqV2 :> Post '[JSON] NearbyDriversRespTV2)

type PostDriverFleetGetNearbyDriversV2Helper =
  ( "fleet" :> "getNearbyDriversV2" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] NearbyDriversReqV2
      :> Post
           '[JSON]
           NearbyDriversRespV2
  )

type GetDriverFleetDashboardAnalyticsAllTime = ("fleet" :> "dashboard" :> "analytics" :> "allTime" :> Get '[JSON] AllTimeFleetAnalyticsRes)

type GetDriverFleetDashboardAnalyticsAllTimeHelper = (Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "dashboard" :> "analytics" :> "allTime" :> Get '[JSON] AllTimeFleetAnalyticsRes)

type GetDriverFleetDashboardAnalytics =
  ( "fleet" :> "dashboard" :> "analytics" :> QueryParam "responseType" FleetAnalyticsResponseType
      :> MandatoryQueryParam
           "from"
           Data.Time.Day
      :> MandatoryQueryParam "to" Data.Time.Day
      :> Get '[JSON] FleetAnalyticsRes
  )

type GetDriverFleetDashboardAnalyticsHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "dashboard" :> "analytics"
      :> QueryParam
           "responseType"
           FleetAnalyticsResponseType
      :> MandatoryQueryParam "from" Data.Time.Day
      :> MandatoryQueryParam "to" Data.Time.Day
      :> Get '[JSON] FleetAnalyticsRes
  )

type PostDriverFleetDashboardAnalyticsCache =
  ( "fleet" :> "dashboard" :> "analytics" :> "cache" :> ReqBody '[JSON] FleetDashboardAnalyticsCacheReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverDashboardFleetEstimateRoute =
  ( "dashboard" :> "fleet" :> "estimateRoute" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] EstimateRouteReq
      :> Post
           '[JSON]
           Kernel.External.Maps.GetRoutesResp
  )

type PostDriverDashboardFleetEstimateRouteHelper =
  ( "dashboard" :> "fleet" :> "estimateRoute" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] EstimateRouteReq
      :> Post
           '[JSON]
           Kernel.External.Maps.GetRoutesResp
  )

type PostDriverFleetApproveDriver = ("fleet" :> "approveDriver" :> ReqBody '[JSON] ApproveDriverReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverFleetApproveDriverHelper =
  ( "fleet" :> "approveDriver" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] ApproveDriverReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetDriverUpdate =
  ( "fleet" :> "driver" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "update" :> ReqBody '[JSON] UpdateDriverReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetDriverUpdateHelper =
  ( "fleet" :> "driver" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "update"
      :> Capture
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> ReqBody '[JSON] UpdateDriverReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetDriverDetails = ("fleet" :> "driver" :> "details" :> MandatoryQueryParam "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> Get '[JSON] DriverDetailsRes)

type GetDriverFleetDriverDetailsHelper =
  ( "fleet" :> "driver" :> "details" :> Capture "fleetOwnerId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "driverId"
           (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> Get '[JSON] DriverDetailsRes
  )

type PostDriverFleetTripTransactionsV2 =
  ( "fleet" :> "trip" :> "transactions" :> "v2" :> QueryParam "mbFrom" Kernel.Prelude.UTCTime
      :> QueryParam
           "mbTo"
           Kernel.Prelude.UTCTime
      :> QueryParam "mbVehicleNumber" Kernel.Prelude.Text
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam
           "mbMemberPersonId"
           Kernel.Prelude.Text
      :> QueryParam
           "mbStatus"
           TripStatus
      :> QueryParam
           "mbDriverId"
           Kernel.Prelude.Text
      :> QueryParam
           "mbDutyType"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "limit"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> Post
           '[JSON]
           TripTransactionRespT
  )

type GetDriverFleetVehicleListStats =
  ( "fleet" :> "vehicle" :> "list" :> "stats" :> QueryParam "vehicleNo" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam "from" Data.Time.Day
      :> MandatoryQueryParam
           "to"
           Data.Time.Day
      :> Get
           '[JSON]
           FleetVehicleStatsRes
  )

type GetDriverFleetVehicleListStatsHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "vehicle" :> "list" :> "stats" :> QueryParam "vehicleNo" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "from"
           Data.Time.Day
      :> MandatoryQueryParam
           "to"
           Data.Time.Day
      :> Get
           '[JSON]
           FleetVehicleStatsRes
  )

type GetDriverFleetDriverOnboardedDriversAndUnlinkedVehicles =
  ( "fleet" :> "driver" :> "OnboardedDriversAndUnlinkedVehicles" :> Capture "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get
           '[JSON]
           OnboardedDriversAndUnlinkedVehiclesRes
  )

type GetDriverFleetDriverOnboardedDriversAndUnlinkedVehiclesHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "driver" :> "OnboardedDriversAndUnlinkedVehicles"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get
           '[JSON]
           OnboardedDriversAndUnlinkedVehiclesRes
  )

type GetDriverFleetScheduledBookingList =
  ( "fleet" :> "scheduledBooking" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "from"
           Data.Time.Day
      :> QueryParam "to" Data.Time.Day
      :> QueryParam
           "tripCategory"
           Dashboard.Common.TripCategory
      :> QueryParam
           "currentLocation"
           Kernel.External.Maps.Types.LatLong
      :> Get
           '[JSON]
           FleetScheduledBookingListRes
  )

type GetDriverFleetScheduledBookingListHelper =
  ( "fleet" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "scheduledBooking" :> "list" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "from" Data.Time.Day
      :> QueryParam
           "to"
           Data.Time.Day
      :> QueryParam
           "tripCategory"
           Dashboard.Common.TripCategory
      :> QueryParam
           "currentLocation"
           Kernel.External.Maps.Types.LatLong
      :> Get
           '[JSON]
           FleetScheduledBookingListRes
  )

type PostDriverFleetScheduledBookingAssign = ("fleet" :> "scheduledBooking" :> "assign" :> ReqBody '[JSON] AssignScheduledBookingReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverFleetScheduledBookingAssignHelper =
  ( "fleet" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "scheduledBooking" :> "assign"
      :> ReqBody
           '[JSON]
           AssignScheduledBookingReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data DriverAPIs = DriverAPIs
  { getDriverFleetAccessList :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetOwnerListRes,
    postDriverFleetAccessSelect :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetV2AccessSelect :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetV2AccessMultiOwnerIdSelect :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> MultiOwnerSelect -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetAddVehicles :: (Data.ByteString.Lazy.ByteString, CreateVehiclesReq) -> EulerHS.Types.EulerClient APISuccessWithUnprocessedEntities,
    postDriverFleetAddVehicle :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Dashboard.Common.Role -> AddVehicleReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetGetDriverRequests :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestType.AlertRequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient DriverRequestRespT,
    postDriverFleetRespondDriverRequest :: Kernel.Prelude.Text -> RequestRespondReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetAddRCWithoutDriver :: Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetGetAllVehicle :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient ListVehicleResT,
    getDriverFleetGetAllDriver :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetListDriverResT,
    getDriverFleetGetAllBadge :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.FleetBadgeType.FleetBadgeType -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetBadgeResT,
    postDriverFleetUnlink :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetRemoveVehicle :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetRemoveDriver :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetTotalEarning :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient FleetTotalEarningResponse,
    getDriverFleetVehicleEarning :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient FleetEarningListRes,
    getDriverFleetDriverEarning :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe SortOn -> EulerHS.Types.EulerClient FleetEarningListRes,
    getDriverFleetBookings :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient FleetBookingsInformationResponse,
    getDriverFleetAssignments :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetBookingAssignmentsResponse,
    getDriverFleetDriverVehicleAssociation :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient DrivertoVehicleAssociationRes,
    getDriverFleetDriverListStats :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe FleetDriverListStatsSortOn -> Kernel.Prelude.Maybe FleetDriverStatsResponseType -> EulerHS.Types.EulerClient FleetDriverStatsListRes,
    getDriverFleetDriverAssociation :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe DriverMode -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient DrivertoVehicleAssociationResT,
    getDriverFleetVehicleAssociation :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe FleetVehicleStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient DrivertoVehicleAssociationResT,
    postDriverFleetVehicleDriverRcStatus :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> RCStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateFleetOwnerInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateFleetOwnerInfoReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetOwnerInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient FleetOwnerInfoRes,
    getDriverFleetOperatorInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetOwnerInfoRes,
    postDriverFleetSendJoiningOtp :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> EulerHS.Types.EulerClient Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes,
    postDriverFleetVerifyJoiningOtp :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> VerifyFleetJoiningOtpReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetRoutes :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> EulerHS.Types.EulerClient RouteAPIResp,
    getDriverFleetPossibleRoutes :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient RouteAPIResp,
    postDriverFleetTripPlanner :: Kernel.Prelude.Text -> TripPlannerReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetTripTransactions :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> EulerHS.Types.EulerClient TripTransactionRespT,
    postDriverFleetAddDrivers :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> (Data.ByteString.Lazy.ByteString, CreateDriversReq) -> EulerHS.Types.EulerClient APISuccessWithUnprocessedEntities,
    postDriverFleetAddDriverBusRouteMapping :: (Data.ByteString.Lazy.ByteString, CreateDriverBusRouteMappingReq) -> EulerHS.Types.EulerClient APISuccessWithUnprocessedEntities,
    postDriverFleetLinkRCWithDriver :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> LinkRCWithDriverForFleetReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverDashboardFleetWmbTripEnd :: Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Dashboard.Common.ActionSource -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverDashboardFleetTripWaypoints :: Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient TripTransactionWaypointsRes,
    getDriverFleetWmbRouteDetails :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient RouteDetails,
    postDriverFleetGetNearbyDrivers :: Kernel.Prelude.Text -> NearbyDriverReq -> EulerHS.Types.EulerClient NearbyDriverResp,
    postDriverDashboardFleetTrackDriver :: Kernel.Prelude.Text -> TrackDriverLocationsReq -> EulerHS.Types.EulerClient TrackDriverLocationsRes,
    getDriverDashboardInternalHelperGetFleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Prelude.Text,
    getDriverDashboardInternalHelperGetFleetOwnerIds :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient [(Kernel.Prelude.Text, Kernel.Prelude.Text)],
    getDriverFleetStatus :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient DriverStatusRes,
    postDriverFleetLocationList :: Kernel.Prelude.Text -> DriverLocationListReq -> EulerHS.Types.EulerClient DriverLocationListResp,
    postDriverFleetGetDriverDetails :: Kernel.Prelude.Text -> DriverDetailsReq -> EulerHS.Types.EulerClient DriverDetailsResp,
    postDriverFleetGetNearbyDriversV2 :: Kernel.Prelude.Text -> NearbyDriversReqV2 -> EulerHS.Types.EulerClient NearbyDriversRespV2,
    getDriverFleetDashboardAnalyticsAllTime :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient AllTimeFleetAnalyticsRes,
    getDriverFleetDashboardAnalytics :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe FleetAnalyticsResponseType -> Data.Time.Day -> Data.Time.Day -> EulerHS.Types.EulerClient FleetAnalyticsRes,
    postDriverFleetDashboardAnalyticsCache :: FleetDashboardAnalyticsCacheReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverDashboardFleetEstimateRoute :: Kernel.Prelude.Text -> EstimateRouteReq -> EulerHS.Types.EulerClient Kernel.External.Maps.GetRoutesResp,
    postDriverFleetApproveDriver :: Kernel.Prelude.Text -> ApproveDriverReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetDriverUpdate :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> UpdateDriverReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetDriverDetails :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient DriverDetailsRes,
    postDriverFleetTripTransactionsV2 :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe TripStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> EulerHS.Types.EulerClient TripTransactionRespT,
    getDriverFleetVehicleListStats :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Data.Time.Day -> EulerHS.Types.EulerClient FleetVehicleStatsRes,
    getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient OnboardedDriversAndUnlinkedVehiclesRes,
    getDriverFleetScheduledBookingList :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Dashboard.Common.TripCategory -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> EulerHS.Types.EulerClient FleetScheduledBookingListRes,
    postDriverFleetScheduledBookingAssign :: Kernel.Prelude.Text -> AssignScheduledBookingReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverFleetAccessList :<|> postDriverFleetAccessSelect :<|> postDriverFleetV2AccessSelect :<|> postDriverFleetV2AccessMultiOwnerIdSelect :<|> postDriverFleetAddVehicles :<|> postDriverFleetAddVehicle :<|> getDriverFleetGetDriverRequests :<|> postDriverFleetRespondDriverRequest :<|> postDriverFleetAddRCWithoutDriver :<|> getDriverFleetGetAllVehicle :<|> getDriverFleetGetAllDriver :<|> getDriverFleetGetAllBadge :<|> postDriverFleetUnlink :<|> postDriverFleetRemoveVehicle :<|> postDriverFleetRemoveDriver :<|> getDriverFleetTotalEarning :<|> getDriverFleetVehicleEarning :<|> getDriverFleetDriverEarning :<|> getDriverFleetBookings :<|> getDriverFleetAssignments :<|> getDriverFleetDriverVehicleAssociation :<|> getDriverFleetDriverListStats :<|> getDriverFleetDriverAssociation :<|> getDriverFleetVehicleAssociation :<|> postDriverFleetVehicleDriverRcStatus :<|> postDriverUpdateFleetOwnerInfo :<|> getDriverFleetOwnerInfo :<|> getDriverFleetOperatorInfo :<|> postDriverFleetSendJoiningOtp :<|> postDriverFleetVerifyJoiningOtp :<|> getDriverFleetRoutes :<|> getDriverFleetPossibleRoutes :<|> postDriverFleetTripPlanner :<|> getDriverFleetTripTransactions :<|> postDriverFleetAddDrivers :<|> postDriverFleetAddDriverBusRouteMapping :<|> postDriverFleetLinkRCWithDriver :<|> postDriverDashboardFleetWmbTripEnd :<|> getDriverDashboardFleetTripWaypoints :<|> getDriverFleetWmbRouteDetails :<|> postDriverFleetGetNearbyDrivers :<|> postDriverDashboardFleetTrackDriver :<|> getDriverDashboardInternalHelperGetFleetOwnerId :<|> getDriverDashboardInternalHelperGetFleetOwnerIds :<|> getDriverFleetStatus :<|> postDriverFleetLocationList :<|> postDriverFleetGetDriverDetails :<|> postDriverFleetGetNearbyDriversV2 :<|> getDriverFleetDashboardAnalyticsAllTime :<|> getDriverFleetDashboardAnalytics :<|> postDriverFleetDashboardAnalyticsCache :<|> postDriverDashboardFleetEstimateRoute :<|> postDriverFleetApproveDriver :<|> postDriverFleetDriverUpdate :<|> getDriverFleetDriverDetails :<|> postDriverFleetTripTransactionsV2 :<|> getDriverFleetVehicleListStats :<|> getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles :<|> getDriverFleetScheduledBookingList :<|> postDriverFleetScheduledBookingAssign = driverClient

data DriverUserActionType
  = GET_DRIVER_FLEET_ACCESS_LIST
  | POST_DRIVER_FLEET_ACCESS_SELECT
  | POST_DRIVER_FLEET_V2_ACCESS_SELECT
  | POST_DRIVER_FLEET_V2_ACCESS_MULTI_OWNER_ID_SELECT
  | POST_DRIVER_FLEET_ADD_VEHICLES
  | POST_DRIVER_FLEET_ADD_VEHICLE
  | GET_DRIVER_FLEET_GET_DRIVER_REQUESTS
  | POST_DRIVER_FLEET_RESPOND_DRIVER_REQUEST
  | POST_DRIVER_FLEET_ADD_RC_WITHOUT_DRIVER
  | GET_DRIVER_FLEET_GET_ALL_VEHICLE
  | GET_DRIVER_FLEET_GET_ALL_DRIVER
  | GET_DRIVER_FLEET_GET_ALL_BADGE
  | POST_DRIVER_FLEET_UNLINK
  | POST_DRIVER_FLEET_REMOVE_VEHICLE
  | POST_DRIVER_FLEET_REMOVE_DRIVER
  | GET_DRIVER_FLEET_TOTAL_EARNING
  | GET_DRIVER_FLEET_VEHICLE_EARNING
  | GET_DRIVER_FLEET_DRIVER_EARNING
  | GET_DRIVER_FLEET_BOOKINGS
  | GET_DRIVER_FLEET_ASSIGNMENTS
  | GET_DRIVER_FLEET_DRIVER_VEHICLE_ASSOCIATION
  | GET_DRIVER_FLEET_DRIVER_LIST_STATS
  | GET_DRIVER_FLEET_DRIVER_ASSOCIATION
  | GET_DRIVER_FLEET_VEHICLE_ASSOCIATION
  | POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS
  | POST_DRIVER_UPDATE_FLEET_OWNER_INFO
  | GET_DRIVER_FLEET_OWNER_INFO
  | GET_DRIVER_FLEET_OPERATOR_INFO
  | POST_DRIVER_FLEET_SEND_JOINING_OTP
  | POST_DRIVER_FLEET_VERIFY_JOINING_OTP
  | GET_DRIVER_FLEET_ROUTES
  | GET_DRIVER_FLEET_POSSIBLE_ROUTES
  | POST_DRIVER_FLEET_TRIP_PLANNER
  | GET_DRIVER_FLEET_TRIP_TRANSACTIONS
  | POST_DRIVER_FLEET_ADD_DRIVERS
  | POST_DRIVER_FLEET_ADD_DRIVER_BUS_ROUTE_MAPPING
  | POST_DRIVER_FLEET_LINK_RC_WITH_DRIVER
  | POST_DRIVER_DASHBOARD_FLEET_WMB_TRIP_END
  | GET_DRIVER_DASHBOARD_FLEET_TRIP_WAYPOINTS
  | GET_DRIVER_FLEET_WMB_ROUTE_DETAILS
  | POST_DRIVER_FLEET_GET_NEARBY_DRIVERS
  | POST_DRIVER_DASHBOARD_FLEET_TRACK_DRIVER
  | GET_DRIVER_DASHBOARD_INTERNAL_HELPER_GET_FLEET_OWNER_ID
  | GET_DRIVER_DASHBOARD_INTERNAL_HELPER_GET_FLEET_OWNER_IDS
  | GET_DRIVER_FLEET_STATUS
  | POST_DRIVER_FLEET_LOCATION_LIST
  | POST_DRIVER_FLEET_GET_DRIVER_DETAILS
  | POST_DRIVER_FLEET_GET_NEARBY_DRIVERS_V2
  | GET_DRIVER_FLEET_DASHBOARD_ANALYTICS_ALL_TIME
  | GET_DRIVER_FLEET_DASHBOARD_ANALYTICS
  | POST_DRIVER_FLEET_DASHBOARD_ANALYTICS_CACHE
  | POST_DRIVER_DASHBOARD_FLEET_ESTIMATE_ROUTE
  | POST_DRIVER_FLEET_APPROVE_DRIVER
  | POST_DRIVER_FLEET_DRIVER_UPDATE
  | GET_DRIVER_FLEET_DRIVER_DETAILS
  | POST_DRIVER_FLEET_TRIP_TRANSACTIONS_V2
  | GET_DRIVER_FLEET_VEHICLE_LIST_STATS
  | GET_DRIVER_FLEET_DRIVER_ONBOARDED_DRIVERS_AND_UNLINKED_VEHICLES
  | GET_DRIVER_FLEET_SCHEDULED_BOOKING_LIST
  | POST_DRIVER_FLEET_SCHEDULED_BOOKING_ASSIGN
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''DriverMode)

$(mkHttpInstancesForEnum ''FleetAnalyticsResponseType)

$(mkHttpInstancesForEnum ''FleetDriverListStatsSortOn)

$(mkHttpInstancesForEnum ''FleetDriverStatsResponseType)

$(mkHttpInstancesForEnum ''FleetVehicleStatus)

$(mkHttpInstancesForEnum ''SortOn)

$(mkHttpInstancesForEnum ''TripStatus)

$(Data.Singletons.TH.genSingletons [''DriverUserActionType])
