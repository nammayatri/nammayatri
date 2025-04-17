{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.Driver where

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
import qualified Domain.Types.Alert.AlertRequestType
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
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

data AddVehicleReq = AddVehicleReq
  { registrationNo :: Kernel.Prelude.Text,
    vehicleClass :: Kernel.Prelude.Text,
    capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    colour :: Kernel.Prelude.Text,
    energyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    model :: Kernel.Prelude.Text,
    make :: Kernel.Prelude.Text,
    airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverName :: Kernel.Prelude.Text,
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

data CreateVehiclesReq = CreateVehiclesReq {file :: Kernel.Prelude.FilePath, fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateVehiclesReq where
  hideSecrets = Kernel.Prelude.identity

data DriveVehicleAssociationListItem = DriveVehicleAssociationListItem
  { driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
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
    fleetOwnerName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverInfo = DriverInfo
  { routeCode :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    rideStatus :: RideStatus,
    routeLongName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    point :: Kernel.External.Maps.Types.LatLong
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
    fleetOwnerName :: Kernel.Prelude.Text
  }
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

data DrivertoVehicleAssociationRes = DrivertoVehicleAssociationRes {fleetOwnerId :: Kernel.Prelude.Text, listItem :: [DriveVehicleAssociationListItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DrivertoVehicleAssociationResT = DrivertoVehicleAssociationResT {fleetOwnerId :: Kernel.Prelude.Text, listItem :: [DriveVehicleAssociationListItemT], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetBadgeRes = FleetBadgeRes {fleetBadgeInfos :: [FleetBadgesAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetBadgeResT = FleetBadgeResT {fleetBadgeInfos :: [FleetBadgesAPIEntityT]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBadgesAPIEntity = FleetBadgesAPIEntity {badgeName :: Kernel.Prelude.Text, isActive :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBadgesAPIEntityT = FleetBadgesAPIEntityT {badgeName :: Kernel.Prelude.Text, isActive :: Kernel.Prelude.Bool, fleetOwnerId :: Kernel.Prelude.Text, fleetOwnerName :: Kernel.Prelude.Text}
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

newtype FleetListDriverRes = FleetListDriverRes {fleetDriversInfos :: [FleetDriversAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetListDriverResT = FleetListDriverResT {fleetDriversInfos :: [FleetDriversAPIEntityT]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerInfoRes = FleetOwnerInfoRes
  { blocked :: Kernel.Prelude.Bool,
    enabled :: Kernel.Prelude.Bool,
    fleetType :: Kernel.Prelude.Text,
    verified :: Kernel.Prelude.Bool,
    gstNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetConfig :: Kernel.Prelude.Maybe FleetConfig,
    operatorName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    operatorContact :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetOwnerInfoRes where
  hideSecrets = Kernel.Prelude.identity

data FleetOwnerListAPIEntity = FleetOwnerListAPIEntity {fleetOwnerId :: Kernel.Prelude.Text, fleetOwnerName :: Kernel.Prelude.Text, enabled :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetOwnerListRes = FleetOwnerListRes {ownersList :: [FleetOwnerListAPIEntity]}
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

data FleetVehicleStatus
  = Active
  | InActive
  | Valid
  | Invalid
  | Pending
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

data NearbyDriverReq = NearbyDriverReq {point :: Kernel.External.Maps.Types.LatLong, radius :: Kernel.Prelude.Int}
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

data RequestRespondReq = RequestRespondReq {status :: Domain.Types.Alert.AlertRequestStatus, approvalRequestId :: Kernel.Prelude.Text, reason :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RequestRespondReq where
  hideSecrets = Kernel.Prelude.identity

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

data TripDetails = TripDetails {routeCode :: Kernel.Prelude.Text, roundTrip :: Kernel.Prelude.Maybe RoundTripDetail}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripPlannerReq = TripPlannerReq {driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver, badgeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text, trips :: [TripDetails]}
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
    fleetOwnerName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripTransactionResp = TripTransactionResp {trips :: [TripTransactionDetail], totalTrips :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripTransactionRespT = TripTransactionRespT {trips :: [TripTransactionDetailT], totalTrips :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateFleetOwnerInfoReq = UpdateFleetOwnerInfoReq
  { firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text
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
    driverLicense :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus
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

type API = ("driver" :> (GetDriverFleetAccessList :<|> PostDriverFleetAccessSelect :<|> PostDriverFleetAddVehiclesHelper :<|> PostDriverFleetAddVehicleHelper :<|> GetDriverFleetDriverRequestsHelper :<|> PostDriverFleetDriverRequestRespondHelper :<|> PostDriverFleetAddRCWithoutDriverHelper :<|> GetDriverFleetGetAllVehicleHelper :<|> GetDriverFleetGetAllDriverHelper :<|> GetDriverFleetGetAllBadgeHelper :<|> PostDriverFleetUnlinkHelper :<|> PostDriverFleetRemoveVehicleHelper :<|> PostDriverFleetRemoveDriverHelper :<|> GetDriverFleetTotalEarningHelper :<|> GetDriverFleetVehicleEarningHelper :<|> GetDriverFleetDriverEarningHelper :<|> GetDriverFleetGetFleetDriverVehicleAssociationHelper :<|> GetDriverFleetGetFleetDriverAssociationHelper :<|> GetDriverFleetGetFleetVehicleAssociationHelper :<|> PostDriverFleetVehicleDriverRCstatusHelper :<|> PostDriverUpdateFleetOwnerInfoHelper :<|> GetDriverFleetOwnerInfo :<|> PostDriverFleetDriverSendJoiningOtpHelper :<|> PostDriverFleetDriverVerifyJoiningOtpHelper :<|> GetDriverFleetRoutesHelper :<|> GetDriverFleetPossibleRoutesHelper :<|> PostDriverFleetTripPlannerHelper :<|> GetDriverFleetTripTransactionsHelper :<|> PostDriverFleetAddDriversHelper :<|> PostDriverFleetAddDriverBusRouteMappingHelper :<|> PostDriverFleetLinkRCWithDriverHelper :<|> PostDriverDashboardFleetWmbTripEndHelper :<|> GetDriverFleetWmbRouteDetailsHelper :<|> PostDriverFleetGetNearbyDriversHelper :<|> PostDriverDashboardFleetTrackDriverHelper))

type GetDriverFleetAccessList = ("fleet" :> "access" :> "list" :> Get '[JSON] FleetOwnerListRes)

type PostDriverFleetAccessSelect =
  ( "fleet" :> "access" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "select" :> QueryParam "onlySingle" Kernel.Prelude.Bool
      :> MandatoryQueryParam
           "enable"
           Kernel.Prelude.Bool
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
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
      :> ReqBody '[JSON] AddVehicleReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddVehicleHelper =
  ( Capture "mobileNo" Kernel.Prelude.Text :> Capture "requestorId" Kernel.Prelude.Text :> "fleet" :> "addVehicle"
      :> QueryParam
           "mobileCountryCode"
           Kernel.Prelude.Text
      :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> ReqBody '[JSON] AddVehicleReq
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
           "mbLimit"
           Kernel.Prelude.Int
      :> QueryParam
           "mbOffset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           DriverRequestRespT
  )

type GetDriverFleetDriverRequestsHelper =
  ( "fleet" :> "driverRequests" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "requestType"
           Domain.Types.Alert.AlertRequestType.AlertRequestType
      :> QueryParam
           "routeCode"
           Kernel.Prelude.Text
      :> QueryParam
           "driverId"
           Kernel.Prelude.Text
      :> QueryParam
           "badgeName"
           Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           DriverRequestResp
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
      :> Get '[JSON] ListVehicleResT
  )

type GetDriverFleetGetAllVehicleHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "getAllVehicle" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "mbRegNumberString" Kernel.Prelude.Text
      :> Get '[JSON] ListVehicleRes
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
      :> Get
           '[JSON]
           FleetListDriverResT
  )

type GetDriverFleetGetAllDriverHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "getAllDriver" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "mobileNumberString" Kernel.Prelude.Text
      :> QueryParam
           "nameString"
           Kernel.Prelude.Text
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           FleetListDriverRes
  )

type GetDriverFleetGetAllBadge =
  ( "fleet" :> "getAllBadge" :> QueryParam "mblimit" Kernel.Prelude.Int :> QueryParam "mboffset" Kernel.Prelude.Int
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> QueryParam "mbFleetOwnerId" Kernel.Prelude.Text
      :> Get '[JSON] FleetBadgeResT
  )

type GetDriverFleetGetAllBadgeHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "getAllBadge" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "mbSearchString" Kernel.Prelude.Text
      :> Get '[JSON] FleetBadgeRes
  )

type PostDriverFleetUnlink =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "unlink"
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetUnlinkHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> Capture
           "vehicleNo"
           Kernel.Prelude.Text
      :> "fleet"
      :> "unlink"
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveVehicle =
  ( Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "remove" :> "vehicle" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveVehicleHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "remove" :> "vehicle"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveDriver =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "fleet" :> "remove" :> "driver"
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveDriverHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text
      :> Capture
           "driverId"
           (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> "fleet"
      :> "remove"
      :> "driver"
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
      :> Get
           '[JSON]
           DrivertoVehicleAssociationResT
  )

type GetDriverFleetGetFleetDriverAssociationHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "getFleetDriverAssociation"
      :> QueryParam
           "isActive"
           Kernel.Prelude.Bool
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "countryCode"
           Kernel.Prelude.Text
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
      :> Get
           '[JSON]
           DrivertoVehicleAssociationRes
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
      :> Get
           '[JSON]
           DrivertoVehicleAssociationResT
  )

type GetDriverFleetGetFleetVehicleAssociationHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "getFleetVehicleAssociation"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam "vehicleNo" Kernel.Prelude.Text
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
           FleetVehicleStatus
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> QueryParam
           "statusAwareVehicleNo"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           DrivertoVehicleAssociationRes
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
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> "fleet"
      :> "vehicleDriverRCstatus"
      :> ReqBody '[JSON] RCStatusReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
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

type PostDriverFleetSendJoiningOtp =
  ( "fleet" :> "sendJoiningOtp" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post '[JSON] Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
  )

type PostDriverFleetDriverSendJoiningOtpHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "driver" :> "sendJoiningOtp"
      :> ReqBody
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post '[JSON] Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
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
      :> ReqBody '[JSON] VerifyFleetJoiningOtpReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
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

type GetDriverFleetTripTransactionsHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "trip" :> "transactions"
      :> Capture
           "driverId"
           (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> QueryParam "mbFrom" Kernel.Prelude.UTCTime
      :> QueryParam
           "mbTo"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "mbVehicleNumber"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "limit"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           TripTransactionResp
  )

type PostDriverFleetAddDrivers =
  ( "fleet" :> "addDrivers" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           CreateDriversReq
      :> Post '[JSON] APISuccessWithUnprocessedEntities
  )

type PostDriverFleetAddDriversHelper =
  ( "fleet" :> "addDrivers" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp CreateDriversReq
      :> Post
           '[JSON]
           APISuccessWithUnprocessedEntities
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
  ( "fleet" :> "linkRCWithDriver" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] LinkRCWithDriverForFleetReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
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

data DriverAPIs = DriverAPIs
  { getDriverFleetAccessList :: EulerHS.Types.EulerClient FleetOwnerListRes,
    postDriverFleetAccessSelect :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetAddVehicles :: (Data.ByteString.Lazy.ByteString, CreateVehiclesReq) -> EulerHS.Types.EulerClient APISuccessWithUnprocessedEntities,
    postDriverFleetAddVehicle :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> AddVehicleReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetGetDriverRequests :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestType.AlertRequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient DriverRequestResp,
    postDriverFleetRespondDriverRequest :: Kernel.Prelude.Text -> RequestRespondReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetAddRCWithoutDriver :: Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetGetAllVehicle :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient ListVehicleRes,
    getDriverFleetGetAllDriver :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetListDriverRes,
    getDriverFleetGetAllBadge :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetBadgeRes,
    postDriverFleetUnlink :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetRemoveVehicle :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetRemoveDriver :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetTotalEarning :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient FleetTotalEarningResponse,
    getDriverFleetVehicleEarning :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient FleetEarningListRes,
    getDriverFleetDriverEarning :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe SortOn -> EulerHS.Types.EulerClient FleetEarningListRes,
    getDriverFleetDriverVehicleAssociation :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient DrivertoVehicleAssociationRes,
    getDriverFleetDriverAssociation :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe DriverMode -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient DrivertoVehicleAssociationRes,
    getDriverFleetVehicleAssociation :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe FleetVehicleStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient DrivertoVehicleAssociationRes,
    postDriverFleetVehicleDriverRcStatus :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> RCStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateFleetOwnerInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateFleetOwnerInfoReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetOwnerInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient FleetOwnerInfoRes,
    postDriverFleetSendJoiningOtp :: Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> EulerHS.Types.EulerClient Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes,
    postDriverFleetVerifyJoiningOtp :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> VerifyFleetJoiningOtpReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetRoutes :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> EulerHS.Types.EulerClient RouteAPIResp,
    getDriverFleetPossibleRoutes :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient RouteAPIResp,
    postDriverFleetTripPlanner :: Kernel.Prelude.Text -> TripPlannerReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetTripTransactions :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> EulerHS.Types.EulerClient TripTransactionResp,
    postDriverFleetAddDrivers :: (Data.ByteString.Lazy.ByteString, CreateDriversReq) -> EulerHS.Types.EulerClient APISuccessWithUnprocessedEntities,
    postDriverFleetAddDriverBusRouteMapping :: (Data.ByteString.Lazy.ByteString, CreateDriverBusRouteMappingReq) -> EulerHS.Types.EulerClient APISuccessWithUnprocessedEntities,
    postDriverFleetLinkRCWithDriver :: Kernel.Prelude.Text -> LinkRCWithDriverForFleetReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverDashboardFleetWmbTripEnd :: Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Dashboard.Common.ActionSource -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetWmbRouteDetails :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient RouteDetails,
    postDriverFleetGetNearbyDrivers :: Kernel.Prelude.Text -> NearbyDriverReq -> EulerHS.Types.EulerClient NearbyDriverResp,
    postDriverDashboardFleetTrackDriver :: Kernel.Prelude.Text -> TrackDriverLocationsReq -> EulerHS.Types.EulerClient TrackDriverLocationsRes
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverFleetAccessList :<|> postDriverFleetAccessSelect :<|> postDriverFleetAddVehicles :<|> postDriverFleetAddVehicle :<|> getDriverFleetGetDriverRequests :<|> postDriverFleetRespondDriverRequest :<|> postDriverFleetAddRCWithoutDriver :<|> getDriverFleetGetAllVehicle :<|> getDriverFleetGetAllDriver :<|> getDriverFleetGetAllBadge :<|> postDriverFleetUnlink :<|> postDriverFleetRemoveVehicle :<|> postDriverFleetRemoveDriver :<|> getDriverFleetTotalEarning :<|> getDriverFleetVehicleEarning :<|> getDriverFleetDriverEarning :<|> getDriverFleetDriverVehicleAssociation :<|> getDriverFleetDriverAssociation :<|> getDriverFleetVehicleAssociation :<|> postDriverFleetVehicleDriverRcStatus :<|> postDriverUpdateFleetOwnerInfo :<|> getDriverFleetOwnerInfo :<|> postDriverFleetSendJoiningOtp :<|> postDriverFleetVerifyJoiningOtp :<|> getDriverFleetRoutes :<|> getDriverFleetPossibleRoutes :<|> postDriverFleetTripPlanner :<|> getDriverFleetTripTransactions :<|> postDriverFleetAddDrivers :<|> postDriverFleetAddDriverBusRouteMapping :<|> postDriverFleetLinkRCWithDriver :<|> postDriverDashboardFleetWmbTripEnd :<|> getDriverFleetWmbRouteDetails :<|> postDriverFleetGetNearbyDrivers :<|> postDriverDashboardFleetTrackDriver = driverClient

data DriverUserActionType
  = GET_DRIVER_FLEET_ACCESS_LIST
  | POST_DRIVER_FLEET_ACCESS_SELECT
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
  | GET_DRIVER_FLEET_DRIVER_VEHICLE_ASSOCIATION
  | GET_DRIVER_FLEET_DRIVER_ASSOCIATION
  | GET_DRIVER_FLEET_VEHICLE_ASSOCIATION
  | POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS
  | POST_DRIVER_UPDATE_FLEET_OWNER_INFO
  | GET_DRIVER_FLEET_OWNER_INFO
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
  | GET_DRIVER_FLEET_WMB_ROUTE_DETAILS
  | POST_DRIVER_FLEET_GET_NEARBY_DRIVERS
  | POST_DRIVER_DASHBOARD_FLEET_TRACK_DRIVER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''DriverMode)

$(mkHttpInstancesForEnum ''FleetVehicleStatus)

$(mkHttpInstancesForEnum ''SortOn)

$(Data.Singletons.TH.genSingletons [''DriverUserActionType])
