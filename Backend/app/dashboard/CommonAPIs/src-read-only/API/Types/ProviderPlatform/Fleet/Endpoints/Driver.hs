{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.Driver where

import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

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
    vehicleModelYear :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AddVehicleReq where
  hideSecrets = Kernel.Prelude.identity

newtype CreateVehiclesReq = CreateVehiclesReq {file :: Kernel.Prelude.FilePath}
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
    isDriverActive :: Kernel.Prelude.Bool,
    isRcAssociated :: Kernel.Prelude.Bool,
    verificationDocsStatus :: Kernel.Prelude.Maybe VerificationDocsStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverMode
  = ONLINE
  | OFFLINE
  | SILENT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data DrivertoVehicleAssociationRes = DrivertoVehicleAssociationRes {fleetOwnerId :: Kernel.Prelude.Text, listItem :: [DriveVehicleAssociationListItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetDriversAPIEntity = FleetDriversAPIEntity
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text
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

data FleetOwnerInfoRes = FleetOwnerInfoRes
  { blocked :: Kernel.Prelude.Bool,
    enabled :: Kernel.Prelude.Bool,
    fleetType :: Kernel.Prelude.Text,
    verified :: Kernel.Prelude.Bool,
    gstNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetOwnerInfoRes where
  hideSecrets = Kernel.Prelude.identity

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
  | Pending
  | Invalid
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

data RoundTripDetail = RoundTripDetail {frequency :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SortOn
  = COMPLETED_RIDES
  | CANCELLED_RIDES
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data TotalDuration = TotalDuration {hours :: Kernel.Prelude.Int, minutes :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripDetails = TripDetails {routeCode :: Kernel.Prelude.Text, roundTrip :: Kernel.Prelude.Maybe RoundTripDetail}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripPlannerReq = TripPlannerReq {driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver, vehicleNumber :: Kernel.Prelude.Text, trips :: [TripDetails]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TripPlannerReq where
  hideSecrets = Kernel.Prelude.identity

data TripStatus
  = TRIP_ASSIGNED
  | IN_PROGRESS
  | PAUSED
  | COMPLETED
  | CANCELLED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripTransactionDetail = TripTransactionDetail
  { routeCode :: Kernel.Prelude.Text,
    tripSequence :: Kernel.Prelude.Int,
    tripStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripStatus :: TripStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TripTransactionResp = TripTransactionResp {trips :: [TripTransactionDetail], totalTrips :: Kernel.Prelude.Int}
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
    registrationNo :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerificationDocsStatus = VerificationDocsStatus
  { vehicleRegistrationCertificate :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehiclePermit :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehicleFitness :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
    vehicleInsurance :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus,
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

type API = ("driver" :> (PostDriverFleetAddVehiclesHelper :<|> PostDriverFleetAddVehicleHelper :<|> PostDriverFleetAddRCWithoutDriverHelper :<|> GetDriverFleetGetAllVehicleHelper :<|> GetDriverFleetGetAllDriverHelper :<|> PostDriverFleetUnlinkHelper :<|> PostDriverFleetRemoveVehicleHelper :<|> PostDriverFleetRemoveDriverHelper :<|> GetDriverFleetTotalEarningHelper :<|> GetDriverFleetVehicleEarningHelper :<|> GetDriverFleetDriverEarningHelper :<|> GetDriverFleetGetFleetDriverVehicleAssociationHelper :<|> GetDriverFleetGetFleetDriverAssociationHelper :<|> GetDriverFleetGetFleetVehicleAssociationHelper :<|> PostDriverFleetVehicleDriverRCstatusHelper :<|> PostDriverUpdateFleetOwnerInfo :<|> GetDriverFleetOwnerInfo :<|> PostDriverFleetDriverSendJoiningOtpHelper :<|> PostDriverFleetDriverVerifyJoiningOtpHelper :<|> PostDriverFleetLinkRCWithDriverHelper :<|> PutDriverDashboardFleetWmbTripDeleteHelper))

type PostDriverFleetAddVehicles = ("fleet" :> "addVehicles" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp CreateVehiclesReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverFleetAddVehiclesHelper =
  ( "fleet" :> "addVehicles" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] CreateVehiclesReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddVehicle =
  ( Capture "mobileNo" Kernel.Prelude.Text :> "fleet" :> "addVehicle" :> QueryParam "countryCode" Kernel.Prelude.Text :> ReqBody '[JSON] AddVehicleReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddVehicleHelper =
  ( Capture "mobileNo" Kernel.Prelude.Text :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "addVehicle"
      :> QueryParam
           "mobileCountryCode"
           Kernel.Prelude.Text
      :> ReqBody '[JSON] AddVehicleReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddRCWithoutDriver =
  ( "fleet" :> "addRC" :> "withoutDriver" :> ReqBody '[JSON] Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddRCWithoutDriverHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "addRC" :> "withoutDriver"
      :> ReqBody
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetGetAllVehicle = ("fleet" :> "getAllVehicle" :> QueryParam "mblimit" Kernel.Prelude.Int :> QueryParam "mboffset" Kernel.Prelude.Int :> Get '[JSON] ListVehicleRes)

type GetDriverFleetGetAllVehicleHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "getAllVehicle" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get '[JSON] ListVehicleRes
  )

type GetDriverFleetGetAllDriver = ("fleet" :> "getAllDriver" :> QueryParam "mblimit" Kernel.Prelude.Int :> QueryParam "mboffset" Kernel.Prelude.Int :> Get '[JSON] FleetListDriverRes)

type GetDriverFleetGetAllDriverHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "getAllDriver" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get '[JSON] FleetListDriverRes
  )

type PostDriverFleetUnlink =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "unlink"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
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

type PostDriverFleetRemoveVehicle = (Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "remove" :> "vehicle" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverFleetRemoveVehicleHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "remove" :> "vehicle"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveDriver = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "fleet" :> "remove" :> "driver" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

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
      :> Get
           '[JSON]
           DrivertoVehicleAssociationRes
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
      :> Get
           '[JSON]
           DrivertoVehicleAssociationRes
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
      :> Get
           '[JSON]
           DrivertoVehicleAssociationRes
  )

type PostDriverFleetVehicleDriverRcStatus =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "fleet" :> "vehicleDriverRCstatus" :> ReqBody '[JSON] RCStatusReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
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
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateFleetOwnerInfo" :> ReqBody '[JSON] UpdateFleetOwnerInfoReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetOwnerInfo = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "fleetOwnerInfo" :> Get '[JSON] FleetOwnerInfoRes)

type PostDriverFleetSendJoiningOtp =
  ( "fleet" :> "sendJoiningOtp" :> ReqBody '[JSON] Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
  )

type PostDriverFleetDriverSendJoiningOtpHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "driver" :> "sendJoiningOtp"
      :> ReqBody
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post '[JSON] Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
  )

type PostDriverFleetVerifyJoiningOtp =
  ( "fleet" :> "verifyJoiningOtp" :> QueryParam "authId" Kernel.Prelude.Text :> ReqBody '[JSON] VerifyFleetJoiningOtpReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetDriverVerifyJoiningOtpHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "driver" :> "verifyJoiningOtp"
      :> QueryParam
           "authId"
           Kernel.Prelude.Text
      :> ReqBody '[JSON] VerifyFleetJoiningOtpReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetLinkRCWithDriver = ("fleet" :> "linkRCWithDriver" :> ReqBody '[JSON] LinkRCWithDriverForFleetReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverFleetLinkRCWithDriverHelper =
  ( "fleet" :> "linkRCWithDriver" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] LinkRCWithDriverForFleetReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PutDriverDashboardFleetWmbTripDelete =
  ( "dashboard" :> "fleet" :> "wmb" :> "trip" :> Capture "tripTransactionId" (Kernel.Types.Id.Id Dashboard.Common.TripTransaction) :> "delete"
      :> Put
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PutDriverDashboardFleetWmbTripDeleteHelper =
  ( "dashboard" :> "fleet" :> "wmb" :> "trip"
      :> Capture
           "tripTransactionId"
           (Kernel.Types.Id.Id Dashboard.Common.TripTransaction)
      :> Capture "fleetOwnerId" Kernel.Prelude.Text
      :> "delete"
      :> Put '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data DriverAPIs = DriverAPIs
  { postDriverFleetAddVehicles :: Kernel.Prelude.Text -> CreateVehiclesReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetAddVehicle :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> AddVehicleReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetAddRCWithoutDriver :: Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetGetAllVehicle :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient ListVehicleRes,
    getDriverFleetGetAllDriver :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient FleetListDriverRes,
    postDriverFleetUnlink :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetRemoveVehicle :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetRemoveDriver :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetTotalEarning :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient FleetTotalEarningResponse,
    getDriverFleetVehicleEarning :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient FleetEarningListRes,
    getDriverFleetDriverEarning :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe SortOn -> EulerHS.Types.EulerClient FleetEarningListRes,
    getDriverFleetDriverVehicleAssociation :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient DrivertoVehicleAssociationRes,
    getDriverFleetDriverAssociation :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe DriverMode -> EulerHS.Types.EulerClient DrivertoVehicleAssociationRes,
    getDriverFleetVehicleAssociation :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe FleetVehicleStatus -> EulerHS.Types.EulerClient DrivertoVehicleAssociationRes,
    postDriverFleetVehicleDriverRcStatus :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> RCStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateFleetOwnerInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateFleetOwnerInfoReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverFleetOwnerInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient FleetOwnerInfoRes,
    postDriverFleetSendJoiningOtp :: Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> EulerHS.Types.EulerClient Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes,
    postDriverFleetVerifyJoiningOtp :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> VerifyFleetJoiningOtpReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverFleetLinkRCWithDriver :: Kernel.Prelude.Text -> LinkRCWithDriverForFleetReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    putDriverDashboardFleetWmbTripDelete :: Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    postDriverFleetAddVehicles :<|> postDriverFleetAddVehicle :<|> postDriverFleetAddRCWithoutDriver :<|> getDriverFleetGetAllVehicle :<|> getDriverFleetGetAllDriver :<|> postDriverFleetUnlink :<|> postDriverFleetRemoveVehicle :<|> postDriverFleetRemoveDriver :<|> getDriverFleetTotalEarning :<|> getDriverFleetVehicleEarning :<|> getDriverFleetDriverEarning :<|> getDriverFleetDriverVehicleAssociation :<|> getDriverFleetDriverAssociation :<|> getDriverFleetVehicleAssociation :<|> postDriverFleetVehicleDriverRcStatus :<|> postDriverUpdateFleetOwnerInfo :<|> getDriverFleetOwnerInfo :<|> postDriverFleetSendJoiningOtp :<|> postDriverFleetVerifyJoiningOtp :<|> postDriverFleetLinkRCWithDriver :<|> putDriverDashboardFleetWmbTripDelete = driverClient

data DriverUserActionType
  = POST_DRIVER_FLEET_ADD_VEHICLES
  | POST_DRIVER_FLEET_ADD_VEHICLE
  | POST_DRIVER_FLEET_ADD_RC_WITHOUT_DRIVER
  | GET_DRIVER_FLEET_GET_ALL_VEHICLE
  | GET_DRIVER_FLEET_GET_ALL_DRIVER
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
  | POST_DRIVER_FLEET_LINK_RC_WITH_DRIVER
  | PUT_DRIVER_DASHBOARD_FLEET_WMB_TRIP_DELETE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''DriverMode)

$(mkHttpInstancesForEnum ''FleetVehicleStatus)

$(mkHttpInstancesForEnum ''SortOn)

$(Data.Singletons.TH.genSingletons [''DriverUserActionType])
