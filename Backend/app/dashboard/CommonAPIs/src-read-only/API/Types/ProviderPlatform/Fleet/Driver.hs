{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Driver where

import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import Data.OpenApi (ToSchema)
import qualified Data.Time
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
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

data DriveVehicleAssociationListItem = DriveVehicleAssociationListItem
  { driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.DriverMode,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    completedRides :: Kernel.Prelude.Int,
    vehicleType :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    earning :: Kernel.Prelude.Int,
    isDriverActive :: Kernel.Prelude.Bool,
    isRcAssociated :: Kernel.Prelude.Bool,
    verificationDocsStatus :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.VerificationDocsStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverMode
  = ONLINE
  | OFFLINE
  | SILENT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data DrivertoVehicleAssociationRes = DrivertoVehicleAssociationRes {fleetOwnerId :: Kernel.Prelude.Text, listItem :: [API.Types.ProviderPlatform.Fleet.Driver.DriveVehicleAssociationListItem], summary :: Dashboard.Common.Summary}
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

data FleetEarningListRes = FleetEarningListRes {fleetEarningRes :: [API.Types.ProviderPlatform.Fleet.Driver.FleetEarningRes], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetEarningRes = FleetEarningRes
  { totalRides :: Kernel.Prelude.Int,
    totalEarning :: Kernel.Prelude.Int,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver),
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.DriverMode,
    vehicleType :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    totalDuration :: API.Types.ProviderPlatform.Fleet.Driver.TotalDuration,
    distanceTravelled :: Kernel.Prelude.Double,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cancelledRides :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetListDriverRes = FleetListDriverRes {fleetDriversInfos :: [API.Types.ProviderPlatform.Fleet.Driver.FleetDriversAPIEntity]}
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

newtype ListVehicleRes = ListVehicleRes {vehicles :: [API.Types.ProviderPlatform.Fleet.Driver.VehicleAPIEntity]}
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

data SortOn
  = COMPLETED_RIDES
  | CANCELLED_RIDES
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data TotalDuration = TotalDuration {hours :: Kernel.Prelude.Int, minutes :: Kernel.Prelude.Int}
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

type API = ("driver" :> (PostDriverFleetAddVehicleHelper :<|> PostDriverFleetAddRCWithoutDriverHelper :<|> GetDriverFleetGetAllVehicleHelper :<|> GetDriverFleetGetAllDriverHelper :<|> PostDriverFleetUnlinkHelper :<|> PostDriverFleetRemoveVehicleHelper :<|> PostDriverFleetRemoveDriverHelper :<|> GetDriverFleetTotalEarningHelper :<|> GetDriverFleetVehicleEarningHelper :<|> GetDriverFleetDriverEarningHelper :<|> GetDriverFleetGetFleetDriverVehicleAssociationHelper :<|> GetDriverFleetGetFleetDriverAssociationHelper :<|> GetDriverFleetGetFleetVehicleAssociationHelper :<|> PostDriverFleetVehicleDriverRCstatusHelper :<|> PostDriverUpdateFleetOwnerInfo :<|> GetDriverFleetOwnerInfo :<|> PostDriverFleetDriverSendJoiningOtpHelper :<|> PostDriverFleetDriverVerifyJoiningOtpHelper :<|> PostDriverFleetLinkRCWithDriverHelper))

type PostDriverFleetAddVehicle =
  ( Capture "mobileNo" Kernel.Prelude.Text :> "fleet" :> "addVehicle" :> QueryParam "countryCode" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddVehicleHelper =
  ( Capture "mobileNo" Kernel.Prelude.Text :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "addVehicle"
      :> QueryParam
           "mobileCountryCode"
           Kernel.Prelude.Text
      :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddRCWithoutDriver =
  ( "fleet" :> "addRC" :> "withoutDriver" :> ReqBody ('[JSON]) Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetAddRCWithoutDriverHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "addRC" :> "withoutDriver"
      :> ReqBody
           ('[JSON])
           Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetGetAllVehicle =
  ( "fleet" :> "getAllVehicle" :> QueryParam "mblimit" Kernel.Prelude.Int :> QueryParam "mboffset" Kernel.Prelude.Int
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.ListVehicleRes
  )

type GetDriverFleetGetAllVehicleHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "getAllVehicle" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get ('[JSON]) API.Types.ProviderPlatform.Fleet.Driver.ListVehicleRes
  )

type GetDriverFleetGetAllDriver =
  ( "fleet" :> "getAllDriver" :> QueryParam "mblimit" Kernel.Prelude.Int :> QueryParam "mboffset" Kernel.Prelude.Int
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.FleetListDriverRes
  )

type GetDriverFleetGetAllDriverHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "getAllDriver" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get ('[JSON]) API.Types.ProviderPlatform.Fleet.Driver.FleetListDriverRes
  )

type PostDriverFleetUnlink =
  ( Capture "driverId" ((Kernel.Types.Id.Id Dashboard.Common.Driver)) :> Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "unlink"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetUnlinkHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> Capture "driverId" ((Kernel.Types.Id.Id Dashboard.Common.Driver))
      :> Capture
           "vehicleNo"
           Kernel.Prelude.Text
      :> "fleet"
      :> "unlink"
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveVehicle = (Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "remove" :> "vehicle" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostDriverFleetRemoveVehicleHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> Capture "vehicleNo" Kernel.Prelude.Text :> "fleet" :> "remove" :> "vehicle"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetRemoveDriver = (Capture "driverId" ((Kernel.Types.Id.Id Dashboard.Common.Driver)) :> "fleet" :> "remove" :> "driver" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostDriverFleetRemoveDriverHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text
      :> Capture
           "driverId"
           ((Kernel.Types.Id.Id Dashboard.Common.Driver))
      :> "fleet"
      :> "remove"
      :> "driver"
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetTotalEarning =
  ( "fleet" :> "totalEarning" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.FleetTotalEarningResponse
  )

type GetDriverFleetTotalEarningHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "totalEarning" :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get ('[JSON]) API.Types.ProviderPlatform.Fleet.Driver.FleetTotalEarningResponse
  )

type GetDriverFleetVehicleEarning =
  ( "fleet" :> "vehicleEarning" :> QueryParam "vehicleNo" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes
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
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes
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
           API.Types.ProviderPlatform.Fleet.Driver.SortOn
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes
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
           API.Types.ProviderPlatform.Fleet.Driver.SortOn
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes
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
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes
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
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes
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
           API.Types.ProviderPlatform.Fleet.Driver.DriverMode
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes
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
           API.Types.ProviderPlatform.Fleet.Driver.DriverMode
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes
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
           API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatus
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes
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
           API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatus
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes
  )

type PostDriverFleetVehicleDriverRCstatus =
  ( Capture "driverId" ((Kernel.Types.Id.Id Dashboard.Common.Driver)) :> "fleet" :> "vehicleDriverRCstatus"
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetVehicleDriverRCstatusHelper =
  ( Capture "driverId" ((Kernel.Types.Id.Id Dashboard.Common.Driver))
      :> Capture
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> "fleet"
      :> "vehicleDriverRCstatus"
      :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateFleetOwnerInfo =
  ( Capture "driverId" ((Kernel.Types.Id.Id Dashboard.Common.Driver)) :> "updateFleetOwnerInfo"
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.UpdateFleetOwnerInfoReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type GetDriverFleetOwnerInfo = (Capture "driverId" ((Kernel.Types.Id.Id Dashboard.Common.Driver)) :> "fleetOwnerInfo" :> Get ('[JSON]) API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerInfoRes)

type PostDriverFleetSendJoiningOtp =
  ( "fleet" :> "sendJoiningOtp" :> ReqBody ('[JSON]) Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post
           ('[JSON])
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
  )

type PostDriverFleetDriverSendJoiningOtpHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "driver" :> "sendJoiningOtp"
      :> ReqBody
           ('[JSON])
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post ('[JSON]) Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
  )

type PostDriverFleetVerifyJoiningOtp =
  ( "fleet" :> "verifyJoiningOtp" :> QueryParam "authId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.VerifyFleetJoiningOtpReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetDriverVerifyJoiningOtpHelper =
  ( Capture "fleetOwnerId" Kernel.Prelude.Text :> "fleet" :> "driver" :> "verifyJoiningOtp"
      :> QueryParam
           "authId"
           Kernel.Prelude.Text
      :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Fleet.Driver.VerifyFleetJoiningOtpReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetLinkRCWithDriver =
  ( "fleet" :> "linkRCWithDriver" :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Fleet.Driver.LinkRCWithDriverForFleetReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverFleetLinkRCWithDriverHelper =
  ( "fleet" :> "linkRCWithDriver" :> Capture "fleetOwnerId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Fleet.Driver.LinkRCWithDriverForFleetReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

data DriverAPIs = DriverAPIs
  { postDriverFleetAddVehicle :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postDriverFleetAddRCWithoutDriver :: (Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getDriverFleetGetAllVehicle :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Driver.ListVehicleRes),
    getDriverFleetGetAllDriver :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Driver.FleetListDriverRes),
    postDriverFleetUnlink :: (Kernel.Prelude.Text -> (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postDriverFleetRemoveVehicle :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postDriverFleetRemoveDriver :: (Kernel.Prelude.Text -> (Kernel.Types.Id.Id Dashboard.Common.Driver) -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getDriverFleetTotalEarning :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Driver.FleetTotalEarningResponse),
    getDriverFleetVehicleEarning :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes),
    getDriverFleetDriverEarning :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.SortOn) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes),
    getDriverFleetDriverVehicleAssociation :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes),
    getDriverFleetDriverAssociation :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.DriverMode) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes),
    getDriverFleetVehicleAssociation :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatus) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes),
    postDriverFleetVehicleDriverRCstatus :: ((Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postDriverUpdateFleetOwnerInfo :: ((Kernel.Types.Id.Id Dashboard.Common.Driver) -> API.Types.ProviderPlatform.Fleet.Driver.UpdateFleetOwnerInfoReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getDriverFleetOwnerInfo :: ((Kernel.Types.Id.Id Dashboard.Common.Driver) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerInfoRes),
    postDriverFleetSendJoiningOtp :: (Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> EulerHS.Types.EulerClient Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes),
    postDriverFleetVerifyJoiningOtp :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.VerifyFleetJoiningOtpReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postDriverFleetLinkRCWithDriver :: (Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.LinkRCWithDriverForFleetReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    postDriverFleetAddVehicle :<|> postDriverFleetAddRCWithoutDriver :<|> getDriverFleetGetAllVehicle :<|> getDriverFleetGetAllDriver :<|> postDriverFleetUnlink :<|> postDriverFleetRemoveVehicle :<|> postDriverFleetRemoveDriver :<|> getDriverFleetTotalEarning :<|> getDriverFleetVehicleEarning :<|> getDriverFleetDriverEarning :<|> getDriverFleetDriverVehicleAssociation :<|> getDriverFleetDriverAssociation :<|> getDriverFleetVehicleAssociation :<|> postDriverFleetVehicleDriverRCstatus :<|> postDriverUpdateFleetOwnerInfo :<|> getDriverFleetOwnerInfo :<|> postDriverFleetSendJoiningOtp :<|> postDriverFleetVerifyJoiningOtp :<|> postDriverFleetLinkRCWithDriver = driverClient
