module Lib.BmsBackend.Transit.Masters.Types where

import Data.Aeson
import Data.Char (toLower)
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Kernel.Prelude hiding (sequence)
import Kernel.Types.HideSecrets (HideSecrets (..))
import Lib.BmsBackend.Transit.Common (ListResponse, omitNothing)
import Servant

renameTypeField :: String -> String
renameTypeField = \case
  "deviceType" -> "type"
  other -> other

typeFieldOptions :: Options
typeFieldOptions = defaultOptions {fieldLabelModifier = renameTypeField}

typeFieldOmitNothing :: Options
typeFieldOmitNothing = omitNothing {fieldLabelModifier = renameTypeField}

lowerConstructorOptions :: Options
lowerConstructorOptions = defaultOptions {constructorTagModifier = fmap toLower}

data Organization = Organization
  { id :: Text,
    code :: Maybe Text,
    name :: Text,
    shortName :: Maybe Text,
    nameLocal :: Maybe Text,
    shortNameLocal :: Maybe Text,
    status :: Text,
    remark :: Maybe Text,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateOrganizationReq = CreateOrganizationReq
  { code :: Text,
    name :: Text,
    shortName :: Maybe Text,
    nameLocal :: Maybe Text,
    shortNameLocal :: Maybe Text,
    status :: Maybe Text,
    remark :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateOrganizationReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateOrganizationReq where
  hideSecrets = identity

data UpdateOrganizationReq = UpdateOrganizationReq
  { code :: Maybe Text,
    name :: Maybe Text,
    shortName :: Maybe Text,
    nameLocal :: Maybe Text,
    shortNameLocal :: Maybe Text,
    status :: Maybe Text,
    remark :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateOrganizationReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateOrganizationReq where
  hideSecrets = identity

data Depot = Depot
  { id :: Text,
    name :: Text,
    entityName :: Text,
    nameLocal :: Maybe Text,
    remark :: Maybe Text,
    organizationId :: Maybe Text,
    organizationName :: Maybe Text,
    status :: Text,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateDepotReq = CreateDepotReq
  { name :: Text,
    nameLocal :: Maybe Text,
    remark :: Maybe Text,
    organizationId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateDepotReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateDepotReq where
  hideSecrets = identity

data UpdateDepotReq = UpdateDepotReq
  { name :: Maybe Text,
    nameLocal :: Maybe Text,
    remark :: Maybe Text,
    organizationId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateDepotReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateDepotReq where
  hideSecrets = identity

data Vehicle = Vehicle
  { id :: Text,
    vehicleNo :: Maybe Text,
    fleetNo :: Maybe Text,
    tagNumber :: Maybe Text,
    status :: Maybe Text,
    depotId :: Maybe Text,
    depotName :: Maybe Text,
    serviceTypeId :: Maybe Text,
    serviceTypeName :: Maybe Text,
    organizationId :: Maybe Text,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateVehicleReq = CreateVehicleReq
  { vehicleNo :: Text,
    fleetNo :: Maybe Text,
    tagNumber :: Maybe Text,
    status :: Maybe Text,
    depotId :: Maybe Text,
    serviceTypeId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateVehicleReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateVehicleReq where
  hideSecrets = identity

data UpdateVehicleReq = UpdateVehicleReq
  { vehicleNo :: Maybe Text,
    fleetNo :: Maybe Text,
    tagNumber :: Maybe Text,
    status :: Maybe Text,
    depotId :: Maybe Text,
    serviceTypeId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateVehicleReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateVehicleReq where
  hideSecrets = identity

data BulkVehiclesReq = BulkVehiclesReq
  { depotId :: Maybe Text,
    vehicles :: [CreateVehicleReq]
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON BulkVehiclesReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets BulkVehiclesReq where
  hideSecrets = identity

data DeviceType = ETM | OBU
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema, ToParamSchema)

instance ToHttpApiData DeviceType where
  toUrlPiece = \case
    ETM -> "ETM"
    OBU -> "OBU"

instance FromHttpApiData DeviceType where
  parseUrlPiece = \case
    "ETM" -> Right ETM
    "OBU" -> Right OBU
    other -> Left ("invalid device type: " <> other)

data Device = Device
  { id :: Text,
    deviceType :: Text,
    serial :: Text,
    fleetNo :: Maybe Text,
    vehicleId :: Maybe Text,
    vehicleNo :: Maybe Text,
    depotId :: Maybe Text,
    organizationId :: Text,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Device where
  parseJSON = genericParseJSON typeFieldOptions

instance ToJSON Device where
  toJSON = genericToJSON typeFieldOptions

instance ToSchema Device where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions typeFieldOptions)

data DeviceRow = DeviceRow
  { serial :: Text,
    fleetNo :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON DeviceRow where
  toJSON = genericToJSON omitNothing

data BulkDevicesReq = BulkDevicesReq
  { deviceType :: DeviceType,
    organizationId :: Maybe Text,
    devices :: [DeviceRow]
  }
  deriving (Generic, Show)

instance FromJSON BulkDevicesReq where
  parseJSON = genericParseJSON typeFieldOptions

instance ToJSON BulkDevicesReq where
  toJSON = genericToJSON typeFieldOmitNothing

instance ToSchema BulkDevicesReq where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions typeFieldOptions)

instance HideSecrets BulkDevicesReq where
  hideSecrets = identity

data Employee = Employee
  { id :: Text,
    tokenNo :: Maybe Text,
    firstName :: Text,
    lastName :: Maybe Text,
    name :: Text,
    mobileNo :: Maybe Text,
    gender :: Maybe Text,
    status :: Maybe Text,
    role :: Maybe Text,
    designationId :: Maybe Text,
    depotId :: Maybe Text,
    depotName :: Maybe Text,
    organizationId :: Maybe Text,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateEmployeeReq = CreateEmployeeReq
  { firstName :: Text,
    lastName :: Maybe Text,
    tokenNo :: Text,
    mobileNo :: Maybe Text,
    gender :: Maybe Text,
    status :: Maybe Text,
    role :: Maybe Text,
    depotId :: Maybe Text,
    passwordHash :: Maybe Text,
    emailHash :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateEmployeeReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateEmployeeReq where
  hideSecrets = identity

data UpdateEmployeeReq = UpdateEmployeeReq
  { firstName :: Maybe Text,
    lastName :: Maybe Text,
    tokenNo :: Maybe Text,
    mobileNo :: Maybe Text,
    gender :: Maybe Text,
    status :: Maybe Text,
    role :: Maybe Text,
    depotId :: Maybe Text,
    passwordHash :: Maybe Text,
    emailHash :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateEmployeeReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateEmployeeReq where
  hideSecrets = identity

data BulkEmployeesReq = BulkEmployeesReq
  { role :: Maybe Text,
    depotId :: Maybe Text,
    employees :: [CreateEmployeeReq]
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON BulkEmployeesReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets BulkEmployeesReq where
  hideSecrets = identity

data Designation = Designation
  { id :: Text,
    name :: Text,
    status :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CrewMember = CrewMember
  { id :: Text,
    tokenNo :: Maybe Text,
    name :: Text,
    mobileNo :: Maybe Text,
    depotId :: Maybe Text,
    role :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data AuthType = Email | EmployeeId
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data EmployeeRole = Driver | Conductor
  deriving (Generic, Show, Eq)

instance FromJSON EmployeeRole where
  parseJSON = genericParseJSON lowerConstructorOptions

instance ToJSON EmployeeRole where
  toJSON = genericToJSON lowerConstructorOptions

instance ToSchema EmployeeRole where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions lowerConstructorOptions)

data EmployeeLoginReq = EmployeeLoginReq
  { auth_type :: Maybe AuthType,
    email_hash :: Maybe Text,
    employee_id :: Maybe Text,
    password_hash :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON EmployeeLoginReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets EmployeeLoginReq where
  hideSecrets = identity

data EmployeeLoginResp = EmployeeLoginResp
  { verified :: Bool,
    token :: Maybe Text,
    role :: Maybe EmployeeRole
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data EmployeeVerifyReq = EmployeeVerifyReq
  { operator_badge_token :: Text,
    device_serial_number :: Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON EmployeeVerifyReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets EmployeeVerifyReq where
  hideSecrets = identity

newtype EmployeeVerifyResp = EmployeeVerifyResp
  { verified :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CodeName = CodeName
  { id :: Text,
    code :: Maybe Text,
    name :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DepotOption = DepotOption
  { id :: Text,
    name :: Text,
    organizationId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data Lookups = Lookups
  { shifts :: [Text],
    tripTypes :: [Text],
    legacyShiftTypes :: [Text],
    legacyTripTypes :: [Text],
    dayTypes :: [Text],
    breakTypes :: [Text],
    reasons :: [Text],
    dutyStatuses :: [Text],
    waybillStatuses :: [Text],
    stopTypes :: [Text],
    scheduleTypes :: [CodeName],
    serviceTypes :: [CodeName],
    depots :: [DepotOption],
    timezone :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ServiceType = ServiceType
  { id :: Text,
    name :: Maybe Text,
    code :: Maybe Text,
    abbreviation :: Maybe Text,
    status :: Maybe Text,
    ticketFooter :: Maybe Text,
    ticketFooterLocal :: Maybe Text,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateServiceTypeReq = CreateServiceTypeReq
  { name :: Text,
    code :: Maybe Text,
    abbreviation :: Maybe Text,
    status :: Maybe Text,
    ticketFooter :: Maybe Text,
    ticketFooterLocal :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateServiceTypeReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateServiceTypeReq where
  hideSecrets = identity

data UpdateServiceTypeReq = UpdateServiceTypeReq
  { name :: Maybe Text,
    code :: Maybe Text,
    abbreviation :: Maybe Text,
    status :: Maybe Text,
    ticketFooter :: Maybe Text,
    ticketFooterLocal :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateServiceTypeReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateServiceTypeReq where
  hideSecrets = identity

data StationEta = StationEta
  { sourceStationCode :: Text,
    destinationStationCode :: Text,
    etaInSeconds :: Maybe Int,
    updatedAt :: Maybe LocalTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data StationEtaRow = StationEtaRow
  { sourceStationCode :: Text,
    destinationStationCode :: Text,
    etaInSeconds :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON StationEtaRow where
  toJSON = genericToJSON omitNothing

newtype PutStationEtasReq = PutStationEtasReq
  { etas :: [StationEtaRow]
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON PutStationEtasReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets PutStationEtasReq where
  hideSecrets = identity

newtype StationEtaUpsertResult = StationEtaUpsertResult
  { upserted :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype AssignPersonOrganizationReq = AssignPersonOrganizationReq
  { organizationId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON AssignPersonOrganizationReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets AssignPersonOrganizationReq where
  hideSecrets = identity

type CrewMemberList = ListResponse CrewMember

type DepotList = ListResponse Depot

type DeviceList = ListResponse Device

type EmployeeList = ListResponse Employee

type OrganizationList = ListResponse Organization

type ServiceTypeList = ListResponse ServiceType

type StationEtaList = ListResponse StationEta

type VehicleList = ListResponse Vehicle
