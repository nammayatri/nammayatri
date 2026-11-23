module Lib.BmsBackend.Transit.V2.Types where

import Data.Aeson
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Data.Time (Day)
import Kernel.Prelude hiding (error)
import Kernel.Types.HideSecrets (HideSecrets (..))
import Lib.BmsBackend.Transit.Common (ListResponse, omitNothing)

data Verdict
  = VerdictCreated
  | VerdictCreatedPartial
  | VerdictExists
  | VerdictCovered
  | VerdictPast
  | VerdictOk
  | VerdictOffDay
  | VerdictOutOfWindow
  | VerdictFailed
  deriving (Generic, Show, Eq)

verdictOptions :: Options
verdictOptions = defaultOptions {constructorTagModifier = camelTo2 '_' . drop 7}

instance ToJSON Verdict where
  toJSON = genericToJSON verdictOptions

instance FromJSON Verdict where
  parseJSON = genericParseJSON verdictOptions

instance ToSchema Verdict where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions verdictOptions)

data CrewScope = CrewRun | CrewTrip
  deriving (Generic, Show, Eq)

crewScopeOptions :: Options
crewScopeOptions = defaultOptions {constructorTagModifier = camelTo2 '_' . drop 4}

instance ToJSON CrewScope where
  toJSON = genericToJSON crewScopeOptions

instance FromJSON CrewScope where
  parseJSON = genericParseJSON crewScopeOptions

instance ToSchema CrewScope where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions crewScopeOptions)

data TripGroup = TripGroup
  { id :: Text,
    gtfsId :: Text,
    organizationId :: Text,
    code :: Text,
    description :: Maybe Text,
    zone :: Text,
    shift :: Text,
    firstDeparture :: TimeOfDay,
    tripType :: Text,
    depotId :: Text,
    serviceTypeId :: Maybe Text,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    depotName :: Maybe Text,
    serviceTypeName :: Maybe Text,
    tripCount :: Int,
    repeatCount :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateTripGroupReq = CreateTripGroupReq
  { code :: Maybe Text,
    description :: Maybe Text,
    zone :: Text,
    shift :: Text,
    firstDeparture :: Text,
    tripType :: Text,
    depotId :: Text,
    serviceTypeId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateTripGroupReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateTripGroupReq where
  hideSecrets = identity

data UpdateTripGroupReq = UpdateTripGroupReq
  { code :: Maybe Text,
    description :: Maybe Text,
    zone :: Maybe Text,
    shift :: Maybe Text,
    firstDeparture :: Maybe Text,
    tripType :: Maybe Text,
    depotId :: Maybe Text,
    serviceTypeId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateTripGroupReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateTripGroupReq where
  hideSecrets = identity

data Trip = Trip
  { id :: Text,
    tripGroupId :: Text,
    gtfsId :: Text,
    organizationId :: Text,
    routeId :: Text,
    isBookable :: Bool,
    tripNumber :: Int,
    tripOrder :: Int,
    scheduledStartTime :: TimeOfDay,
    scheduledStartDayOffset :: Int,
    scheduledEndTime :: TimeOfDay,
    scheduledEndDayOffset :: Int,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data TripInput = TripInput
  { id :: Maybe Text,
    routeId :: Text,
    tripNumber :: Int,
    tripOrder :: Int,
    scheduledStartTime :: Text,
    scheduledEndTime :: Text,
    isBookable :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON TripInput where
  toJSON = genericToJSON omitNothing

data ReplaceTripsReq = ReplaceTripsReq
  { firstTripDayOffset :: Maybe Int,
    trips :: [TripInput]
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON ReplaceTripsReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets ReplaceTripsReq where
  hideSecrets = identity

data DutyRepeat = DutyRepeat
  { id :: Text,
    tripGroupId :: Text,
    gtfsId :: Text,
    organizationId :: Text,
    repeatStatus :: Text,
    recurrenceDays :: [Int],
    effectiveFrom :: Day,
    effectiveTill :: Maybe Day,
    generatedTill :: Maybe Day,
    vehicleNumber :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DutyRepeatView = DutyRepeatView
  { id :: Text,
    tripGroupId :: Text,
    gtfsId :: Text,
    organizationId :: Text,
    repeatStatus :: Text,
    recurrenceDays :: [Int],
    effectiveFrom :: Day,
    effectiveTill :: Maybe Day,
    generatedTill :: Maybe Day,
    vehicleNumber :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    tripGroupCode :: Text,
    depotId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data GenerateEntry = GenerateEntry
  { dutyRepeatId :: Text,
    operationDate :: Day,
    verdict :: Verdict,
    dutyGroupId :: Maybe Text,
    waybillNo :: Maybe Text,
    error :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpsertDutyRepeatResp = UpsertDutyRepeatResp
  { dutyRepeat :: DutyRepeat,
    generated :: [GenerateEntry]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateDutyRepeatReq = CreateDutyRepeatReq
  { tripGroupId :: Text,
    repeatStatus :: Maybe Text,
    recurrenceDays :: [Int],
    effectiveFrom :: Day,
    effectiveTill :: Maybe Day,
    vehicleNumber :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    generate :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateDutyRepeatReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateDutyRepeatReq where
  hideSecrets = identity

data UpdateDutyRepeatReq = UpdateDutyRepeatReq
  { repeatStatus :: Maybe Text,
    recurrenceDays :: Maybe [Int],
    effectiveFrom :: Maybe Day,
    effectiveTill :: Maybe Day,
    vehicleNumber :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    generate :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateDutyRepeatReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateDutyRepeatReq where
  hideSecrets = identity

data GenerateDutiesReq = GenerateDutiesReq
  { daysAhead :: Maybe Int,
    from :: Maybe Day,
    to :: Maybe Day,
    dutyRepeatIds :: Maybe [Text]
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON GenerateDutiesReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets GenerateDutiesReq where
  hideSecrets = identity

data DutyGroup = DutyGroup
  { id :: Text,
    waybillNo :: Text,
    tripGroupId :: Text,
    dutyRepeatId :: Maybe Text,
    gtfsId :: Text,
    organizationId :: Text,
    operationDate :: Day,
    depotId :: Text,
    vehicleNumber :: Maybe Text,
    serviceTypeId :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    windowStartAt :: UTCTime,
    windowEndAt :: UTCTime,
    isActive :: Bool,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DutyGroupListItem = DutyGroupListItem
  { id :: Text,
    waybillNo :: Text,
    tripGroupId :: Text,
    dutyRepeatId :: Maybe Text,
    gtfsId :: Text,
    organizationId :: Text,
    operationDate :: Day,
    depotId :: Text,
    vehicleNumber :: Maybe Text,
    serviceTypeId :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    windowStartAt :: UTCTime,
    windowEndAt :: UTCTime,
    isActive :: Bool,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    tripGroupCode :: Text,
    depotName :: Maybe Text,
    serviceTypeName :: Maybe Text,
    totalTrips :: Int,
    pendingTrips :: Int,
    unassignedTrips :: Int,
    runningTripNumber :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data Duty = Duty
  { id :: Text,
    dutyGroupId :: Text,
    tripId :: Text,
    gtfsId :: Text,
    organizationId :: Text,
    routeId :: Text,
    isBookable :: Bool,
    tripNumber :: Int,
    tripOrder :: Int,
    scheduledStartAt :: UTCTime,
    scheduledEndAt :: UTCTime,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    recordedStartTime :: Maybe UTCTime,
    recordedEndTime :: Maybe UTCTime,
    recordedVehicleNumber :: Maybe Text,
    recordedServiceTypeId :: Maybe Text,
    runActive :: Bool,
    status :: Text,
    cancelReason :: Maybe Text,
    skipReason :: Maybe Text,
    statusChangedBy :: Maybe Text,
    statusChangedAt :: Maybe UTCTime,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    dutyTripId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DutyGroupDetail = DutyGroupDetail
  { dutyGroup :: DutyGroup,
    tripGroupCode :: Text,
    depotName :: Maybe Text,
    serviceTypeName :: Maybe Text,
    duties :: [Duty]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateDutyGroupReq = CreateDutyGroupReq
  { tripGroupId :: Text,
    operationDate :: Day,
    vehicleNumber :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateDutyGroupReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateDutyGroupReq where
  hideSecrets = identity

data UpdateVehicleReq = UpdateVehicleReq
  { vehicleNumber :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateVehicleReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateVehicleReq where
  hideSecrets = identity

data VehicleChange = VehicleChange
  { changed :: Bool,
    oldVehicleNumber :: Maybe Text,
    newVehicleNumber :: Maybe Text,
    oldServiceTypeId :: Maybe Text,
    newServiceTypeId :: Maybe Text,
    dutyGroup :: DutyGroup
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateCrewReq = UpdateCrewReq
  { driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateCrewReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateCrewReq where
  hideSecrets = identity

data SetActiveReq = SetActiveReq
  { isActive :: Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON SetActiveReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets SetActiveReq where
  hideSecrets = identity

data EventValue = EventValue
  { scope :: Maybe CrewScope,
    tripNumber :: Maybe Int,
    vehicleNumber :: Maybe Text,
    serviceTypeId :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    tripsUpdated :: Maybe Int,
    status :: Maybe Text,
    isActive :: Maybe Bool,
    reason :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON EventValue where
  toJSON = genericToJSON omitNothing

data DutyEventLog = DutyEventLog
  { id :: Text,
    gtfsId :: Text,
    organizationId :: Text,
    eventType :: Text,
    dutyGroupId :: Maybe Text,
    dutyId :: Maybe Text,
    dutyRepeatId :: Maybe Text,
    operationDate :: Maybe Day,
    actorPersonId :: Maybe Text,
    trigger :: Maybe Text,
    oldValue :: Maybe EventValue,
    newValue :: Maybe EventValue,
    reason :: Maybe Text,
    errorCode :: Maybe Text,
    errorMessage :: Maybe Text,
    resolvedAt :: Maybe UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type DutyEventLogList = ListResponse DutyEventLog

type DutyGroupListItemList = ListResponse DutyGroupListItem

type DutyRepeatViewList = ListResponse DutyRepeatView

type TripGroupList = ListResponse TripGroup
