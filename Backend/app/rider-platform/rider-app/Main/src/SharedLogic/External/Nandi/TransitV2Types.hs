{-# LANGUAGE DuplicateRecordFields #-}

-- | Typed transitV2 operator models (GIMS @/internal/operator/{gtfs_id}/v2/...@), used as the
-- request / response types of the TransitOperator v2 dashboard APIs. JSON is exactly what GIMS
-- speaks (camelCase fields; enums as GIMS strings). Plan: scripts/plans/gims/transitV2.
module SharedLogic.External.Nandi.TransitV2Types where

import Data.Aeson
import Data.Char (toLower, toUpper)
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Data.Time (Day)
import Kernel.Prelude hiding (error)
import Kernel.Types.HideSecrets (HideSecrets (..))

-- ─── Enums ─────────────────────────────────────────────────────────────────

-- | Constructor tag = constructor name minus @prefix@, then @render@ (e.g. ShiftAllDay -> ALLDAY).
enumOptions :: String -> (String -> String) -> Options
enumOptions prefix render = defaultOptions {constructorTagModifier = render . drop (length prefix), allNullaryToStringTag = True}

upper, snakeLower, snakeUpper :: String -> String
upper = map toUpper
snakeLower = camelTo2 '_'
snakeUpper = map toUpper . camelTo2 '_'

data V2Shift = ShiftMorning | ShiftAfternoon | ShiftEvening | ShiftNight | ShiftMidnight | ShiftAllDay
  deriving (Show, Eq, Ord, Generic)

shiftOptions :: Options
shiftOptions = enumOptions "Shift" upper

instance ToJSON V2Shift where toJSON = genericToJSON shiftOptions

instance FromJSON V2Shift where parseJSON = genericParseJSON shiftOptions

instance ToSchema V2Shift where declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions shiftOptions)

data V2GroupTripType = TripTypeNormal | TripTypeShort | TripTypeFeeder
  deriving (Show, Eq, Ord, Generic)

groupTripTypeOptions :: Options
groupTripTypeOptions = enumOptions "TripType" upper

instance ToJSON V2GroupTripType where toJSON = genericToJSON groupTripTypeOptions

instance FromJSON V2GroupTripType where parseJSON = genericParseJSON groupTripTypeOptions

instance ToSchema V2GroupTripType where declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions groupTripTypeOptions)

data V2RepeatStatus = RepeatActive | RepeatInactive
  deriving (Show, Eq, Ord, Generic)

repeatStatusOptions :: Options
repeatStatusOptions = enumOptions "Repeat" (map toLower)

instance ToJSON V2RepeatStatus where toJSON = genericToJSON repeatStatusOptions

instance FromJSON V2RepeatStatus where parseJSON = genericParseJSON repeatStatusOptions

instance ToSchema V2RepeatStatus where declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions repeatStatusOptions)

-- | A trip's (duty's) state.
data V2DutyStatus = DutyUpcoming | DutyActive | DutyCompleted | DutySkipped | DutyCancelled
  deriving (Show, Eq, Ord, Generic)

dutyStatusOptions :: Options
dutyStatusOptions = enumOptions "Duty" (map toLower)

instance ToJSON V2DutyStatus where toJSON = genericToJSON dutyStatusOptions

instance FromJSON V2DutyStatus where parseJSON = genericParseJSON dutyStatusOptions

instance ToSchema V2DutyStatus where declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions dutyStatusOptions)

-- | Why a trip was cancelled / skipped.
data V2TripReason = ReasonOperator | ReasonBreakdown | ReasonAdmin | ReasonDriver | ReasonOther
  deriving (Show, Eq, Ord, Generic)

tripReasonOptions :: Options
tripReasonOptions = enumOptions "Reason" upper

instance ToJSON V2TripReason where toJSON = genericToJSON tripReasonOptions

instance FromJSON V2TripReason where parseJSON = genericParseJSON tripReasonOptions

instance ToSchema V2TripReason where declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions tripReasonOptions)

data V2DutyEventType = EventVehicleChange | EventCrewChange | EventTripStatusChange | EventRunActiveChange | EventGenerationFailure
  deriving (Show, Eq, Ord, Generic)

dutyEventTypeOptions :: Options
dutyEventTypeOptions = enumOptions "Event" snakeUpper

instance ToJSON V2DutyEventType where toJSON = genericToJSON dutyEventTypeOptions

instance FromJSON V2DutyEventType where parseJSON = genericParseJSON dutyEventTypeOptions

instance ToSchema V2DutyEventType where declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions dutyEventTypeOptions)

-- | What caused a change / generation.
data V2Trigger = TriggerApi | TriggerCron | TriggerRunFinish | TriggerRuleSave | TriggerManualGenerate
  deriving (Show, Eq, Ord, Generic)

triggerOptions :: Options
triggerOptions = enumOptions "Trigger" snakeUpper

instance ToJSON V2Trigger where toJSON = genericToJSON triggerOptions

instance FromJSON V2Trigger where parseJSON = genericParseJSON triggerOptions

instance ToSchema V2Trigger where declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions triggerOptions)

-- | Outcome of generating one (repeat config, date).
data V2GenerateVerdict
  = VerdictCreated
  | VerdictCreatedPartial
  | VerdictExists
  | VerdictCovered
  | VerdictPast
  | VerdictOk
  | VerdictOffDay
  | VerdictOutOfWindow
  | VerdictFailed
  deriving (Show, Eq, Ord, Generic)

verdictOptions :: Options
verdictOptions = enumOptions "Verdict" snakeLower

instance ToJSON V2GenerateVerdict where toJSON = genericToJSON verdictOptions

instance FromJSON V2GenerateVerdict where parseJSON = genericParseJSON verdictOptions

instance ToSchema V2GenerateVerdict where declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions verdictOptions)

-- ─── Trip groups & trips ───────────────────────────────────────────────────

data V2TripGroup = V2TripGroup
  { id :: Text,
    gtfsId :: Text,
    operatorId :: Maybe Text,
    code :: Text,
    description :: Maybe Text,
    zone :: Text,
    shift :: V2Shift,
    -- | HH:MM:SS (IST)
    firstDeparture :: Text,
    tripType :: V2GroupTripType,
    depotId :: Maybe Text,
    serviceTypeId :: Maybe Text,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

-- | A trip group row in the list, with its trip and active repeat config counts.
data V2TripGroupListItem = V2TripGroupListItem
  { id :: Text,
    gtfsId :: Text,
    operatorId :: Maybe Text,
    code :: Text,
    description :: Maybe Text,
    zone :: Text,
    shift :: V2Shift,
    firstDeparture :: Text,
    tripType :: V2GroupTripType,
    depotId :: Maybe Text,
    serviceTypeId :: Maybe Text,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    tripCount :: Int,
    repeatCount :: Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data V2TripGroupPage = V2TripGroupPage {items :: [V2TripGroupListItem], total :: Int}
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data V2Trip = V2Trip
  { id :: Text,
    tripGroupId :: Text,
    gtfsId :: Text,
    operatorId :: Maybe Text,
    routeId :: Text,
    isBookable :: Bool,
    tripNumber :: Int,
    tripOrder :: Int,
    -- | HH:MM:SS (IST)
    scheduledStartTime :: Text,
    scheduledStartDayOffset :: Int,
    scheduledEndTime :: Text,
    scheduledEndDayOffset :: Int,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

-- | Create (no id) or edit a trip group. The code is rebuilt by GIMS from zone / shift / first
-- departure / trip type; a mismatching @code@ is rejected.
data V2UpsertTripGroupReq = V2UpsertTripGroupReq
  { id :: Maybe Text,
    code :: Maybe Text,
    description :: Maybe Text,
    zone :: Text,
    shift :: V2Shift,
    -- | HH:MM (IST); must equal the first trip's start
    firstDeparture :: Text,
    tripType :: V2GroupTripType,
    depotId :: Maybe Text,
    serviceTypeId :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets V2UpsertTripGroupReq where hideSecrets = identity

data V2TripInput = V2TripInput
  { id :: Maybe Text,
    routeId :: Text,
    tripNumber :: Int,
    tripOrder :: Int,
    -- | HH:MM (IST)
    scheduledStartTime :: Text,
    scheduledEndTime :: Text,
    isBookable :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

-- | Upserts some or all trips of a group; GIMS recomputes every trip's day offsets.
data V2UpsertTripsReq = V2UpsertTripsReq
  { firstTripDayOffset :: Maybe Int,
    trips :: [V2TripInput]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets V2UpsertTripsReq where hideSecrets = identity

-- ─── Repeat configs ────────────────────────────────────────────────────────

data V2DutyRepeat = V2DutyRepeat
  { id :: Text,
    tripGroupId :: Text,
    gtfsId :: Text,
    operatorId :: Maybe Text,
    repeatStatus :: V2RepeatStatus,
    -- | ISO weekdays, 1 = Mon .. 7 = Sun
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
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data V2DutyRepeatPage = V2DutyRepeatPage {items :: [V2DutyRepeat], total :: Int}
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data V2UpsertDutyRepeatReq = V2UpsertDutyRepeatReq
  { id :: Maybe Text,
    tripGroupId :: Text,
    repeatStatus :: Maybe V2RepeatStatus,
    recurrenceDays :: [Int],
    effectiveFrom :: Day,
    effectiveTill :: Maybe Day,
    vehicleNumber :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    -- | create duty groups for today .. today + 7 right after saving (GIMS default: true)
    generate :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets V2UpsertDutyRepeatReq where hideSecrets = identity

data V2GenerateEntry = V2GenerateEntry
  { dutyRepeatId :: Text,
    operationDate :: Day,
    verdict :: V2GenerateVerdict,
    dutyGroupId :: Maybe Text,
    waybillNo :: Maybe Text,
    error :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data V2UpsertDutyRepeatResp = V2UpsertDutyRepeatResp
  { dutyRepeat :: V2DutyRepeat,
    generated :: [V2GenerateEntry]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

-- | Either @daysAhead@ (from today IST) or @from@ .. @to@; optionally only some repeat configs.
data V2GenerateReq = V2GenerateReq
  { daysAhead :: Maybe Int,
    from :: Maybe Day,
    to :: Maybe Day,
    dutyRepeatIds :: Maybe [Text]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets V2GenerateReq where hideSecrets = identity

-- ─── Duty groups & duties ──────────────────────────────────────────────────

data V2DutyGroup = V2DutyGroup
  { id :: Text,
    waybillNo :: Text,
    tripGroupId :: Text,
    dutyRepeatId :: Maybe Text,
    gtfsId :: Text,
    operatorId :: Maybe Text,
    operationDate :: Day,
    depotId :: Maybe Text,
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
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

-- | A duty group row in the list, with trip progress.
data V2DutyGroupListItem = V2DutyGroupListItem
  { id :: Text,
    waybillNo :: Text,
    tripGroupId :: Text,
    dutyRepeatId :: Maybe Text,
    gtfsId :: Text,
    operatorId :: Maybe Text,
    operationDate :: Day,
    depotId :: Maybe Text,
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
    totalTrips :: Int,
    pendingTrips :: Int,
    unassignedTrips :: Int,
    runningTripNumber :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data V2DutyGroupPage = V2DutyGroupPage {items :: [V2DutyGroupListItem], total :: Int}
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

-- | One trip of a duty group.
data V2Duty = V2Duty
  { id :: Text,
    dutyGroupId :: Text,
    -- | the template trip (trips.id)
    tripId :: Text,
    gtfsId :: Text,
    operatorId :: Maybe Text,
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
    status :: V2DutyStatus,
    cancelReason :: Maybe V2TripReason,
    skipReason :: Maybe V2TripReason,
    statusChangedBy :: Maybe Text,
    statusChangedAt :: Maybe UTCTime,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    -- | @waybillNo-tripNumber@: the trip's id in rider / fleet APIs
    dutyTripId :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data V2DutyGroupDetail = V2DutyGroupDetail
  { dutyGroup :: V2DutyGroup,
    tripGroupCode :: Text,
    duties :: [V2Duty]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data V2CreateDutyGroupReq = V2CreateDutyGroupReq
  { tripGroupId :: Text,
    operationDate :: Day,
    vehicleNumber :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets V2CreateDutyGroupReq where hideSecrets = identity

-- | Bus of a duty group; @Nothing@ removes it.
newtype V2UpdateVehicleReq = V2UpdateVehicleReq {vehicleNumber :: Maybe Text}
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets V2UpdateVehicleReq where hideSecrets = identity

-- | Crew change: an absent field is unchanged, an empty string clears it.
data V2UpdateCrewReq = V2UpdateCrewReq
  { driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets V2UpdateCrewReq where hideSecrets = identity

newtype V2SetActiveReq = V2SetActiveReq {isActive :: Bool}
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets V2SetActiveReq where hideSecrets = identity

newtype V2SuccessResp = V2SuccessResp {success :: Bool}
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

-- ─── Event log ─────────────────────────────────────────────────────────────

-- | Before / after of a logged change; which fields are set depends on the event type.
data V2DutyEventValue = V2DutyEventValue
  { tripNumber :: Maybe Int,
    status :: Maybe V2DutyStatus,
    -- | "trip" or "run" for crew changes
    scope :: Maybe Text,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    vehicleNumber :: Maybe Text,
    serviceTypeId :: Maybe Text,
    isActive :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data V2DutyEventLog = V2DutyEventLog
  { id :: Text,
    gtfsId :: Text,
    operatorId :: Maybe Text,
    eventType :: V2DutyEventType,
    dutyGroupId :: Maybe Text,
    dutyId :: Maybe Text,
    dutyRepeatId :: Maybe Text,
    operationDate :: Maybe Day,
    actorPersonId :: Maybe Text,
    trigger :: Maybe V2Trigger,
    oldValue :: Maybe V2DutyEventValue,
    newValue :: Maybe V2DutyEventValue,
    reason :: Maybe V2TripReason,
    errorCode :: Maybe Text,
    errorMessage :: Maybe Text,
    resolvedAt :: Maybe UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data V2DutyEventLogPage = V2DutyEventLogPage {items :: [V2DutyEventLog], total :: Int}
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
