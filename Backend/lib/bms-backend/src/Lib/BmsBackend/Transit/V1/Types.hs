module Lib.BmsBackend.Transit.V1.Types where

import Data.Aeson
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Data.Time (Day)
import Kernel.Prelude
import Kernel.Types.HideSecrets (HideSecrets (..))
import Lib.BmsBackend.Transit.Common (ItemsResponse, ListResponse, omitNothing)

data Schedule = Schedule
  { scheduleId :: Text,
    scheduleNumber :: Maybe Text,
    serviceCode :: Maybe Text,
    routeId :: Maybe Text,
    routeCode :: Maybe Text,
    routeName :: Maybe Text,
    serviceTypeId :: Maybe Text,
    serviceTypeCode :: Maybe Text,
    serviceTypeName :: Maybe Text,
    scheduleTypeId :: Maybe Text,
    scheduleTypeCode :: Maybe Text,
    scheduleTypeName :: Maybe Text,
    depotId :: Maybe Text,
    depotName :: Maybe Text,
    organizationId :: Text,
    status :: Maybe Text,
    effectiveFrom :: Maybe UTCTime,
    effectiveTill :: Maybe UTCTime,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateScheduleReq = CreateScheduleReq
  { depotId :: Text,
    routeId :: Text,
    serviceTypeId :: Text,
    scheduleTypeId :: Text,
    scheduleNumber :: Maybe Text,
    serviceCode :: Maybe Text,
    effectiveFrom :: Maybe UTCTime,
    effectiveTill :: Maybe UTCTime,
    status :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateScheduleReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateScheduleReq where
  hideSecrets = identity

data UpdateScheduleReq = UpdateScheduleReq
  { depotId :: Maybe Text,
    routeId :: Maybe Text,
    serviceTypeId :: Maybe Text,
    scheduleTypeId :: Maybe Text,
    scheduleNumber :: Maybe Text,
    serviceCode :: Maybe Text,
    effectiveFrom :: Maybe UTCTime,
    effectiveTill :: Maybe UTCTime,
    status :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateScheduleReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateScheduleReq where
  hideSecrets = identity

data ScheduleType = ScheduleType
  { scheduleTypeId :: Text,
    scheduleTypeCode :: Maybe Text,
    scheduleTypeName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ScheduleTrip = ScheduleTrip
  { scheduleTripId :: Text,
    scheduleId :: Maybe Text,
    scheduleNumber :: Maybe Text,
    scheduleNumberName :: Maybe Text,
    noTrip :: Int,
    startTime :: Maybe Text,
    status :: Maybe Text,
    effectiveStart :: Maybe UTCTime,
    effectiveEnd :: Maybe UTCTime,
    repeatStatus :: Text,
    validFrom :: Maybe Day,
    validUntil :: Maybe Day,
    recurrenceDays :: [Int],
    generatedTill :: Maybe Day,
    routeId :: Maybe Text,
    depotId :: Maybe Text,
    organizationId :: Text,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateScheduleTripReq = CreateScheduleTripReq
  { scheduleId :: Text,
    dayType :: Maybe Text,
    scheduleNumberName :: Maybe Text,
    startTime :: Text,
    noTrip :: Maybe Int,
    effectiveStart :: Maybe UTCTime,
    effectiveEnd :: Maybe UTCTime,
    status :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateScheduleTripReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateScheduleTripReq where
  hideSecrets = identity

data UpdateScheduleTripReq = UpdateScheduleTripReq
  { dayType :: Maybe Text,
    scheduleNumberName :: Maybe Text,
    startTime :: Maybe Text,
    noTrip :: Maybe Int,
    effectiveStart :: Maybe UTCTime,
    effectiveEnd :: Maybe UTCTime,
    status :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateScheduleTripReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateScheduleTripReq where
  hideSecrets = identity

data RepeatConfig = RepeatConfig
  { scheduleTripId :: Text,
    repeatStatus :: Text,
    validFrom :: Maybe Day,
    validUntil :: Maybe Day,
    recurrenceDays :: [Int],
    generatedTill :: Maybe Day
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data SetRepeatConfigReq = SetRepeatConfigReq
  { repeatStatus :: Text,
    validFrom :: Maybe Day,
    validUntil :: Maybe Day,
    recurrenceDays :: [Int]
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON SetRepeatConfigReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets SetRepeatConfigReq where
  hideSecrets = identity

data TripDetail = TripDetail
  { scheduleTripDetailId :: Text,
    scheduleTripId :: Maybe Text,
    tripNumber :: Int,
    tripOrder :: Int,
    tripType :: Maybe Text,
    startTime :: Maybe Text,
    endTime :: Maybe Text,
    runningTime :: Maybe Text,
    breakTime :: Maybe Text,
    breakType :: Maybe Text,
    distance :: Double,
    routeId :: Maybe Text,
    routeNumber :: Maybe Text,
    routeName :: Maybe Text,
    orgName :: Maybe Text,
    scheduleNumber :: Maybe Text,
    shiftTypeName :: Maybe Text,
    shiftDayName :: Maybe Text,
    status :: Maybe Text,
    isActiveTrip :: Bool,
    isCompleted :: Maybe Bool,
    tripStartTime :: Maybe Int,
    tripEndTime :: Maybe Int,
    syncStartTime :: Maybe Int,
    syncEndTime :: Maybe Int,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data TripDetailInput = TripDetailInput
  { scheduleTripDetailId :: Maybe Text,
    tripNumber :: Int,
    tripType :: Maybe Text,
    startTime :: Maybe Text,
    endTime :: Maybe Text,
    runningTime :: Maybe Text,
    breakTime :: Maybe Text,
    breakType :: Maybe Text,
    distance :: Maybe Double,
    routeId :: Text,
    status :: Maybe Text,
    shiftDayName :: Maybe Text,
    orgName :: Maybe Text,
    scheduleNumber :: Maybe Text,
    shiftTypeName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON TripDetailInput where
  toJSON = genericToJSON omitNothing

data ReplaceTripDetailsReq = ReplaceTripDetailsReq
  { details :: [TripDetailInput]
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON ReplaceTripDetailsReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets ReplaceTripDetailsReq where
  hideSecrets = identity

data UpdateTripDetailReq = UpdateTripDetailReq
  { tripNumber :: Maybe Int,
    tripOrder :: Maybe Int,
    tripType :: Maybe Text,
    startTime :: Maybe Text,
    endTime :: Maybe Text,
    runningTime :: Maybe Text,
    breakTime :: Maybe Text,
    breakType :: Maybe Text,
    distance :: Maybe Double,
    routeId :: Maybe Text,
    status :: Maybe Text,
    shiftDayName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateTripDetailReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateTripDetailReq where
  hideSecrets = identity

data Waybill = Waybill
  { waybillId :: Text,
    waybillNo :: Maybe Text,
    dutyDate :: Maybe Text,
    status :: Maybe Text,
    vehicleNo :: Maybe Text,
    driverTokenNo :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNo :: Maybe Text,
    conductorName :: Maybe Text,
    noOfDevice :: Int,
    deviceSerialNumber :: Maybe Text,
    tabletId :: Maybe Text,
    scheduleId :: Text,
    scheduleNo :: Maybe Text,
    scheduleTripId :: Maybe Text,
    scheduleTripName :: Maybe Text,
    scheduleType :: Maybe Text,
    scheduleStartTime :: Maybe Text,
    serviceType :: Maybe Text,
    serviceTypeId :: Maybe Text,
    shiftTypeId :: Maybe Text,
    depotId :: Maybe Text,
    depotName :: Maybe Text,
    organizationId :: Text,
    isFlexi :: Bool,
    auditedDate :: Maybe UTCTime,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreateWaybillReq = CreateWaybillReq
  { scheduleTripId :: Text,
    dutyDate :: Day,
    vehicleNo :: Text,
    driverTokenNo :: Text,
    conductorTokenNo :: Maybe Text,
    noOfDevice :: Int,
    deviceSerialNumber :: Maybe Text,
    status :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON CreateWaybillReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets CreateWaybillReq where
  hideSecrets = identity

data UpdateWaybillReq = UpdateWaybillReq
  { vehicleNo :: Maybe Text,
    driverTokenNo :: Maybe Text,
    conductorTokenNo :: Maybe Text,
    noOfDevice :: Maybe Int,
    deviceSerialNumber :: Maybe Text,
    tabletId :: Maybe Text,
    status :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON UpdateWaybillReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets UpdateWaybillReq where
  hideSecrets = identity

data RepeatRangeReq = RepeatRangeReq
  { from :: Day,
    to :: Day,
    scheduleTripIds :: Maybe [Text]
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON RepeatRangeReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets RepeatRangeReq where
  hideSecrets = identity

data RepeatVerdict
  = OutOfWindow
  | OffDay
  | Exists
  | Ok
  | Created
  | SkippedNoPriorWaybill
  deriving (Generic, Show, Eq)

repeatVerdictOptions :: Options
repeatVerdictOptions = defaultOptions {constructorTagModifier = camelTo2 '_'}

instance ToJSON RepeatVerdict where
  toJSON = genericToJSON repeatVerdictOptions

instance FromJSON RepeatVerdict where
  parseJSON = genericParseJSON repeatVerdictOptions

instance ToSchema RepeatVerdict where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions repeatVerdictOptions)

data RepeatWalkEntry = RepeatWalkEntry
  { scheduleTripId :: Text,
    dutyDate :: Text,
    verdict :: RepeatVerdict,
    waybillId :: Maybe Text,
    waybillNo :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data RepeatWalkResponse = RepeatWalkResponse
  { created :: Int,
    entries :: [RepeatWalkEntry]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type ScheduleList = ListResponse Schedule

type ScheduleTripList = ListResponse ScheduleTrip

type ScheduleTypeItems = ItemsResponse ScheduleType

type TripDetailItems = ItemsResponse TripDetail

type WaybillList = ListResponse Waybill
