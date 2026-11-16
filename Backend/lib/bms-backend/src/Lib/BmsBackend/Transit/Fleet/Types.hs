module Lib.BmsBackend.Transit.Fleet.Types where

import Data.Aeson
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Data.Time (Day)
import Kernel.Prelude
import Lib.BmsBackend.Transit.Common (omitNothing)

snakeCase :: Options
snakeCase = defaultOptions {fieldLabelModifier = camelTo2 '_', omitNothingFields = True}

data TripActionV1
  = V1Start
  | V1End
  | V1Reset
  | V1Rollback
  deriving (Generic, Show, Eq)

tripActionV1Options :: Options
tripActionV1Options = defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}

instance ToJSON TripActionV1 where
  toJSON = genericToJSON tripActionV1Options

instance FromJSON TripActionV1 where
  parseJSON = genericParseJSON tripActionV1Options

instance ToSchema TripActionV1 where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions tripActionV1Options)

data FleetAnchorReq = FleetAnchorReq
  { conductorToken :: Maybe Text,
    driverToken :: Maybe Text,
    vehicleNumber :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON FleetAnchorReq where
  toJSON = genericToJSON snakeCase

instance FromJSON FleetAnchorReq where
  parseJSON = genericParseJSON snakeCase

instance ToSchema FleetAnchorReq where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions snakeCase)

data TripActionV1Req = TripActionV1Req
  { action :: TripActionV1,
    tripNumber :: Maybe Int,
    timestamp :: Maybe Int,
    conductorToken :: Maybe Text,
    driverToken :: Maybe Text,
    vehicleNumber :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON TripActionV1Req where
  toJSON = genericToJSON snakeCase

instance FromJSON TripActionV1Req where
  parseJSON = genericParseJSON snakeCase

instance ToSchema TripActionV1Req where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions snakeCase)

data TripActionV1Resp = TripActionV1Resp
  { success :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CurrentOperationV1 = CurrentOperationV1
  { waybillNo :: Text,
    vehicleNumber :: Text,
    conductorToken :: Maybe Text,
    driverToken :: Maybe Text,
    numberOfTrips :: Int,
    tripNumbers :: [Int]
  }
  deriving (Generic, Show)

instance ToJSON CurrentOperationV1 where
  toJSON = genericToJSON snakeCase

instance FromJSON CurrentOperationV1 where
  parseJSON = genericParseJSON snakeCase

instance ToSchema CurrentOperationV1 where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions snakeCase)

data ActiveTripV1 = ActiveTripV1
  { waybillNo :: Text,
    activeTripNumber :: Maybe Int,
    routeId :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON ActiveTripV1 where
  toJSON = genericToJSON snakeCase

instance FromJSON ActiveTripV1 where
  parseJSON = genericParseJSON snakeCase

instance ToSchema ActiveTripV1 where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions snakeCase)

data CurrentTripDetailsV1Req = CurrentTripDetailsV1Req
  { previousTripNumber :: Int,
    conductorToken :: Maybe Text,
    driverToken :: Maybe Text,
    vehicleNumber :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON CurrentTripDetailsV1Req where
  toJSON = genericToJSON snakeCase

instance FromJSON CurrentTripDetailsV1Req where
  parseJSON = genericParseJSON snakeCase

instance ToSchema CurrentTripDetailsV1Req where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions snakeCase)

data TripDataV1 = TripDataV1
  { tripNumber :: Int,
    routeId :: Maybe Text,
    routeNumber :: Maybe Text,
    routeName :: Maybe Text,
    isActiveTrip :: Bool,
    dutyDate :: Maybe Text,
    startTime :: Maybe Text,
    endTime :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON TripDataV1 where
  toJSON = genericToJSON snakeCase

instance FromJSON TripDataV1 where
  parseJSON = genericParseJSON snakeCase

instance ToSchema TripDataV1 where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions snakeCase)

data CurrentTripDetailsV1 = CurrentTripDetailsV1
  { waybillNo :: Text,
    vehicleNumber :: Text,
    conductorToken :: Maybe Text,
    driverToken :: Maybe Text,
    history :: [TripDataV1],
    current :: Maybe TripDataV1,
    upcoming :: [TripDataV1]
  }
  deriving (Generic, Show)

instance ToJSON CurrentTripDetailsV1 where
  toJSON = genericToJSON snakeCase

instance FromJSON CurrentTripDetailsV1 where
  parseJSON = genericParseJSON snakeCase

instance ToSchema CurrentTripDetailsV1 where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions snakeCase)

data TripActionV2
  = ActionStart
  | ActionEnd
  | ActionRollback
  | ActionSkip
  | ActionCancel
  | ActionUncancel
  deriving (Generic, Show, Eq)

tripActionV2Options :: Options
tripActionV2Options = defaultOptions {constructorTagModifier = camelTo2 '_' . drop 6}

instance ToJSON TripActionV2 where
  toJSON = genericToJSON tripActionV2Options

instance FromJSON TripActionV2 where
  parseJSON = genericParseJSON tripActionV2Options

instance ToSchema TripActionV2 where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions tripActionV2Options)

data AnchorReq = AnchorReq
  { vehicleNumber :: Maybe Text,
    driverToken :: Maybe Text,
    conductorToken :: Maybe Text,
    dutyGroupId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON AnchorReq where
  toJSON = genericToJSON omitNothing

data TripActionV2Req = TripActionV2Req
  { vehicleNumber :: Maybe Text,
    driverToken :: Maybe Text,
    conductorToken :: Maybe Text,
    dutyGroupId :: Maybe Text,
    action :: TripActionV2,
    tripNumber :: Maybe Int,
    timestamp :: Maybe Int,
    reason :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON TripActionV2Req where
  toJSON = genericToJSON omitNothing

data DutyView = DutyView
  { dutyId :: Text,
    tripId :: Text,
    dutyTripId :: Text,
    tripNumber :: Int,
    tripOrder :: Int,
    routeId :: Text,
    isBookable :: Bool,
    scheduledStartAt :: UTCTime,
    scheduledEndAt :: UTCTime,
    recordedStartTime :: Maybe UTCTime,
    recordedEndTime :: Maybe UTCTime,
    driverTokenNumber :: Maybe Text,
    driverName :: Maybe Text,
    conductorTokenNumber :: Maybe Text,
    conductorName :: Maybe Text,
    status :: Text,
    cancelReason :: Maybe Text,
    skipReason :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CurrentOperationV2 = CurrentOperationV2
  { waybillNo :: Text,
    dutyGroupId :: Text,
    tripGroupCode :: Text,
    operationDate :: Day,
    vehicleNumber :: Maybe Text,
    serviceTypeId :: Maybe Text,
    driverToken :: Maybe Text,
    conductorToken :: Maybe Text,
    active :: Maybe DutyView,
    upcoming :: [DutyView],
    history :: [DutyView]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ActiveTripV2 = ActiveTripV2
  { waybillNo :: Text,
    dutyGroupId :: Text,
    tripId :: Text,
    dutyTripId :: Text,
    tripNumber :: Int,
    routeId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
