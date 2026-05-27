{-# LANGUAGE DuplicateRecordFields #-}

module Lib.GtfsDataServer.Types where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import Data.OpenApi ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Maps (HasCoordinates (..))
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.HideSecrets (HideSecrets (..))
import Kernel.Types.Time (Seconds)

sanitizeJsonQuotes :: Text -> Text
sanitizeJsonQuotes = T.replace "'" "\""

data Gate = Gate
  { gateName :: String,
    stopCode :: String,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance HasCoordinates Gate

data RouteStopMappingInMemoryServer = RouteStopMappingInMemoryServer
  { estimatedTravelTimeFromPreviousStop :: Maybe Seconds,
    providerCode :: Text,
    routeCode :: Text,
    sequenceNum :: Int,
    stopCode :: Text,
    stopName :: Text,
    stopPoint :: LatLong,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    hindiName :: Maybe Text,
    regionalName :: Maybe Text,
    parentStopCode :: Maybe Text,
    gates :: Maybe [Gate]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data RouteInfoNandi = RouteInfoNandi
  { id :: Text,
    shortName :: Maybe Text,
    longName :: Maybe Text,
    mode :: BecknV2.FRFS.Enums.VehicleCategory,
    agencyName :: Maybe Text,
    tripCount :: Maybe Int,
    startPoint :: LatLong,
    endPoint :: LatLong,
    stopCount :: Maybe Int,
    serviceTierType :: Maybe BecknV2.FRFS.Enums.ServiceTierType
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data ExtraInfo = ExtraInfo
  { fareStageNumber :: Maybe Text,
    providerStopCode :: Maybe Text,
    isStageStop :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data StopInfo = StopInfo
  { stopId :: Text,
    stopCode :: Text,
    stopName :: Text,
    sequenceNum :: Int,
    lat :: Double,
    lon :: Double
  }
  deriving (Show, Generic)

instance FromJSON StopInfo where
  parseJSON = withObject "StopInfo" $ \v ->
    StopInfo
      <$> v .: "stopId"
      <*> v .: "stopCode"
      <*> v .: "stopName"
      <*> v .: "sequence"
      <*> v .: "lat"
      <*> v .: "lon"

instance ToJSON StopInfo where
  toJSON (StopInfo sId sCode sName seqNum lat' lon') =
    object
      [ "stopId" .= sId,
        "stopCode" .= sCode,
        "stopName" .= sName,
        "sequence" .= seqNum,
        "lat" .= lat',
        "lon" .= lon'
      ]

data StopSchedule = StopSchedule
  { stopCode :: Text,
    arrivalTime :: Int,
    departureTime :: Int,
    sequenceNum :: Int
  }
  deriving (Show, Generic)

instance FromJSON StopSchedule where
  parseJSON = withObject "StopSchedule" $ \v ->
    StopSchedule
      <$> v .: "stopCode"
      <*> v .: "arrivalTime"
      <*> v .: "departureTime"
      <*> v .: "sequence"

instance ToJSON StopSchedule where
  toJSON (StopSchedule sCode arrTime depTime seqNum) =
    object
      [ "stopCode" .= sCode,
        "arrivalTime" .= arrTime,
        "departureTime" .= depTime,
        "sequence" .= seqNum
      ]

data TripStopDetail = TripStopDetail
  { stopId :: Text,
    stopCode :: Text,
    stopName :: Maybe Text,
    platformCode :: Maybe Text,
    lat :: Double,
    lon :: Double,
    scheduledArrival :: Int,
    scheduledDeparture :: Int,
    extraInfo :: Maybe ExtraInfo,
    stopPosition :: Int
  }
  deriving (Generic, Show)

instance FromJSON TripStopDetail where
  parseJSON = withObject "TripStopDetail" $ \obj -> do
    headsignParser <- do
      mHeadsignText <- obj .:? "headsign"
      case mHeadsignText of
        Nothing -> pure Nothing
        Just headsignText -> do
          let sanitized = sanitizeJsonQuotes headsignText
          case eitherDecodeStrict (TE.encodeUtf8 sanitized) of
            Right (Object headsignObj) -> do
              ei <- parseJSON (Object headsignObj)
              pure (Just ei)
            Right (String jsonString) -> do
              case eitherDecodeStrict (TE.encodeUtf8 (sanitizeJsonQuotes jsonString)) of
                Right (Object headsignObj) -> do
                  ei <- parseJSON (Object headsignObj)
                  pure (Just ei)
                _ -> pure (Just (ExtraInfo (Just headsignText) Nothing Nothing))
            _ -> pure (Just (ExtraInfo (Just headsignText) Nothing Nothing))
    TripStopDetail
      <$> obj .: "stopId"
      <*> obj .: "stopCode"
      <*> obj .:? "stopName"
      <*> obj .:? "platformCode"
      <*> obj .: "lat"
      <*> obj .: "lon"
      <*> obj .: "scheduledArrival"
      <*> obj .: "scheduledDeparture"
      <*> pure headsignParser
      <*> obj .: "stopPosition"

instance ToJSON TripStopDetail where
  toJSON (TripStopDetail sId sCode sName pCode lat' lon' schedArr schedDep ei stopPos) =
    object
      [ "stopId" .= sId,
        "stopCode" .= sCode,
        "stopName" .= sName,
        "platformCode" .= pCode,
        "lat" .= lat',
        "lon" .= lon',
        "scheduledArrival" .= schedArr,
        "scheduledDeparture" .= schedDep,
        "extraInfo" .= ei,
        "stopPosition" .= stopPos
      ]

data TripDetails = TripDetails
  { tripId :: Text,
    stops :: [TripStopDetail]
  }
  deriving (Generic, Show)

instance FromJSON TripDetails where
  parseJSON = withObject "TripDetails" $ \obj ->
    TripDetails
      <$> obj .: "tripId"
      <*> obj .: "stops"

instance ToJSON TripDetails where
  toJSON (TripDetails tid ss) =
    object ["tripId" .= tid, "stops" .= ss]

-- | Single stop-code lookup response (GIMS "stop-code" endpoint).
newtype StopCodeResponse = StopCodeResponse
  { stop_code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data GimsTripAction
  = GimsTripActionStart
  | GimsTripActionEnd
  | GimsTripActionReset
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON GimsTripAction where
  toJSON GimsTripActionStart = toJSON ("start" :: Text)
  toJSON GimsTripActionEnd = toJSON ("end" :: Text)
  toJSON GimsTripActionReset = toJSON ("reset" :: Text)

instance FromJSON GimsTripAction where
  parseJSON = withText "GimsTripAction" $ \case
    "start" -> pure GimsTripActionStart
    "end" -> pure GimsTripActionEnd
    "reset" -> pure GimsTripActionReset
    v -> fail $ "Unknown GimsTripAction: " <> T.unpack v

data GimsTripActionReq = GimsTripActionReq
  { action :: GimsTripAction,
    trip_number :: Maybe Int,
    timestamp :: Maybe Int64,
    conductor_token :: Maybe Text,
    driver_token :: Maybe Text,
    vehicle_number :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data GimsCurrentOperationResp = GimsCurrentOperationResp
  { waybill_no :: Text,
    number_of_trips :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data GimsCurrentTripDetailsReq = GimsCurrentTripDetailsReq
  { previous_trip_number :: Int,
    conductor_token :: Maybe Text,
    driver_token :: Maybe Text,
    vehicle_number :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data GimsTripInfo = GimsTripInfo
  { trip_number :: Int,
    route_id :: Text,
    route_number :: Text,
    route_name :: Text,
    is_active_trip :: Bool,
    duty_date :: Maybe Text,
    start_time :: Maybe Text,
    end_time :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

instance FromJSON GimsTripInfo where
  parseJSON = withObject "GimsTripInfo" $ \v -> do
    trip_number <- v .: "trip_number"
    route_id <- v .: "route_id"
    route_number <- fromMaybe route_id <$> (v .:? "route_number")
    route_name <- fromMaybe "" <$> (v .:? "route_name")
    is_active_trip <- v .: "is_active_trip"
    duty_date <- v .:? "duty_date"
    start_time <- v .:? "start_time"
    end_time <- v .:? "end_time"
    return GimsTripInfo {..}

data GimsCurrentTripDetailsResp = GimsCurrentTripDetailsResp
  { waybill_no :: Text,
    vehicle_number :: Text,
    conductor_token :: Maybe Text,
    driver_token :: Maybe Text,
    history :: [GimsTripInfo],
    current :: Maybe GimsTripInfo,
    upcoming :: [GimsTripInfo]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data GimsOperationAnchor = GimsOperationAnchor
  { conductor_token :: Maybe Text,
    driver_token :: Maybe Text,
    vehicle_number :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance HideSecrets GimsOperationAnchor where
  hideSecrets = identity

-- | Verify conductor badge token against device serial number (GIMS "verify" endpoint).
data GimsVerifyReq = GimsVerifyReq
  { operator_badge_token :: Text,
    device_serial_number :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance HideSecrets GimsVerifyReq where
  hideSecrets = identity

newtype GimsVerifyResp = GimsVerifyResp
  { verified :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- | Employee login request for conductor GIMS_EMAIL_PASSWORD auth (driver-app only).
-- GIMS expects pre-hashed values: SHA256(hashSalt <> value).
data GimsEmployeeLoginReq = GimsEmployeeLoginReq
  { auth_type :: Maybe Text,
    email_hash :: Text,
    password_hash :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance HideSecrets GimsEmployeeLoginReq where
  hideSecrets req = req {email_hash = "***", password_hash = "***"}

-- | Roles GIMS recognises for an employee. Wire form is lowercase
-- (\"driver\" / \"conductor\"); parser is case-insensitive. Unknown values
-- fail the whole login response — keep this in sync with the Rust enum.
data GimsEmployeeRole = GimsDriver | GimsConductor
  deriving (Generic, Show, Eq)

instance FromJSON GimsEmployeeRole where
  parseJSON = withText "GimsEmployeeRole" $ \t -> case T.toLower t of
    "driver" -> pure GimsDriver
    "conductor" -> pure GimsConductor
    other -> fail $ "Unknown GIMS employee role: " <> T.unpack other

instance ToJSON GimsEmployeeRole where
  toJSON GimsDriver = String "driver"
  toJSON GimsConductor = String "conductor"

-- | Response from the employee login endpoint (driver-app only).
data GimsEmployeeLoginResp = GimsEmployeeLoginResp
  { verified :: Bool,
    token :: Maybe Text,
    role :: Maybe GimsEmployeeRole
  }
  deriving (Generic, FromJSON, ToJSON, Show)
