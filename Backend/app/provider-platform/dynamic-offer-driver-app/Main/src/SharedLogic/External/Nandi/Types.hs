{-# LANGUAGE DuplicateRecordFields #-}

module SharedLogic.External.Nandi.Types where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import Data.OpenApi ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.HideSecrets (HideSecrets (..))
import qualified Kernel.Types.Time

data RouteStopMappingInMemoryServer = RouteStopMappingInMemoryServer
  { estimatedTravelTimeFromPreviousStop :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    providerCode :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    sequenceNum :: Kernel.Prelude.Int,
    stopCode :: Kernel.Prelude.Text,
    stopName :: Kernel.Prelude.Text,
    stopPoint :: Kernel.External.Maps.Types.LatLong,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    hindiName :: Maybe Text,
    regionalName :: Maybe Text,
    parentStopCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data RouteInfoNandi = RouteInfoNandi
  { id :: Text,
    shortName :: Maybe Text,
    longName :: Maybe Text,
    mode :: BecknV2.FRFS.Enums.VehicleCategory,
    agencyName :: Maybe Text,
    tripCount :: Maybe Int,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    stopCount :: Maybe Int,
    serviceTierType :: Maybe BecknV2.FRFS.Enums.ServiceTierType
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

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
  toJSON (StopInfo stopId stopCode stopName sequenceNum lat lon) =
    object
      [ "stopId" .= stopId,
        "stopCode" .= stopCode,
        "stopName" .= stopName,
        "sequence" .= sequenceNum,
        "lat" .= lat,
        "lon" .= lon
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
  toJSON (StopSchedule stopCode arrivalTime departureTime sequenceNum) =
    object
      [ "stopCode" .= stopCode,
        "arrivalTime" .= arrivalTime,
        "departureTime" .= departureTime,
        "sequence" .= sequenceNum
      ]

data ExtraInfo = ExtraInfo
  { fareStageNumber :: Maybe Text,
    providerStopCode :: Maybe Text,
    isStageStop :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Replace single quotes with double quotes
sanitizeJsonQuotes :: Text -> Text
sanitizeJsonQuotes = T.replace "'" "\""

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
  deriving (Generic, Show, ToSchema)

instance FromJSON TripStopDetail where
  parseJSON = withObject "TripStopDetail" $ \obj -> do
    headsignParser <- do
      mHeadsignText <- obj .:? "headsign"
      case mHeadsignText of
        Nothing -> pure Nothing
        Just headsignText -> do
          let sanitized = sanitizeJsonQuotes headsignText
          -- Try to parse headsign as JSON first
          case eitherDecodeStrict (TE.encodeUtf8 sanitized) of
            Right (Object headsignObj) -> do
              -- Parse as ExtraInfo object
              extraInfo <- parseJSON (Object headsignObj)
              pure (Just extraInfo)
            Right (String jsonString) -> do
              -- The JSON string contains another JSON object, parse that
              case eitherDecodeStrict (TE.encodeUtf8 (sanitizeJsonQuotes jsonString)) of
                Right (Object headsignObj) -> do
                  extraInfo <- parseJSON (Object headsignObj)
                  pure (Just extraInfo)
                _ -> do
                  -- Fallback: treat as simple text for fareStageNumber
                  pure (Just (ExtraInfo (Just headsignText) Nothing Nothing))
            _ -> do
              -- Fallback: treat as simple text for fareStageNumber
              pure (Just (ExtraInfo (Just headsignText) Nothing Nothing))

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
  toJSON (TripStopDetail stopId stopCode stopName platformCode lat lon scheduledArrival scheduledDeparture extraInfo stopPosition) =
    object
      [ "stopId" .= stopId,
        "stopCode" .= stopCode,
        "stopName" .= stopName,
        "platformCode" .= platformCode,
        "lat" .= lat,
        "lon" .= lon,
        "scheduledArrival" .= scheduledArrival,
        "scheduledDeparture" .= scheduledDeparture,
        "extraInfo" .= extraInfo,
        "stopPosition" .= stopPosition
      ]

data TripDetails = TripDetails
  { tripId :: Text,
    stops :: [TripStopDetail]
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON TripDetails where
  parseJSON = withObject "TripDetails" $ \obj ->
    TripDetails
      <$> obj .: "tripId"
      <*> obj .: "stops"

instance ToJSON TripDetails where
  toJSON (TripDetails tripId stops) =
    object
      [ "tripId" .= tripId,
        "stops" .= stops
      ]

data GimsTripAction
  = GimsTripActionStart
  | GimsTripActionEnd
  | GimsTripActionReset
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

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
  deriving (Generic, FromJSON, ToSchema, Show)

instance ToJSON GimsTripActionReq where
  toJSON (GimsTripActionReq act tn ts ct dt vn) =
    object $
      ("action" .= act) :
      catMaybes
        [ ("trip_number" .=) <$> tn,
          ("timestamp" .=) <$> ts,
          ("conductor_token" .=) <$> ct,
          ("driver_token" .=) <$> dt,
          ("vehicle_number" .=) <$> vn
        ]

data GimsCurrentOperationResp = GimsCurrentOperationResp
  { waybill_no :: Text,
    number_of_trips :: Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data GimsCurrentTripDetailsReq = GimsCurrentTripDetailsReq
  { previous_trip_number :: Int,
    conductor_token :: Maybe Text,
    driver_token :: Maybe Text,
    vehicle_number :: Maybe Text
  }
  deriving (Generic, FromJSON, ToSchema, Show)

instance ToJSON GimsCurrentTripDetailsReq where
  toJSON (GimsCurrentTripDetailsReq ptn ct dt vn) =
    object $
      ("previous_trip_number" .= ptn) :
      catMaybes
        [ ("conductor_token" .=) <$> ct,
          ("driver_token" .=) <$> dt,
          ("vehicle_number" .=) <$> vn
        ]

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
  deriving anyclass (ToJSON, ToSchema)

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
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data GimsOperationAnchor = GimsOperationAnchor
  { conductor_token :: Maybe Text,
    driver_token :: Maybe Text,
    vehicle_number :: Maybe Text
  }
  deriving (Generic, FromJSON, ToSchema, Show)

instance ToJSON GimsOperationAnchor where
  toJSON (GimsOperationAnchor ct dt vn) =
    object $
      catMaybes
        [ ("conductor_token" .=) <$> ct,
          ("driver_token" .=) <$> dt,
          ("vehicle_number" .=) <$> vn
        ]

instance HideSecrets GimsOperationAnchor where
  hideSecrets = identity

-- | Request body for the fleet-operator employee login endpoint.
-- GIMS expects pre-hashed values: SHA256(hashSalt <> value).
data GimsEmployeeLoginReq = GimsEmployeeLoginReq
  { auth_type :: Maybe Text,
    email_hash :: Text,
    password_hash :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets GimsEmployeeLoginReq where
  hideSecrets req = req {email_hash = "***", password_hash = "***"}

-- | Response body from the fleet-operator employee login endpoint.
-- Returns verification status and optional badge token.
data GimsEmployeeLoginResp = GimsEmployeeLoginResp
  { verified :: Bool,
    token :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data StopCodeResponse = StopCodeResponse
  { stopCodes :: [Text]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
