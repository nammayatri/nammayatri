{-# LANGUAGE DuplicateRecordFields #-}

module SharedLogic.External.Nandi.Types where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Station
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Time
import Storage.CachedQueries.Merchant.MultiModalBus (BusStopETA)

newtype NandiPatternsRes = NandiPatternsRes
  { patterns :: [NandiPattern]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiPattern = NandiPattern
  { id :: Text,
    desc :: Text,
    routeId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiPatternDetails = NandiPatternDetails
  { id :: Text,
    desc :: Maybe Text,
    routeId :: Text,
    stops :: [NandiStop],
    trips :: [NandiTrip]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiStop = NandiStop
  { id :: Text,
    code :: Text,
    name :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

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
    parentStopCode :: Maybe Text,
    gates :: Maybe [Gate]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data RouteStopMappingInMemoryServerWithPublicData = RouteStopMappingInMemoryServerWithPublicData
  { estimatedTravelTimeFromPreviousStop :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    providerCode :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    sequenceNum :: Kernel.Prelude.Int,
    stopCode :: Kernel.Prelude.Text,
    stopName :: Kernel.Prelude.Text,
    stopPoint :: Kernel.External.Maps.Types.LatLong,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    geoJson :: Maybe Value,
    gates :: Maybe [Gate],
    hindiName :: Maybe Text,
    regionalName :: Maybe Text,
    parentStopCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data VehicleServiceTypeResponse = VehicleServiceTypeResponse
  { service_type :: BecknV2.FRFS.Enums.ServiceTierType,
    vehicle_no :: Text,
    last_updated :: Maybe UTCTime,
    schedule_no :: Maybe Text,
    trip_number :: Maybe Int,
    route_id :: Maybe Text,
    waybill_id :: Maybe Text,
    route_number :: Maybe Text,
    depot :: Maybe Text,
    remaining_trip_details :: Maybe [BusScheduleTrip],
    is_actually_valid :: Maybe Bool,
    driver_id :: Maybe Text,
    conductor_id :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data BusScheduleTrip = BusScheduleTrip
  { schedule_number :: Maybe Text,
    route_id :: Text,
    route_name :: Maybe Text,
    org_name :: Maybe Text,
    trip_number :: Maybe Int,
    route_number :: Maybe Text,
    stops_count :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

newtype StopCodeResponse = StopCodeResponse
  { stop_code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data TripInfoResponse = TripInfoResponse
  { tripId :: Text,
    routeId :: Text,
    routeName :: Text,
    direction :: Maybe Text,
    stops :: [StopInfo],
    schedule :: [StopSchedule],
    lastUpdated :: UTCTime,
    source :: Text
  }
  deriving (Show, Generic)

instance FromJSON TripInfoResponse

instance ToJSON TripInfoResponse

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

data RouteInfoNandi = RouteInfoNandi
  { id :: Text,
    shortName :: Maybe Text,
    longName :: Maybe Text,
    mode :: BecknV2.FRFS.Enums.VehicleCategory,
    agencyName :: Maybe Text,
    tripCount :: Maybe Int,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    stopCount :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data BusScheduleDetail = BusScheduleDetail
  { eta :: [BusStopETA],
    vehicle_no :: Text,
    service_tier :: BecknV2.FRFS.Enums.ServiceTierType
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

type BusScheduleDetails = [BusScheduleDetail]

data NandiTrip = NandiTrip
  { id :: Text,
    direction :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiRoutesRes = NandiRoutesRes
  { id :: Text,
    shortName :: Maybe Text,
    longName :: Maybe Text,
    mode :: Text,
    agencyName :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data RouteStopMappingNandi = RouteStopMappingNandi
  { dailyTripCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    routeLongName :: Kernel.Prelude.Text,
    routeShortName :: Kernel.Prelude.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    stopCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    estimatedTravelTimeFromPreviousStop :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    routeCode :: Kernel.Prelude.Text,
    sequenceNum :: Kernel.Prelude.Int,
    stopCode :: Kernel.Prelude.Text,
    stopName :: Kernel.Prelude.Text,
    stopPoint :: Kernel.External.Maps.Types.LatLong,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data GtfsGraphQLRequest = GtfsGraphQLRequest
  { query :: Text,
    variables :: Maybe Value,
    operation_name :: Maybe Text,
    city :: Maybe Text, -- todo: remove this
    feedId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data RouteStopMappingByStopCodesReq = RouteStopMappingByStopCodesReq
  { stopCodes :: [Text],
    gtfsId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

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

data VehicleInfoResponse = VehicleInfoResponse
  { driverCode :: Text,
    conductorCode :: Text,
    depotName :: Text,
    scheduleNo :: Text,
    waybillNo :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data VehicleOperationInfo = VehicleOperationInfo
  { waybill_id :: Maybe Text,
    waybill_no :: Maybe Text,
    depot_id :: Text,
    depot_name :: Text,
    conductor_code :: Maybe Text,
    driver_code :: Maybe Text,
    schedule_no :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data DepotVehicle = DepotVehicle
  { fleet_no :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicle_no :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
