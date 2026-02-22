{-# LANGUAGE DuplicateRecordFields #-}

module SharedLogic.External.Nandi.Types where

import qualified BecknV2.FRFS.Enums
import Control.Lens ((?~))
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.OpenApi (OpenApiType (..), ToParamSchema (..), enum_, toParamSchema, type_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day)
import Domain.Types.Station
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.HideSecrets (HideSecrets (..))
import qualified Kernel.Types.Time
import Storage.CachedQueries.Merchant.MultiModalBus (BusStopETA)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

data NandiTable
  = RouteInternal
  | RoutePointInternal
  | BusScheduleInternal
  | BusScheduleTripInternal
  | BusScheduleTripDetailInternal
  | BusScheduleTripFlexiInternal
  | ServiceTypeInternal
  | StopInternal
  | DesignationsInternal
  | EmployeesInternal
  | EntitiesInternal
  | VehiclesInternal
  | WaybillDeviceInternal
  | FleetEtmMappingInternal
  | FleetObuMappingInternal
  | WaybillsInternal
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

nandiTableToText :: NandiTable -> Text
nandiTableToText RouteInternal = "route_internal"
nandiTableToText RoutePointInternal = "route_point_internal"
nandiTableToText BusScheduleInternal = "bus_schedule_internal"
nandiTableToText BusScheduleTripInternal = "bus_schedule_trip_internal"
nandiTableToText BusScheduleTripDetailInternal = "bus_schedule_trip_detail_internal"
nandiTableToText BusScheduleTripFlexiInternal = "bus_schedule_trip_flexi_internal"
nandiTableToText ServiceTypeInternal = "service_type_internal"
nandiTableToText StopInternal = "stop_internal"
nandiTableToText DesignationsInternal = "designations_internal"
nandiTableToText EmployeesInternal = "employees_internal"
nandiTableToText EntitiesInternal = "entities_internal"
nandiTableToText VehiclesInternal = "vehicles_internal"
nandiTableToText WaybillDeviceInternal = "waybill_device_internal"
nandiTableToText FleetEtmMappingInternal = "fleet_etm_mapping_internal"
nandiTableToText FleetObuMappingInternal = "fleet_obu_mapping_internal"
nandiTableToText WaybillsInternal = "waybills_internal"

nandiTableFromText :: Text -> Either Text NandiTable
nandiTableFromText "route_internal" = Right RouteInternal
nandiTableFromText "route_point_internal" = Right RoutePointInternal
nandiTableFromText "bus_schedule_internal" = Right BusScheduleInternal
nandiTableFromText "bus_schedule_trip_internal" = Right BusScheduleTripInternal
nandiTableFromText "bus_schedule_trip_detail_internal" = Right BusScheduleTripDetailInternal
nandiTableFromText "bus_schedule_trip_flexi_internal" = Right BusScheduleTripFlexiInternal
nandiTableFromText "service_type_internal" = Right ServiceTypeInternal
nandiTableFromText "stop_internal" = Right StopInternal
nandiTableFromText "designations_internal" = Right DesignationsInternal
nandiTableFromText "employees_internal" = Right EmployeesInternal
nandiTableFromText "entities_internal" = Right EntitiesInternal
nandiTableFromText "vehicles_internal" = Right VehiclesInternal
nandiTableFromText "waybill_device_internal" = Right WaybillDeviceInternal
nandiTableFromText "fleet_etm_mapping_internal" = Right FleetEtmMappingInternal
nandiTableFromText "fleet_obu_mapping_internal" = Right FleetObuMappingInternal
nandiTableFromText "waybills_internal" = Right WaybillsInternal
nandiTableFromText t = Left $ "Unknown NandiTable: " <> t

instance ToJSON NandiTable where
  toJSON = toJSON . nandiTableToText

instance FromJSON NandiTable where
  parseJSON = withText "NandiTable" $ \t ->
    either (fail . T.unpack) pure (nandiTableFromText t)

instance ToHttpApiData NandiTable where
  toQueryParam = nandiTableToText

instance FromHttpApiData NandiTable where
  parseQueryParam = nandiTableFromText

instance ToParamSchema NandiTable where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ map (toJSON . nandiTableToText) [minBound .. maxBound]

instance Bounded NandiTable where
  minBound = RouteInternal
  maxBound = WaybillsInternal

instance Enum NandiTable where
  toEnum 0 = RouteInternal
  toEnum 1 = RoutePointInternal
  toEnum 2 = BusScheduleInternal
  toEnum 3 = BusScheduleTripInternal
  toEnum 4 = BusScheduleTripDetailInternal
  toEnum 5 = BusScheduleTripFlexiInternal
  toEnum 6 = ServiceTypeInternal
  toEnum 7 = StopInternal
  toEnum 8 = DesignationsInternal
  toEnum 9 = EmployeesInternal
  toEnum 10 = EntitiesInternal
  toEnum 11 = VehiclesInternal
  toEnum 12 = WaybillDeviceInternal
  toEnum 13 = FleetEtmMappingInternal
  toEnum 14 = FleetObuMappingInternal
  toEnum 15 = WaybillsInternal
  toEnum n = error $ "NandiTable.toEnum: bad argument " <> show n
  fromEnum RouteInternal = 0
  fromEnum RoutePointInternal = 1
  fromEnum BusScheduleInternal = 2
  fromEnum BusScheduleTripInternal = 3
  fromEnum BusScheduleTripDetailInternal = 4
  fromEnum BusScheduleTripFlexiInternal = 5
  fromEnum ServiceTypeInternal = 6
  fromEnum StopInternal = 7
  fromEnum DesignationsInternal = 8
  fromEnum EmployeesInternal = 9
  fromEnum EntitiesInternal = 10
  fromEnum VehiclesInternal = 11
  fromEnum WaybillDeviceInternal = 12
  fromEnum FleetEtmMappingInternal = 13
  fromEnum FleetObuMappingInternal = 14
  fromEnum WaybillsInternal = 15

-- | Waybill lifecycle statuses.
data WaybillStatus
  = WaybillOnline
  | WaybillUpcoming
  | WaybillNew
  | WaybillProcessed
  | WaybillAudited
  | WaybillClosed
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

instance ToJSON WaybillStatus where
  toJSON WaybillOnline = toJSON ("online" :: Text)
  toJSON WaybillUpcoming = toJSON ("upcoming" :: Text)
  toJSON WaybillNew = toJSON ("new" :: Text)
  toJSON WaybillProcessed = toJSON ("processed" :: Text)
  toJSON WaybillAudited = toJSON ("audited" :: Text)
  toJSON WaybillClosed = toJSON ("closed" :: Text)

instance FromJSON WaybillStatus where
  parseJSON = withText "WaybillStatus" $ \case
    "online" -> pure WaybillOnline
    "upcoming" -> pure WaybillUpcoming
    "new" -> pure WaybillNew
    "processed" -> pure WaybillProcessed
    "audited" -> pure WaybillAudited
    "closed" -> pure WaybillClosed
    v -> fail $ "Unknown WaybillStatus: " <> T.unpack v

-- | Employee roles.
data OperatorRole
  = OperatorRoleDrivers
  | OperatorRoleConductors
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

operatorRoleToText :: OperatorRole -> Text
operatorRoleToText OperatorRoleDrivers = "drivers"
operatorRoleToText OperatorRoleConductors = "conductors"

instance ToJSON OperatorRole where
  toJSON = toJSON . operatorRoleToText

instance FromJSON OperatorRole where
  parseJSON = withText "OperatorRole" $ \case
    "drivers" -> pure OperatorRoleDrivers
    "conductors" -> pure OperatorRoleConductors
    v -> fail $ "Unknown OperatorRole: " <> T.unpack v

instance ToHttpApiData OperatorRole where
  toQueryParam = operatorRoleToText

instance FromHttpApiData OperatorRole where
  parseQueryParam "drivers" = Right OperatorRoleDrivers
  parseQueryParam "conductors" = Right OperatorRoleConductors
  parseQueryParam v = Left $ "Unknown OperatorRole: " <> v

instance ToParamSchema OperatorRole where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["drivers", "conductors"]

-- | Shift types.
data ShiftType
  = ShiftAm
  | ShiftPm
  | ShiftFull
  | ShiftPmNight
  | ShiftPmAm
  | ShiftNightHalt
  | ShiftGeneral
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

instance ToJSON ShiftType where
  toJSON ShiftAm = toJSON ("am" :: Text)
  toJSON ShiftPm = toJSON ("pm" :: Text)
  toJSON ShiftFull = toJSON ("full-shift" :: Text)
  toJSON ShiftPmNight = toJSON ("pm-night" :: Text)
  toJSON ShiftPmAm = toJSON ("pm-am" :: Text)
  toJSON ShiftNightHalt = toJSON ("night-halt" :: Text)
  toJSON ShiftGeneral = toJSON ("general-shift" :: Text)

instance FromJSON ShiftType where
  parseJSON = withText "ShiftType" $ \case
    "am" -> pure ShiftAm
    "pm" -> pure ShiftPm
    "full-shift" -> pure ShiftFull
    "pm-night" -> pure ShiftPmNight
    "pm-am" -> pure ShiftPmAm
    "night-halt" -> pure ShiftNightHalt
    "general-shift" -> pure ShiftGeneral
    v -> fail $ "Unknown ShiftType: " <> T.unpack v

-- | Day types.
data DayType
  = DayWeekdays
  | DayWeekend
  | DayAlldays
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

instance ToJSON DayType where
  toJSON DayWeekdays = toJSON ("weekdays" :: Text)
  toJSON DayWeekend = toJSON ("weekend" :: Text)
  toJSON DayAlldays = toJSON ("alldays" :: Text)

instance FromJSON DayType where
  parseJSON = withText "DayType" $ \case
    "weekdays" -> pure DayWeekdays
    "weekend" -> pure DayWeekend
    "alldays" -> pure DayAlldays
    v -> fail $ "Unknown DayType: " <> T.unpack v

-- | Trip types.
data TripType
  = CutTrip
  | RegularTrip
  | DeadTrip
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

instance ToJSON TripType where
  toJSON CutTrip = toJSON ("cut-trip" :: Text)
  toJSON RegularTrip = toJSON ("regular-trip" :: Text)
  toJSON DeadTrip = toJSON ("dead-trip" :: Text)

instance FromJSON TripType where
  parseJSON = withText "TripType" $ \case
    "cut-trip" -> pure CutTrip
    "regular-trip" -> pure RegularTrip
    "dead-trip" -> pure DeadTrip
    v -> fail $ "Unknown TripType: " <> T.unpack v

-- | Break types.
data BreakType
  = NoBreak
  | FoodBreak
  | TeaBreak
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

instance ToJSON BreakType where
  toJSON NoBreak = toJSON ("no-break" :: Text)
  toJSON FoodBreak = toJSON ("food-break" :: Text)
  toJSON TeaBreak = toJSON ("tea-break" :: Text)

instance FromJSON BreakType where
  parseJSON = withText "BreakType" $ \case
    "no-break" -> pure NoBreak
    "food-break" -> pure FoodBreak
    "tea-break" -> pure TeaBreak
    v -> fail $ "Unknown BreakType: " <> T.unpack v

data NandiRouteRow = NandiRouteRow
  { route_id :: Int64,
    created_at :: Maybe UTCTime,
    description :: Maybe Text,
    route_direction :: Maybe Text,
    route_group :: Maybe Text,
    route_name :: Maybe Text,
    route_number :: Maybe Text,
    route_string :: Maybe Text,
    route_type_id :: Maybe Int,
    status :: Maybe Text,
    updated_at :: Maybe UTCTime,
    via :: Maybe Text,
    bus_service_type_id :: Int64,
    end_point_id :: Int64,
    start_point_id :: Int64,
    route_distance :: Maybe Double,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiRoutePointRow = NandiRoutePointRow
  { route_points_id :: Int64,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    fare_stage :: Maybe Text,
    point_status :: Maybe Text,
    route_order :: Int,
    stage_no :: Maybe Int,
    sub_stage :: Maybe Text,
    travel_distance :: Int,
    travel_time :: Maybe Text,
    updated_at :: Maybe UTCTime,
    bus_stop_id :: Int64,
    route_id :: Int64,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiBusScheduleRow = NandiBusScheduleRow
  { schedule_id :: Int64,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    effective_from :: Maybe UTCTime,
    effective_till :: Maybe UTCTime,
    route_code :: Maybe Text,
    schedule_number :: Maybe Text,
    service_code :: Maybe Text,
    service_type_code :: Maybe Text,
    schedule_type_code :: Maybe Text,
    status :: Maybe Text,
    updated_at :: Maybe UTCTime,
    entity_id :: Int64,
    route_id :: Int64,
    service_type_id :: Int64,
    schedule_type_id :: Int64,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiBusScheduleTripRow = NandiBusScheduleTripRow
  { schedule_trip_id :: Int64,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    effective_end_date :: Maybe UTCTime,
    effective_start_date :: Maybe UTCTime,
    no_trip :: Int,
    schedule_number_name :: Maybe Text,
    start_time :: Maybe Text,
    status :: Maybe Text,
    updated_at :: Maybe UTCTime,
    calendar_id :: Int64,
    schedule_id :: Int64,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiBusScheduleTripDetailRow = NandiBusScheduleTripDetailRow
  { schedule_trip_detail_id :: Int64,
    break_time :: Maybe Text,
    break_type :: Maybe BreakType,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    distance :: Double,
    end_time :: Maybe Text,
    org_name :: Maybe Text,
    running_time :: Maybe Text,
    schedule_number :: Maybe Text,
    shift_day_name :: Maybe Text,
    shift_type_name :: Maybe Text,
    start_time :: Maybe Text,
    trip_number :: Int,
    trip_order :: Int,
    trip_type :: Maybe TripType,
    updated_at :: Maybe UTCTime,
    calendar_id :: Int64,
    route_number_id :: Int64,
    schedule_trip_id :: Int64,
    is_active_trip :: Bool,
    trip_end_time :: Maybe Text,
    trip_start_time :: Maybe Text,
    sync_end_time :: Maybe Text,
    sync_start_time :: Maybe Text,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiBusScheduleTripFlexiRow = NandiBusScheduleTripFlexiRow
  { schedule_trip_flexi_id :: Int64,
    break_time :: Maybe Text,
    break_type :: Maybe BreakType,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    distance :: Double,
    end_time :: Maybe Text,
    org_name :: Maybe Text,
    running_time :: Maybe Text,
    schedule_number :: Maybe Text,
    shift_day_name :: Maybe Text,
    shift_type_name :: Maybe Text,
    start_time :: Maybe Text,
    trip_number :: Int,
    trip_order :: Int,
    trip_type :: Maybe TripType,
    updated_at :: Maybe UTCTime,
    calendar_id :: Int64,
    route_number_id :: Int64,
    schedule_trip_id :: Int64,
    waybill_id :: Int64,
    is_active_trip :: Bool,
    trip_end_time :: Maybe Text,
    trip_start_time :: Maybe Text,
    sync_end_time :: Maybe Text,
    sync_start_time :: Maybe Text,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiServiceTypeRow = NandiServiceTypeRow
  { service_type_id :: Int64,
    abbreviation :: Maybe Text,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    service_type_code :: Maybe Text,
    service_type_name :: Maybe Text,
    status :: Maybe Text,
    ticket_footer :: Maybe Text,
    ticket_footer_local_lang :: Maybe Text,
    updated_at :: Maybe UTCTime,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiStopRow = NandiStopRow
  { bus_stop_id :: Int64,
    bus_stop_code :: Maybe Text,
    bus_stop_name :: Maybe Text,
    bus_stop_name_local_lang :: Maybe Text,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    description :: Maybe Text,
    fare_stage :: Maybe Text,
    landmark :: Maybe Text,
    latitude_current :: Double,
    longitude_current :: Double,
    route_status :: Maybe Text,
    status :: Maybe Text,
    stop_direction :: Maybe Text,
    stop_group_id :: Maybe Int,
    stop_type_id :: Int,
    sub_stage :: Maybe Text,
    toll_fee :: Maybe Int,
    toll_zone :: Maybe Text,
    updated_at :: Maybe UTCTime,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiDesignationRow = NandiDesignationRow
  { designation_id :: Int64,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    designation_name :: Text,
    designation_remark :: Maybe Text,
    designation_status :: Text,
    is_default :: Maybe Int,
    updated_at :: Maybe UTCTime,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiEmployeeRow = NandiEmployeeRow
  { emp_id :: Int64,
    address :: Maybe Text,
    basic_amount :: Maybe Double,
    created_at :: Maybe UTCTime,
    da_amount :: Maybe Double,
    dob :: Maybe Day,
    deleted :: Bool,
    driving_license_expiry :: Maybe Text,
    driving_license_number :: Maybe Text,
    email :: Maybe Text,
    father_name :: Maybe Text,
    first_name :: Text,
    gender :: Maybe Text,
    last_name :: Maybe Text,
    mobile_no :: Maybe Text,
    status :: Maybe Text,
    token_no :: Maybe Text,
    updated_at :: Maybe UTCTime,
    week_off :: Maybe Text,
    department_id :: Int64,
    designation_id :: Int64,
    entity_id :: Int64,
    organization_id :: Int64,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiEntityRow = NandiEntityRow
  { entity_id :: Int64,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    entity_address :: Maybe Text,
    entity_contact :: Maybe Text,
    entity_email :: Maybe Text,
    entity_name :: Text,
    entity_name_local_lang :: Maybe Text,
    entity_remark :: Maybe Text,
    entity_status :: Text,
    updated_at :: Maybe UTCTime,
    organization_id :: Int64,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiVehicleRow = NandiVehicleRow
  { vehicle_id :: Int64,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    fleet_no :: Maybe Text,
    status :: Maybe Text,
    updated_at :: Maybe UTCTime,
    vehicle_no :: Maybe Text,
    bus_service_type_id :: Int64,
    entity_id :: Int64,
    organization_id :: Int64,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiWaybillDeviceRow = NandiWaybillDeviceRow
  { waybill_device_id :: Int64,
    created_at :: Maybe UTCTime,
    deleted :: Bool,
    device_serial_no :: Maybe Text,
    is_audited :: Maybe Bool,
    is_primary :: Maybe Bool,
    is_uploaded :: Maybe Bool,
    updated_at :: Maybe UTCTime,
    waybill_id :: Int64,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiFleetEtmMappingRow = NandiFleetEtmMappingRow
  { fleet_etm_mapping_id :: Int64,
    vehicle_no :: Text,
    gtfs_id :: Text,
    etm_serial_no :: Text,
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime,
    deleted :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiFleetObuMappingRow = NandiFleetObuMappingRow
  { fleet_obu_mapping_id :: Int64,
    vehicle_no :: Text,
    gtfs_id :: Text,
    obu_id :: Text,
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime,
    deleted :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiWaybillRow = NandiWaybillRow
  { waybill_id :: Int64,
    audited_date :: Maybe UTCTime,
    bag_master :: Maybe Text,
    challan_no :: Maybe Int64,
    conductor_name :: Maybe Text,
    conductor_token_no :: Maybe Text,
    created_at :: Maybe UTCTime,
    dc_name :: Maybe Text,
    dc_token_no :: Maybe Text,
    deleted :: Bool,
    driver_name :: Maybe Text,
    driver_token_no :: Maybe Text,
    duty_date :: Maybe Text,
    device_serial_number :: Maybe Text,
    is_flexi :: Bool,
    no_of_device :: Int,
    schedule_id :: Int64,
    schedule_no :: Maybe Text,
    schedule_trip_name :: Maybe Text,
    schedule_type :: Maybe Text,
    service_type :: Maybe Text,
    schedule_start_time :: Maybe Text,
    status :: Maybe WaybillStatus,
    updated_at :: Maybe UTCTime,
    vehicle_no :: Maybe Text,
    waybill_no :: Maybe Text,
    entity_id :: Int64,
    schedule_trip_id :: Int64,
    service_type_id :: Int64,
    shift_type_id :: Int64,
    tablet_id :: Maybe Text,
    gtfs_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

-- ─── NandiRow sum type ────────────────────────────────────────────────────────

-- | One row from any of the 16 operator DB tables.
-- The constructor identifies which table it came from.
data NandiRow
  = RowRoute NandiRouteRow
  | RowRoutePoint NandiRoutePointRow
  | RowBusSchedule NandiBusScheduleRow
  | RowBusScheduleTrip NandiBusScheduleTripRow
  | RowBusScheduleTripDetail NandiBusScheduleTripDetailRow
  | RowBusScheduleTripFlexi NandiBusScheduleTripFlexiRow
  | RowServiceType NandiServiceTypeRow
  | RowStop NandiStopRow
  | RowDesignation NandiDesignationRow
  | RowEmployee NandiEmployeeRow
  | RowEntity NandiEntityRow
  | RowVehicle NandiVehicleRow
  | RowWaybillDevice NandiWaybillDeviceRow
  | RowFleetEtmMapping NandiFleetEtmMappingRow
  | RowFleetObuMapping NandiFleetObuMappingRow
  | RowWaybill NandiWaybillRow
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

-- | Decode a raw Aeson Value into the correct NandiRow constructor
-- based on which table was queried.
decodeNandiRow :: NandiTable -> Value -> Either String NandiRow
decodeNandiRow RouteInternal v = RowRoute <$> parseEither parseJSON v
decodeNandiRow RoutePointInternal v = RowRoutePoint <$> parseEither parseJSON v
decodeNandiRow BusScheduleInternal v = RowBusSchedule <$> parseEither parseJSON v
decodeNandiRow BusScheduleTripInternal v = RowBusScheduleTrip <$> parseEither parseJSON v
decodeNandiRow BusScheduleTripDetailInternal v = RowBusScheduleTripDetail <$> parseEither parseJSON v
decodeNandiRow BusScheduleTripFlexiInternal v = RowBusScheduleTripFlexi <$> parseEither parseJSON v
decodeNandiRow ServiceTypeInternal v = RowServiceType <$> parseEither parseJSON v
decodeNandiRow StopInternal v = RowStop <$> parseEither parseJSON v
decodeNandiRow DesignationsInternal v = RowDesignation <$> parseEither parseJSON v
decodeNandiRow EmployeesInternal v = RowEmployee <$> parseEither parseJSON v
decodeNandiRow EntitiesInternal v = RowEntity <$> parseEither parseJSON v
decodeNandiRow VehiclesInternal v = RowVehicle <$> parseEither parseJSON v
decodeNandiRow WaybillDeviceInternal v = RowWaybillDevice <$> parseEither parseJSON v
decodeNandiRow FleetEtmMappingInternal v = RowFleetEtmMapping <$> parseEither parseJSON v
decodeNandiRow FleetObuMappingInternal v = RowFleetObuMapping <$> parseEither parseJSON v
decodeNandiRow WaybillsInternal v = RowWaybill <$> parseEither parseJSON v

newtype FilteredServiceSubTypes = FilteredServiceSubTypes [BecknV2.FRFS.Enums.ServiceSubType]
  deriving (Show)

instance FromJSON FilteredServiceSubTypes where
  parseJSON (Array values) = do
    -- Parse each element, collecting successes and ignoring failures
    let parseElement v = case fromJSON v of
          Success val -> Just val
          Error _ -> Nothing
    let parsed = catMaybes $ map parseElement $ toList values
    return $ FilteredServiceSubTypes parsed
  parseJSON _ = fail "Expected an array for service_sub_types"

-- Helper function to normalize FilteredServiceSubTypes to Maybe [ServiceSubType]
-- Converts empty lists to Nothing for cleaner API responses
normalizeServiceSubTypes :: Maybe FilteredServiceSubTypes -> Maybe [BecknV2.FRFS.Enums.ServiceSubType]
normalizeServiceSubTypes Nothing = Nothing
normalizeServiceSubTypes (Just (FilteredServiceSubTypes [])) = Nothing
normalizeServiceSubTypes (Just (FilteredServiceSubTypes subtypes)) = Just subtypes

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
    service_sub_types :: Maybe [BecknV2.FRFS.Enums.ServiceSubType],
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
    conductor_id :: Maybe Text,
    eligible_pass_ids :: Maybe [Text]
  }
  deriving (Generic, ToJSON, ToSchema, Show)

-- Custom FromJSON instance that filters unknown ServiceSubType values and normalizes empty lists to Nothing
instance FromJSON VehicleServiceTypeResponse where
  parseJSON = withObject "VehicleServiceTypeResponse" $ \v -> do
    service_type <- v .: "service_type"
    -- Parse with filtering, then normalize to public type
    raw_service_sub_types <- v .:? "service_sub_types" :: Parser (Maybe FilteredServiceSubTypes)
    let service_sub_types = normalizeServiceSubTypes raw_service_sub_types
    vehicle_no <- v .: "vehicle_no"
    last_updated <- v .:? "last_updated"
    schedule_no <- v .:? "schedule_no"
    trip_number <- v .:? "trip_number"
    route_id <- v .:? "route_id"
    waybill_id <- v .:? "waybill_id"
    route_number <- v .:? "route_number"
    depot <- v .:? "depot"
    remaining_trip_details <- v .:? "remaining_trip_details"
    is_actually_valid <- v .:? "is_actually_valid"
    driver_id <- v .:? "driver_id"
    conductor_id <- v .:? "conductor_id"
    eligible_pass_ids <- v .:? "eligible_pass_ids"
    return VehicleServiceTypeResponse {..}

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

data RowsAffectedResp = RowsAffectedResp
  { message :: Maybe Text,
    rows_affected :: Maybe Int64
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data ServiceType = ServiceType
  { service_type_id :: Int64,
    service_type_code :: Maybe Text,
    service_type_name :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiRoute = NandiRoute
  { route_id :: Int64,
    route_number :: Maybe Text,
    route_direction :: Maybe Text,
    start_point_id :: Int64,
    end_point_id :: Int64
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data Depot = Depot
  { entity_id :: Int64,
    entity_name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data ScheduleNumber = ScheduleNumber
  { schedule_id :: Int64,
    schedule_number :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data Fleet = Fleet
  { vehicle_id :: Int64,
    vehicle_no :: Maybe Text,
    fleet_no :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data Employee = Employee
  { emp_id :: Int64,
    first_name :: Text,
    last_name :: Maybe Text,
    token_no :: Maybe Text,
    mobile_no :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiTripDetail = NandiTripDetail
  { schedule_trip_detail_id :: Int64,
    trip_number :: Int,
    trip_order :: Int,
    trip_type :: Maybe TripType,
    start_time :: Maybe Text,
    end_time :: Maybe Text,
    break_time :: Maybe Text,
    break_type :: Maybe BreakType,
    shift_type :: Maybe ShiftType,
    distance :: Maybe Double,
    route_id :: Int64,
    schedule_trip_id :: Int64,
    is_active_trip :: Bool,
    entity_name :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data UpdateWaybillStatusReq = UpdateWaybillStatusReq
  { waybill_id :: Int64,
    status :: WaybillStatus
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets UpdateWaybillStatusReq where
  hideSecrets = identity

data UpdateWaybillFleetReq = UpdateWaybillFleetReq
  { waybill_id :: Int64,
    fleet_no :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets UpdateWaybillFleetReq where
  hideSecrets = identity

data UpdateWaybillTabletReq = UpdateWaybillTabletReq
  { waybill_id :: Int64,
    tablet_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

instance HideSecrets UpdateWaybillTabletReq where
  hideSecrets = identity
