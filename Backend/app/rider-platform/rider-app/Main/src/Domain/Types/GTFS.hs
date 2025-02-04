module Domain.Types.GTFS where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Csv
import qualified Data.HashMap.Lazy as HM
import Data.List.Split
import Data.Ord (comparing)
import Kernel.Prelude

data LocationType
  = LocStop
  | LocStation
  deriving (Show, Enum, Bounded, Eq, Ord)

data RouteType
  = Tram
  | Metro
  | Rail
  | Bus
  | Ferry
  | CableCar
  | Gondola
  | Funicular
  deriving (Show, Enum, Bounded, Eq, Ord)

data DirectionID
  = DirectionA
  | DirectionB
  deriving (Show, Enum, Bounded, Eq, Ord)

data OnOffType
  = RegularlyScheduled
  | NotAvailable
  | MustPhone
  | MustAskDriver
  deriving (Show, Enum, Bounded, Eq, Ord)

data ServiceFlag
  = NoService
  | HasService
  deriving (Show, Enum, Bounded, Eq, Ord)

data ExceptionType
  = NoException
  | ServiceAdded
  | ServiceRemoved
  deriving (Show, Enum, Bounded, Eq, Ord)

data PaymentMethod
  = PayOnBoard
  | PayBeforeBoarding
  deriving (Show, Enum, Bounded, Eq, Ord)

data TransferType
  = RecommendedTransfer
  | TimedTransfer
  | MinimumTransfer
  | NoTransfer
  deriving (Show, Enum, Bounded, Eq, Ord)

data WheelchairAccessibility
  = SomeWheelchairAccessibility
  | NoWheelchairAccessibility
  deriving (Show, Enum, Bounded, Eq, Ord)

data BikesAllowed
  = SomeBikesAllowed
  | NoBikesAllowed
  deriving (Show, Enum, Bounded, Eq, Ord)

data Timepoint
  = ApproximateTimepoint
  | ExactTimepoint
  deriving (Show, Enum, Bounded, Eq, Ord)

addGeoJSON :: Stop -> Stop
addGeoJSON stop = case s_location_type stop of
  Just LocStation
    | isNothing (s_geoJSON stop) ->
      stop {s_geoJSON = Just (generateDummyGeoJSON stop)}
  _ -> stop

data GeoJSON = GeoJSON
  { gjType :: Text,
    gjCoordinates :: [(Double, Double)]
  }
  deriving (Show, Eq)

instance FromField GeoJSON where
  parseField bs = case A.parseOnly geoJsonParser bs of
    Left err -> fail $ "Invalid GeoJSON: " ++ err
    Right gj -> pure gj
    where
      geoJsonParser :: A.Parser GeoJSON
      geoJsonParser = do
        parts <- parseAndSplit
        case parts of
          [] -> fail "Empty GeoJSON field"
          (typPart : coordParts) -> do
            let typ = decodeUtf8 typPart -- Ensure you have Text/UTF8 handling
                coords = map parseCoord coordParts
            case sequence coords of
              Just pairs -> pure $ GeoJSON typ pairs
              Nothing -> fail "Invalid coordinate format"

      parseAndSplit :: A.Parser [BS.ByteString]
      parseAndSplit = do
        str <- A.takeByteString
        pure $ BS.split ';' str

      parseCoord :: ByteString -> Maybe (Double, Double)
      parseCoord s = case A.parseOnly ((,) <$> AC.double <* AC.char ',' <*> AC.double) s of
        Right (x, y) -> Just (x, y)
        Left _ -> Nothing

validateMetro :: Feed -> Either Text MetroGTFS
validateMetro feed = do
  let routes = f_routes feed
  unless (all (isJust . r_route_color) routes) $
    Left "Metro routes must have a route_color!"
  let stopTimes = f_stop_times feed
  when (null stopTimes) $
    Left "Metro GTFS requires stop_times.txt!"
  let updatedStops = map ensureStationGeoJSON (f_stops feed)
      updatedFeed = feed {f_stops = updatedStops}
  let updatedRoutes = map ensurePolyline (f_routes updatedFeed)
      finalFeed = updatedFeed {f_routes = updatedRoutes}

  pure $ MetroGTFS finalFeed (deriveServiceabilityTime stopTimes)

ensureStationGeoJSON :: Stop -> Stop
ensureStationGeoJSON stop =
  case s_location_type stop of
    Just LocStation
      | isNothing (s_geoJSON stop) ->
        stop {s_geoJSON = Just (generateDummyGeoJSON stop)}
    _ -> stop

generateDummyGeoJSON :: Stop -> GeoJSON
generateDummyGeoJSON stop =
  GeoJSON
    { gjType = "Point",
      gjCoordinates = [(s_stop_lat stop, s_stop_lon stop)]
    }

ensurePolyline :: Route -> Route
ensurePolyline route
  | isNothing (r_polyline route) =
    route {r_polyline = Just ("OSRM-GENERATED-POLYLINE-FOR-" <> r_route_id route)}
  | otherwise = route

deriveServiceabilityTime :: [StopTime] -> ServiceabilityTime
deriveServiceabilityTime stopTimes =
  let arrivalTimes = map st_arrival_time stopTimes
      departureTimes = map st_departure_time stopTimes
      earliestArrival = minimumBy (comparing timeToSeconds) arrivalTimes
      latestDeparture = maximumBy (comparing timeToSeconds) departureTimes
   in ServiceabilityTime earliestArrival latestDeparture

timeToSeconds :: Time -> Int
timeToSeconds (Time h m s) = h * 3600 + m * 60 + s

data MetroGTFS = MetroGTFS
  { mgFeed :: Feed, -- Reuse your existing Feed type
    mgServiceabilityTime :: ServiceabilityTime -- Booking window from stop_times.txt
  }
  deriving (Show)

validateBus :: Feed -> Either Text BusGTFS
validateBus feed = do
  let hasStopTimes = not (null (f_stop_times feed))
      hasFrequencies = not (null (f_frequencies feed))
  unless (hasStopTimes || hasFrequencies) $
    Left "Bus GTFS requires stop_times.txt or frequency.txt!"
  Right $ BusGTFS feed (not hasStopTimes)

data BusGTFS = BusGTFS
  { bgFeed :: Feed,
    bgRequiresFrequencies :: Bool
  }
  deriving (Show)

data ServiceabilityTime = ServiceabilityTime
  { frfsStartTime :: Time,
    frfsEndTime :: Time
  }
  deriving (Show)

data Agency = Agency
  { a_agency_id :: Maybe AgencyID,
    a_agency_name :: Text,
    a_agency_url :: URL,
    a_agency_timezone :: Timezone,
    a_agency_lang :: Maybe Language,
    a_agency_phone :: Maybe Phone,
    a_agency_fare_url :: Maybe URL,
    a_agency_email :: Maybe Email
  }
  deriving (Show)

data Stop = Stop
  { s_stop_id :: StopID,
    s_stop_code :: Maybe Text,
    s_stop_name :: Text,
    s_stop_desc :: Maybe Text,
    s_stop_lat :: LatLon,
    s_stop_lon :: LatLon,
    s_zone_id :: Maybe ZoneID,
    s_stop_url :: Maybe URL,
    s_location_type :: Maybe LocationType,
    s_parent_station :: Maybe StopID,
    s_stop_timezone :: Maybe Timezone,
    s_wheelchair_boarding :: Maybe WheelchairAccessibility,
    s_geoJSON :: Maybe GeoJSON -- Added: GeoJSON for stations
  }
  deriving (Show)

data Route = Route
  { r_route_id :: RouteID,
    r_agency_id :: Maybe AgencyID,
    r_route_short_name :: Text,
    r_route_long_name :: Text,
    r_route_desc :: Maybe Text,
    r_route_type :: RouteType,
    r_route_url :: Maybe URL,
    r_route_color :: Maybe Color, -- Mandatory for MetroGTFS
    r_route_text_color :: Maybe Color,
    r_route_sort_order :: Maybe Integer,
    r_polyline :: Maybe Text -- Added: Optional polyline for Metro
  }
  deriving (Show)

data Trip = Trip
  { t_route_id :: RouteID,
    t_service_id :: ServiceID,
    t_trip_id :: TripID,
    t_trip_headsign :: Maybe Text,
    t_trip_short_name :: Maybe Text,
    t_direction_id :: Maybe DirectionID,
    t_block_id :: Maybe BlockID,
    t_shape_id :: Maybe ShapeID,
    t_wheelchair_accessible :: Maybe WheelchairAccessibility,
    t_bikes_allowed :: Maybe BikesAllowed
  }
  deriving (Show)

data StopTime = StopTime
  { st_trip_id :: TripID,
    st_arrival_time :: Time,
    st_departure_time :: Time,
    st_stop_id :: StopID,
    st_stop_sequence :: Sequence,
    st_stop_headsign :: Maybe Text,
    st_pickup_type :: Maybe OnOffType,
    st_drop_off_type :: Maybe OnOffType,
    st_shape_dist_traveled :: Maybe Distance,
    st_timepoint :: Maybe Timepoint
  }
  deriving (Show)

data Calendar = Calendar
  { c_service_id :: ServiceID,
    c_monday :: ServiceFlag,
    c_tuesday :: ServiceFlag,
    c_wednesday :: ServiceFlag,
    c_thursday :: ServiceFlag,
    c_friday :: ServiceFlag,
    c_saturday :: ServiceFlag,
    c_sunday :: ServiceFlag,
    c_start_date :: Date,
    c_end_date :: Date
  }
  deriving (Show)

data CalendarDate = CalendarDate
  { cd_service_id :: ServiceID,
    cd_date :: Date,
    cd_exception_type :: ExceptionType
  }
  deriving (Show)

data FareAttribute = FareAttribute
  { fa_fare_id :: FareID,
    fa_price :: Price,
    fa_currency_type :: Currency,
    fa_payment_method :: PaymentMethod,
    fa_transfers :: TransferLimit,
    fa_agency_id :: Maybe AgencyID,
    fa_transfer_duration :: Maybe Seconds
  }
  deriving (Show)

data FareRule = FareRule
  { fr_fare_id :: FareID,
    fr_route_id :: Maybe RouteID,
    fr_origin_id :: Maybe ZoneID,
    fr_destination_id :: Maybe ZoneID,
    fr_contains_id :: Maybe ZoneID
  }
  deriving (Show)

data Shape = Shape
  { sh_shape_id :: ShapeID,
    sh_shape_pt_lat :: LatLon,
    sh_shape_pt_lon :: LatLon,
    sh_shape_pt_sequence :: Sequence,
    sh_shape_dist_traveled :: Maybe Distance
  }
  deriving (Show)

data Frequency = Frequency
  { fq_trip_id :: TripID,
    fq_start_time :: Time,
    fq_end_time :: Time,
    fq_headway_secs :: Seconds,
    fq_exact_times :: Maybe Timepoint
  }
  deriving (Show)

data Transfer = Transfer
  { x_from_stop_id :: StopID,
    x_to_stop_id :: StopID,
    x_transfer_type :: TransferType,
    x_min_transfer_time :: Maybe Seconds
  }
  deriving (Show)

data FeedInfo = FeedInfo
  { fi_feed_publisher_name :: Text,
    fi_feed_publisher_url :: URL,
    fi_feed_lang :: Language,
    fi_feed_start_date :: Maybe Date,
    fi_feed_end_date :: Maybe Date,
    fi_feed_version :: Maybe Text
  }
  deriving (Show)

data Feed = Feed
  { f_agency :: [Agency],
    f_stops :: [Stop],
    f_routes :: [Route],
    f_trips :: [Trip],
    f_stop_times :: [StopTime],
    f_calendar :: [Calendar],
    f_calendar_dates :: [CalendarDate],
    f_fare_attributes :: [FareAttribute],
    f_fare_rules :: [FareRule],
    f_shapes :: [Shape],
    f_frequencies :: [Frequency],
    f_transfers :: [Transfer],
    f_feed_infos :: [FeedInfo]
  }
  deriving (Show)

data Date = Date Int Int Int
  deriving (Show)

data Time = Time Int Int Int
  deriving (Show)

type AgencyID = Text

type BlockID = Text

type FareID = Text

type RouteID = Text

type ServiceID = Text

type ShapeID = Text

type StopID = Text

type TripID = Text

type ZoneID = Text

type URL = Text

type Email = Text

type Phone = Text

type Timezone = Text

type Language = Text

type Currency = Text

type Color = Text

type Sequence = Int

type Seconds = Int

type Distance = Double

type Price = Double

type LatLon = Double

type TransferLimit = Maybe Int

enumParseField :: forall a. (Enum a, Bounded a) => ByteString -> Parser a
enumParseField xs = parseField xs >>= conv
  where
    mx = fromEnum (maxBound :: a)
    conv n
      | n > mx = fail ("out of range: " ++ show n)
      | otherwise = pure (toEnum n)

instance FromField LocationType where parseField = enumParseField

instance FromField RouteType where parseField = enumParseField

instance FromField DirectionID where parseField = enumParseField

instance FromField OnOffType where parseField = enumParseField

instance FromField ServiceFlag where parseField = enumParseField

instance FromField ExceptionType where parseField = enumParseField

instance FromField PaymentMethod where parseField = enumParseField

instance FromField TransferType where parseField = enumParseField

instance FromField Timepoint where parseField = enumParseField

instance FromField WheelchairAccessibility where parseField = enumParseField

instance FromField BikesAllowed where parseField = enumParseField

instance FromField Date where
  parseField x = parseField x >>= f
    where
      f (splitAt 4 -> (ys, splitAt 2 -> (ms, ds)))
        | length ds == 2,
          Just [y, m, d] <- mapM readMay [ys, ms, ds] =
          pure $ Date y m d
      f d = fail $ "invalid date: " ++ d

instance FromField Time where
  parseField x = parseField x >>= f
    where
      f (mapM readMay . splitOn ":" -> Just [h, m, s]) =
        pure $ Time h m s
      f s = fail $ "invalid time: " ++ s

(.:?) :: FromField a => NamedRecord -> ByteString -> Parser (Maybe a)
(.:?) m name = maybe (pure Nothing) parseField $ HM.lookup name m

instance FromNamedRecord Agency where
  parseNamedRecord m =
    Agency
      <$> m .:? "agency_id"
      <*> m .: "agency_name"
      <*> m .: "agency_url"
      <*> m .: "agency_timezone"
      <*> m .:? "agency_lang"
      <*> m .:? "agency_phone"
      <*> m .:? "agency_fare_url"
      <*> m .:? "agency_email"

instance FromNamedRecord Stop where
  parseNamedRecord m =
    Stop
      <$> m .: "stop_id"
      <*> m .:? "stop_code"
      <*> m .: "stop_name"
      <*> m .:? "stop_desc"
      <*> m .: "stop_lat"
      <*> m .: "stop_lon"
      <*> m .:? "zone_id"
      <*> m .:? "stop_url"
      <*> m .:? "location_type"
      <*> m .:? "parent_station"
      <*> m .:? "stop_timezone"
      <*> m .:? "wheelchair_boarding"
      <*> m .:? "geoJSON"

instance FromNamedRecord Route where
  parseNamedRecord m =
    Route
      <$> m .: "route_id"
      <*> m .:? "agency_id"
      <*> m .: "route_short_name"
      <*> m .: "route_long_name"
      <*> m .:? "route_desc"
      <*> m .: "route_type"
      <*> m .:? "route_url"
      <*> m .:? "route_color"
      <*> m .:? "route_text_color"
      <*> m .:? "route_sort_order"
      <*> m .:? "polyline"

instance FromNamedRecord Trip where
  parseNamedRecord m =
    Trip
      <$> m .: "route_id"
      <*> m .: "service_id"
      <*> m .: "trip_id"
      <*> m .:? "trip_headsign"
      <*> m .:? "trip_short_name"
      <*> m .:? "direction_id"
      <*> m .:? "block_id"
      <*> m .:? "shape_id"
      <*> m .:? "wheelchair_accessible"
      <*> m .:? "bikes_allowed"

instance FromNamedRecord StopTime where
  parseNamedRecord m =
    StopTime
      <$> m .: "trip_id"
      <*> m .: "arrival_time"
      <*> m .: "departure_time"
      <*> m .: "stop_id"
      <*> m .: "stop_sequence"
      <*> m .:? "stop_headsign"
      <*> m .:? "pickup_type"
      <*> m .:? "drop_off_type"
      <*> m .:? "shape_dist_travelled"
      <*> m .:? "timepoint"

instance FromNamedRecord Calendar where
  parseNamedRecord m =
    Calendar
      <$> m .: "service_id"
      <*> m .: "monday"
      <*> m .: "tuesday"
      <*> m .: "wednesday"
      <*> m .: "thursday"
      <*> m .: "friday"
      <*> m .: "saturday"
      <*> m .: "sunday"
      <*> m .: "start_date"
      <*> m .: "end_date"

instance FromNamedRecord CalendarDate where
  parseNamedRecord m =
    CalendarDate
      <$> m .: "service_id"
      <*> m .: "date"
      <*> m .: "exception_type"

instance FromNamedRecord FareAttribute where
  parseNamedRecord m =
    FareAttribute
      <$> m .: "fare_id"
      <*> m .: "price"
      <*> m .: "currency_type"
      <*> m .: "payment_method"
      <*> m .: "transfers"
      <*> m .:? "agency_id"
      <*> m .:? "transfer_duration"

instance FromNamedRecord FareRule where
  parseNamedRecord m =
    FareRule
      <$> m .: "fare_id"
      <*> m .:? "route_id"
      <*> m .:? "origin_id"
      <*> m .:? "destination_id"
      <*> m .:? "contains_id"

instance FromNamedRecord Shape where
  parseNamedRecord m =
    Shape
      <$> m .: "shape_id"
      <*> m .: "shape_pt_lat"
      <*> m .: "shape_pt_lon"
      <*> m .: "shape_pt_sequence"
      <*> m .:? "shape_dist_travelled"

instance FromNamedRecord Frequency where
  parseNamedRecord m =
    Frequency
      <$> m .: "trip_id"
      <*> m .: "start_time"
      <*> m .: "end_time"
      <*> m .: "headway_secs"
      <*> m .:? "exact_times"

instance FromNamedRecord Transfer where
  parseNamedRecord m =
    Transfer
      <$> m .: "from_stop_id"
      <*> m .: "to_stop_id"
      <*> m .: "transfer_type"
      <*> m .:? "min_transfer_time"

instance FromNamedRecord FeedInfo where
  parseNamedRecord m =
    FeedInfo
      <$> m .: "feed_publisher_name"
      <*> m .: "feed_publisher_url"
      <*> m .: "feed_lang"
      <*> m .:? "feed_start_date"
      <*> m .:? "feed_end_date"
      <*> m .:? "feed_version"
