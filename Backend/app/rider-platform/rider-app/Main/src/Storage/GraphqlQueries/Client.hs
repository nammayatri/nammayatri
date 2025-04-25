module Storage.GraphqlQueries.Client
  ( RouteStopTimeTableQueryVars (..),
    RouteStopTimeTableResponse (..),
    TimetableEntry (..),
    TripData (..),
    RouteData (..),
    StopData (..),
    OTPResponse (..),
    executeRouteStopTimeTableQuery,
  )
where

import qualified BecknV2.FRFS.Enums
import Data.Aeson (eitherDecode, encode, object)
import Data.Aeson.Types (Parser)
import Data.List (isInfixOf)
import Data.Morpheus.Client
import Data.Morpheus.Client.CodeGen.Internal
import Data.Text as Text (drop, length, splitOn, take, unpack)
import Data.Time.Calendar
import qualified Data.Time.LocalTime as LocalTime
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Network.HTTP.Client
import Network.HTTP.Types (statusCode)

-- Data types for RouteStopTimeTable query
data RouteStopTimeTableQueryVars = RouteStopTimeTableQueryVars
  { routeCode :: [Text],
    stopCode :: Text
  }
  deriving (Show, Generic)

-- Response structure from OpenTripPlanner
newtype OTPResponse = OTPResponse
  { stop :: StopData
  }
  deriving (Show, Generic)

data StopData = StopData
  { gtfsId :: Text,
    name :: Text,
    stoptimesWithoutPatterns :: [RouteStopTimeTableEntry]
  }
  deriving (Show, Generic)

data RouteStopTimeTableEntry = RouteStopTimeTableEntry
  { scheduledArrival :: Int,
    realtimeArrival :: Int,
    arrivalDelay :: Int,
    scheduledDeparture :: Int,
    headsign :: Maybe Text,
    trip :: TripData
  }
  deriving (Show, Generic)

data TripData = TripData
  { serviceId :: Text,
    tripShortName :: Maybe Text,
    gtfsId :: Text,
    route :: RouteData,
    activeDates :: [Text]
  }
  deriving (Show, Generic)

newtype RouteData = RouteData
  { gtfsId :: Text
  }
  deriving (Show, Generic)

-- Our transformed response structure
newtype RouteStopTimeTableResponse = RouteStopTimeTableResponse
  { routeStopTimeTables :: [TimetableEntry]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- Transformed entry for our domain model
data TimetableEntry = TimetableEntry
  { routeCode :: Text,
    serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType,
    stopCode :: Text,
    timeOfArrival :: LocalTime.TimeOfDay,
    timeOfDeparture :: LocalTime.TimeOfDay,
    tripId :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    serviceability :: [Int]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- JSON instances for OpenTripPlanner response
instance FromJSON OTPResponse where
  parseJSON = withObject "OTPResponse" $ \obj -> do
    dataObj <- obj .: "data"
    OTPResponse <$> dataObj .: "stop"

instance FromJSON StopData where
  parseJSON = withObject "StopData" $ \obj -> do
    StopData
      <$> obj .: "gtfsId"
      <*> obj .: "name"
      <*> obj .: "stoptimesWithoutPatterns"

instance FromJSON RouteStopTimeTableEntry where
  parseJSON = withObject "RouteStopTimeTableEntry" $ \obj -> do
    RouteStopTimeTableEntry
      <$> obj .: "scheduledArrival"
      <*> obj .: "realtimeArrival"
      <*> obj .: "arrivalDelay"
      <*> obj .: "scheduledDeparture"
      <*> obj .:? "headsign"
      <*> obj .: "trip"

instance FromJSON TripData where
  parseJSON = withObject "TripData" $ \obj -> do
    TripData
      <$> obj .: "serviceId"
      <*> obj .:? "tripShortName"
      <*> obj .: "gtfsId"
      <*> obj .: "route"
      <*> obj .:? "activeDates" .!= []
    where
      (.!=) :: Parser (Maybe a) -> a -> Parser a
      parser .!= def = fromMaybe def <$> parser

instance FromJSON RouteData where
  parseJSON = withObject "RouteData" $ \obj -> do
    RouteData <$> obj .: "gtfsId"

-- Define GraphQL operation using RequestType approach instead of gql quasiquoter
data RouteStopTimeTableQuery

instance RequestType RouteStopTimeTableQuery where
  type RequestArgs RouteStopTimeTableQuery = RouteStopTimeTableQueryVars
  __name _ = "RouteStopTimeTableQuery"
  __query _ =
    "query RouteStopTimeTableQuery($stopId: String!) {\n"
      ++ "  stop(id: $stopId) {\n"
      ++ "    gtfsId\n"
      ++ "    name\n"
      ++ "    stoptimesWithoutPatterns {\n"
      ++ "      scheduledArrival\n"
      ++ "      realtimeArrival\n"
      ++ "      arrivalDelay\n"
      ++ "      scheduledDeparture\n"
      ++ "      headsign\n"
      ++ "      trip {\n"
      ++ "        serviceId\n"
      ++ "        tripShortName\n"
      ++ "        activeDates\n"
      ++ "        gtfsId\n"
      ++ "        route {\n"
      ++ "          gtfsId\n"
      ++ "        }\n"
      ++ "      }\n"
      ++ "    }\n"
      ++ "  }\n"
      ++ "}"
  __type _ = OPERATION_QUERY

-- Execute the query and transform the response
executeRouteStopTimeTableQuery ::
  MonadFlow m =>
  BaseUrl ->
  RouteStopTimeTableQueryVars ->
  m (Either String RouteStopTimeTableResponse)
executeRouteStopTimeTableQuery baseUrl vars = do
  let graphqlUrl = showBaseUrl baseUrl <> "/otp/gtfs/v1"

  logInfo $ "Executing RouteStopTimeTable GraphQL query to " <> graphqlUrl

  manager <- liftIO $ newManager defaultManagerSettings
  initialRequest <-
    try (parseRequest (toString graphqlUrl))
      >>= \case
        Left (err :: SomeException) -> do
          logError $ "Failed to parse request URL: " <> show err
          throwError $ InternalError $ "Failed to parse request URL: " <> show err
        Right req -> pure req

  -- Create the request body manually instead of using Morpheus Client's request function
  let reqBody =
        encode $
          object
            [ "query" .= __query (Kernel.Prelude.Proxy @RouteStopTimeTableQuery),
              "variables"
                .= object
                  [ "stopId" .= vars.stopCode
                  ]
            ]

  let req =
        initialRequest
          { method = "POST",
            requestHeaders = [("Content-Type", "application/json")],
            requestBody = RequestBodyLBS reqBody
          }

  response <- liftIO $ try $ httpLbs req manager

  case response of
    Left (err :: SomeException) -> do
      logError $ "HTTP request failed: " <> show err
      pure $ Left $ "HTTP request error: " <> show err
    Right res -> do
      let status = statusCode (responseStatus res)
      let respBody = responseBody res

      if status >= 200 && status < 300
        then case eitherDecode respBody of
          Left decodeErr -> do
            logError $ "Failed to parse GraphQL response: " <> show decodeErr
            logError $ "Response body: " <> show respBody
            pure $ Left $ "JSON parse error: " <> decodeErr
          Right (otpResponse :: OTPResponse) -> do
            -- Transform OTP response to our domain model
            entries <- transformToTimeTableEntries otpResponse
            pure $ Right $ RouteStopTimeTableResponse entries
        else pure $ Left $ "HTTP error: " <> show status

-- Helper function to convert OTP response to our domain model
transformToTimeTableEntries :: MonadFlow m => OTPResponse -> m [TimetableEntry]
transformToTimeTableEntries otpResponse = do
  now <- getCurrentTime
  return $ map (transformEntry otpResponse.stop now) otpResponse.stop.stoptimesWithoutPatterns

transformEntry :: StopData -> UTCTime -> RouteStopTimeTableEntry -> TimetableEntry
transformEntry stopData timestamp entry =
  TimetableEntry
    { routeCode = fromMaybe entry.trip.route.gtfsId $ lastMay $ splitOn ":" entry.trip.route.gtfsId,
      serviceTierType = mapToServiceTierType entry.trip.gtfsId,
      stopCode = fromMaybe stopData.gtfsId $ lastMay $ splitOn ":" stopData.gtfsId,
      -- Convert seconds from midnight to HH:MM:SS
      timeOfArrival = secondsToTime entry.scheduledArrival,
      timeOfDeparture = secondsToTime entry.scheduledDeparture,
      tripId = fromMaybe entry.trip.gtfsId $ lastMay $ splitOn ":" entry.trip.gtfsId,
      createdAt = timestamp,
      updatedAt = timestamp,
      serviceability = calculateServiceability entry.trip.activeDates
    }

-- Convert seconds from midnight to HH:MM:SS format
secondsToTime :: Int -> LocalTime.TimeOfDay
secondsToTime seconds =
  let hours :: Int = seconds `div` 3600
      minutes :: Int = (seconds `mod` 3600) `div` 60
      secs = fromIntegral $ seconds `mod` 60
   in LocalTime.TimeOfDay hours minutes secs

-- Map route codes to service tier types
mapToServiceTierType :: Text -> BecknV2.FRFS.Enums.ServiceTierType
mapToServiceTierType routeCode =
  let serviceCode = extractServiceCode routeCode
      serviceTierMapping =
        [ ("Z", BecknV2.FRFS.Enums.AC),
          ("XS", BecknV2.FRFS.Enums.SPECIAL),
          ("OS", BecknV2.FRFS.Enums.NON_AC),
          ("S", BecknV2.FRFS.Enums.EXECUTIVE),
          ("X", BecknV2.FRFS.Enums.EXPRESS),
          ("O", BecknV2.FRFS.Enums.ORDINARY)
        ]
   in fromMaybe BecknV2.FRFS.Enums.NON_AC $ lookup serviceCode serviceTierMapping

extractServiceCode :: Text -> Text
extractServiceCode routeCode = maybe "O" snd (find match patterns)
  where
    patterns =
      [ ("-Z-", "Z"),
        ("-XS-", "XS"),
        ("-OS-", "OS"),
        ("-S-", "S"),
        ("-X-", "X"),
        ("-O-", "O")
      ]
    match (pat, _) = pat `isInfixOf` (unpack routeCode)

-- Calculate serviceability array from active dates
-- Returns an array of length 7, with 1s for days the service runs and 0s for days it doesn't
-- Index 0 = Monday, 1 = Tuesday, ..., 6 = Sunday
calculateServiceability :: [Text] -> [Int]
calculateServiceability activeDates =
  let defaultServiceability = replicate 7 0 -- Initialize with all zeroes
      activeDaysOfWeek = mapMaybe getWeekdayFromDate activeDates
      incrementDays = foldl' (\acc day -> updateAt day 1 acc) defaultServiceability activeDaysOfWeek
   in incrementDays

-- Update the value at a specific index in a list
updateAt :: Int -> a -> [a] -> [a]
updateAt idx newVal list =
  let (before, after) = splitAt idx list
   in case after of
        [] -> before
        _ : xs -> before ++ [newVal] ++ xs

-- Parse a date string (YYYYMMDD format) to day of week (0 = Monday, 6 = Sunday)
getWeekdayFromDate :: Text -> Maybe Int
getWeekdayFromDate dateStr | Text.length dateStr /= 8 = Nothing
getWeekdayFromDate dateStr = do
  let yearStr = Text.take 4 dateStr
      monthStr = Text.take 2 $ Text.drop 4 dateStr
      dayStr = Text.take 2 $ Text.drop 6 dateStr
  year <- readMaybe (toString yearStr)
  month <- readMaybe (toString monthStr)
  day <- readMaybe (toString dayStr)
  let date = fromGregorian year month day
      dow = dayOfWeek date
      dowInt = dayOfWeekToInt dow
  Just dowInt

-- Convert Day of week to Int (0 = Monday, 6 = Sunday)
dayOfWeekToInt :: DayOfWeek -> Int
dayOfWeekToInt = \case
  Monday -> 0
  Tuesday -> 1
  Wednesday -> 2
  Thursday -> 3
  Friday -> 4
  Saturday -> 5
  Sunday -> 6
