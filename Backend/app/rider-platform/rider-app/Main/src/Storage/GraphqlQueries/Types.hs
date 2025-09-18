{-
 Copyright 2025-26, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.GraphqlQueries.Types where

import qualified BecknV2.FRFS.Enums
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Morpheus.Client.CodeGen.Internal
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.LocalTime as LocalTime
import EulerHS.Types (OptionEntity)
import Kernel.Prelude
import qualified Kernel.Types.Time
import Network.HTTP.Client

-- Data types for RouteStopTimeTable query
data RouteStopTimeTableQueryVars = RouteStopTimeTableQueryVars
  { routeIds :: [Text],
    stopId :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Response structure from OpenTripPlanner
newtype OTPResponse = OTPResponse
  { stop :: StopData
  }
  deriving (Show, Generic)

-- JSON instances for OpenTripPlanner response
instance FromJSON OTPResponse where
  parseJSON = withObject "OTPResponse" $ \obj -> do
    dataObj <- obj .: "data"
    OTPResponse <$> dataObj .: "stop"

newtype PlatformCode = PlatformCode
  { platformCode :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON PlatformCode where
  parseJSON = withObject "PlatformCode" $ \obj -> do
    PlatformCode <$> obj .: "platformCode"

data StopData = StopData
  { gtfsId :: Text,
    name :: Text,
    stoptimesWithoutPatterns :: [RouteStopTimeTableEntry]
  }
  deriving (Show, Generic)

instance FromJSON StopData where
  parseJSON = withObject "StopData" $ \obj -> do
    StopData
      <$> obj .: "gtfsId"
      <*> obj .: "name"
      <*> obj .: "stoptimesWithoutPatterns"

data ExtraInfo = ExtraInfo
  { fareStageNumber :: Maybe Text,
    providerStopCode :: Maybe Text,
    isStageStop :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON)

data RouteStopTimeTableEntry = RouteStopTimeTableEntry
  { scheduledArrival :: Int,
    realtimeArrival :: Int,
    arrivalDelay :: Int,
    scheduledDeparture :: Int,
    realtimeDeparture :: Int,
    extraInfo :: Maybe ExtraInfo,
    trip :: TripData,
    stop :: Maybe PlatformCode
  }
  deriving (Show, Generic)

-- Replace single quotes with double quotes
sanitizeJsonQuotes :: Text -> Text
sanitizeJsonQuotes = T.replace "'" "\""

instance FromJSON RouteStopTimeTableEntry where
  parseJSON = withObject "RouteStopTimeTableEntry" $ \obj -> do
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

    RouteStopTimeTableEntry
      <$> obj .: "scheduledArrival"
      <*> obj .: "realtimeArrival"
      <*> obj .: "arrivalDelay"
      <*> obj .: "scheduledDeparture"
      <*> obj .: "realtimeDeparture"
      <*> pure headsignParser
      <*> obj .: "trip"
      <*> obj .:? "stop"

data TripData = TripData
  { serviceId :: Text,
    tripShortName :: Maybe Text,
    gtfsId :: Text,
    route :: RouteData
  }
  deriving (Show, Generic)

instance FromJSON TripData where
  parseJSON = withObject "TripData" $ \obj -> do
    TripData
      <$> obj .: "serviceId"
      <*> obj .:? "tripShortName"
      <*> obj .: "gtfsId"
      <*> obj .: "route"

newtype RouteData = RouteData
  { gtfsId :: Text
  }
  deriving (Show, Generic)

instance FromJSON RouteData where
  parseJSON = withObject "RouteData" $ \obj -> do
    RouteData <$> obj .: "gtfsId"

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
    stage :: Maybe Int,
    providerStopCode :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    platformCode :: Maybe Text,
    isStageStop :: Maybe Bool,
    arrivalDelay :: Kernel.Types.Time.Seconds
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance RequestType RouteStopTimeTableQuery where
  type RequestArgs RouteStopTimeTableQuery = RouteStopTimeTableQueryVars
  __name _ = "RouteStopTimeTableQuery"
  __query _ =
    "query RouteStopTimeTableQuery($stopId: String!) {\n"
      ++ "  stop(id: $stopId) {\n"
      ++ "    gtfsId\n"
      ++ "    name\n"
      ++ "    stoptimesWithoutPatterns(numberOfDepartures: 10000) {\n"
      ++ "      scheduledArrival\n"
      ++ "      realtimeArrival\n"
      ++ "      arrivalDelay\n"
      ++ "      scheduledDeparture\n"
      ++ "      realtimeDeparture\n"
      ++ "      headsign\n"
      ++ "      trip {\n"
      ++ "        serviceId\n"
      ++ "        tripShortName\n"
      ++ "        gtfsId\n"
      ++ "        route {\n"
      ++ "          gtfsId\n"
      ++ "        }\n"
      ++ "      }\n"
      ++ "      stop {\n"
      ++ "        platformCode\n"
      ++ "      }\n"
      ++ "    }\n"
      ++ "  }\n"
      ++ "}"
  __type _ = OPERATION_QUERY

data RouteStopTimeTableQuery

data HttpManager = HttpManager
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity HttpManager Manager
