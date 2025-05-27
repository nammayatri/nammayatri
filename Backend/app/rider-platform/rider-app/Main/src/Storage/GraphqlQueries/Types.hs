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
import Data.Morpheus.Client.CodeGen.Internal
import qualified Data.Time.LocalTime as LocalTime
import EulerHS.Types (OptionEntity)
import Kernel.Prelude
import Network.HTTP.Client

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

-- JSON instances for OpenTripPlanner response
instance FromJSON OTPResponse where
  parseJSON = withObject "OTPResponse" $ \obj -> do
    dataObj <- obj .: "data"
    OTPResponse <$> dataObj .: "stop"

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

data RouteStopTimeTableEntry = RouteStopTimeTableEntry
  { scheduledArrival :: Int,
    realtimeArrival :: Int,
    arrivalDelay :: Int,
    scheduledDeparture :: Int,
    headsign :: Maybe Text,
    trip :: TripData
  }
  deriving (Show, Generic)

instance FromJSON RouteStopTimeTableEntry where
  parseJSON = withObject "RouteStopTimeTableEntry" $ \obj -> do
    RouteStopTimeTableEntry
      <$> obj .: "scheduledArrival"
      <*> obj .: "realtimeArrival"
      <*> obj .: "arrivalDelay"
      <*> obj .: "scheduledDeparture"
      <*> obj .:? "headsign"
      <*> obj .: "trip"

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
    createdAt :: UTCTime,
    updatedAt :: UTCTime
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
      ++ "    stoptimesWithoutPatterns(numberOfDepartures: 500) {\n"
      ++ "      scheduledArrival\n"
      ++ "      realtimeArrival\n"
      ++ "      arrivalDelay\n"
      ++ "      scheduledDeparture\n"
      ++ "      headsign\n"
      ++ "      trip {\n"
      ++ "        serviceId\n"
      ++ "        tripShortName\n"
      ++ "        gtfsId\n"
      ++ "        route {\n"
      ++ "          gtfsId\n"
      ++ "        }\n"
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
