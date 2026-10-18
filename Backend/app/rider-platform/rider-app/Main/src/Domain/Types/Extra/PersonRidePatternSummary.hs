module Domain.Types.Extra.PersonRidePatternSummary where

import qualified Data.Map.Strict as M
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

data TravelMode = TAXI | PUBLIC_TRANSPORT
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data RoutePattern = RoutePattern
  { count :: Int,
    frequency :: Text,
    timeOfDay :: [Text],
    travelMode :: TravelMode,
    fromGeohash :: Maybe Text,
    toGeohash :: Maybe Text,
    fromLat :: Maybe Double,
    fromLon :: Maybe Double,
    toLat :: Maybe Double,
    toLon :: Maybe Double,
    fromAddress :: Maybe Text,
    toAddress :: Maybe Text,
    fromStopCode :: Maybe Text,
    toStopCode :: Maybe Text,
    serviceCounts :: M.Map Text Int,
    medianFare :: Maybe HighPrecMoney,
    lastTravelledAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

type ServiceCountMap = M.Map Text Int

type MoneyByServiceMap = M.Map Text HighPrecMoney

type RoutePatternMap = M.Map Text RoutePattern
