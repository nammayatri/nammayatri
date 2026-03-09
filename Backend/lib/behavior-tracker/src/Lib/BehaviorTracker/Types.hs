{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.BehaviorTracker.Types
  ( EntityType (..),
    ActionType (..),
    CounterType (..),
    ActionEvent (..),
    CounterConfig (..),
    PeriodConfig (..),
    CounterValues (..),
    BehaviorSnapshot (..),
    defaultCounterValues,
    mkPeriodConfig,
  )
where

import Data.Aeson (Value)
import qualified Data.Map.Strict as Map
import Kernel.Prelude

-- | Entity performing the action
data EntityType = DRIVER | RIDER
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON)

-- | Extensible action type — new actions added without touching core logic
data ActionType
  = -- Driver actions
    DRIVER_RIDE_CANCELLATION
  | DRIVER_EXTRA_FARE_REQUEST
  | DRIVER_AC_NOT_TURNED_ON
  | DRIVER_GPS_OFF_ON_TOLL_ROUTE
  | DRIVER_TOLL_ROUTE_DEVIATION
  | DRIVER_ROUTE_DEVIATION
  | DRIVER_SOS_FALSE_ALARM
  | -- Rider actions
    RIDER_BOOKING_CANCELLATION
  | RIDER_NO_SHOW
  | RIDER_FARE_DISPUTE
  | RIDER_PAYMENT_DEFAULT
  | RIDER_SAFETY_FALSE_ALARM
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON)

-- | Which counter to maintain for an action
data CounterType = ACTION_COUNT | ELIGIBLE_COUNT
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON)

-- | Input event from caller
data ActionEvent = ActionEvent
  { entityType :: EntityType,
    entityId :: Text,
    actionType :: ActionType,
    merchantOperatingCityId :: Text,
    flowContext :: Value, -- opaque: caller-resolved city/flow info
    eventData :: Value, -- this event's context (distances, wait time, etc.)
    timestamp :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Counter configuration (caller-provided per action)
data CounterConfig = CounterConfig
  { windowSizeDays :: Integer, -- SWC storage window
    counters :: [CounterType], -- which counters to increment
    periods :: [PeriodConfig] -- which time periods to compute in snapshot
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | A time period to compute counter values for
data PeriodConfig = PeriodConfig
  { periodName :: Text, -- e.g. "daily", "weekly", "monthly"
    periodDays :: Integer -- days to look back (1, 7, 30)
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Helper to create a PeriodConfig
mkPeriodConfig :: Text -> Integer -> PeriodConfig
mkPeriodConfig = PeriodConfig

-- | Counter values for one time period
data CounterValues = CounterValues
  { actionCount :: Integer,
    eligibleCount :: Integer,
    rate :: Integer -- (actionCount * 100) / max 1 eligibleCount
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Default counter values (all zeros)
defaultCounterValues :: CounterValues
defaultCounterValues =
  CounterValues
    { actionCount = 0,
      eligibleCount = 0,
      rate = 0
    }

-- | The unified output — everything downstream needs
data BehaviorSnapshot = BehaviorSnapshot
  { entityType :: EntityType,
    entityId :: Text,
    actionType :: ActionType,
    merchantOperatingCityId :: Text,
    counters :: Map.Map Text CounterValues, -- keyed by periodName
    flowContext :: Value, -- passthrough from ActionEvent
    eventData :: Value, -- passthrough from ActionEvent
    entityState :: Value, -- caller-provided entity state
    snapshotAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)
