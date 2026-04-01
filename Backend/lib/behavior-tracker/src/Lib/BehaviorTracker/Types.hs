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
    CounterType (..),
    ActionEvent (..),
    CounterConfig (..),
    PeriodConfig (..),
    CounterValues (..),
    BehaviorSnapshot (..),
    BlockType (..),
    BlockInfo (..),
    CooldownInfo (..),
    ActiveBlockInfo (..),
    ActiveCooldownInfo (..),
    BehaviorDomainConfig (..),
    EntityBehaviorVisibility (..),
    defaultCounterValues,
    mkPeriodConfig,
  )
where

import Data.Aeson (Value)
import qualified Data.Aeson as A
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime (..))
import Kernel.Prelude
import Kernel.Types.App ()

-- orphan ToSchema Value instance

-- | Entity performing the action
data EntityType = DRIVER | RIDER
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Which counter to maintain for an action
data CounterType = ACTION_COUNT | ELIGIBLE_COUNT
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Block type classification (matches ConsequenceAction variants)
data BlockType = HARD_BLOCK | SOFT_BLOCK | FEATURE_BLOCK | PERMANENT_BLOCK
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Input event from caller
-- actionType is Text so apps can define their own action types without modifying this library
data ActionEvent = ActionEvent
  { entityType :: EntityType,
    entityId :: Text,
    actionType :: Text, -- e.g. "RIDE_CANCELLATION", "EXTRA_FARE_REQUEST" — defined by caller
    merchantOperatingCityId :: Text,
    flowContext :: Value, -- opaque: caller-resolved city/flow info
    eventData :: Value, -- this event's context (distances, wait time, etc.)
    timestamp :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Counter configuration (caller-provided per action)
data CounterConfig = CounterConfig
  { windowSizeDays :: Integer, -- SWC storage window
    counters :: [CounterType], -- which counters to increment
    periods :: [PeriodConfig] -- which time periods to compute in snapshot
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | A time period to compute counter values for
data PeriodConfig = PeriodConfig
  { periodName :: Text, -- e.g. "daily", "weekly", "monthly"
    periodDays :: Integer -- days to look back (1, 7, 30)
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Helper to create a PeriodConfig
mkPeriodConfig :: Text -> Integer -> PeriodConfig
mkPeriodConfig = PeriodConfig

-- | Counter values for one time period
data CounterValues = CounterValues
  { actionCount :: Integer,
    eligibleCount :: Integer,
    rate :: Integer -- (actionCount * 100) / max 1 eligibleCount
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Default CounterValues where
  def = defaultCounterValues

-- | Default counter values (all zeros)
defaultCounterValues :: CounterValues
defaultCounterValues =
  CounterValues
    { actionCount = 0,
      eligibleCount = 0,
      rate = 0
    }

-- | Stored as JSON value in block Redis keys
data BlockInfo = BlockInfo
  { blockType :: BlockType,
    blockReason :: Text,
    reasonTag :: Text,
    blockedAt :: UTCTime,
    expiresAt :: Maybe UTCTime,
    extraParams :: Value
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Stored as JSON value in cooldown Redis keys
data CooldownInfo = CooldownInfo
  { reasonTag :: Text,
    setAt :: UTCTime,
    expiresAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Active block info returned by visibility layer (includes remaining TTL)
data ActiveBlockInfo = ActiveBlockInfo
  { blockType :: BlockType,
    blockReason :: Text,
    reasonTag :: Text,
    blockedAt :: UTCTime,
    expiresAt :: Maybe UTCTime,
    remainingSeconds :: Integer,
    extraParams :: Value
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Active cooldown info returned by visibility layer
data ActiveCooldownInfo = ActiveCooldownInfo
  { reasonTag :: Text,
    setAt :: UTCTime,
    expiresAt :: UTCTime,
    remainingSeconds :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Config for visibility layer: which action types and reason tags to query
data BehaviorDomainConfig = BehaviorDomainConfig
  { actionTypes :: [(Text, CounterConfig)], -- (actionType, its counter config)
    blockReasonTags :: [Text], -- known reason tags to probe
    blockTypes :: [BlockType] -- which block types to probe per tag
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Full visibility response for an entity
data EntityBehaviorVisibility = EntityBehaviorVisibility
  { entityId :: Text,
    entityType :: EntityType,
    counters :: Map.Map Text (Map.Map Text CounterValues), -- actionType -> periodName -> values
    activeBlocks :: [ActiveBlockInfo],
    activeCooldowns :: [ActiveCooldownInfo],
    queriedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | The unified output — everything downstream needs
data BehaviorSnapshot = BehaviorSnapshot
  { entityType :: EntityType,
    entityId :: Text,
    actionType :: Text,
    merchantOperatingCityId :: Text,
    counters :: Map.Map Text CounterValues, -- keyed by periodName
    cooldowns :: Map.Map Text Bool, -- reasonTag -> isActive (True = in cooldown)
    flowContext :: Value, -- passthrough from ActionEvent
    eventData :: Value, -- passthrough from ActionEvent
    entityState :: Value, -- caller-provided entity state
    snapshotAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Default BehaviorSnapshot where
  def =
    BehaviorSnapshot
      { entityType = DRIVER,
        entityId = "",
        actionType = "",
        merchantOperatingCityId = "",
        counters = Map.empty,
        cooldowns = Map.empty,
        flowContext = A.Object mempty,
        eventData = A.Object mempty,
        entityState = A.Object mempty,
        snapshotAt = UTCTime (toEnum 0) 0
      }
