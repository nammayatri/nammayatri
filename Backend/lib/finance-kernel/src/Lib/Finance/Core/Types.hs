{-
  Finance.Core.Types

  Common types and FinanceEnv for domain configuration.
-}
module Lib.Finance.Core.Types
  ( -- * FinanceEnv (domain provides config)
    FinanceEnv (..),
    ConfigKey (..),
    ConfigValue (..),

    -- * Common Types
    TimeRange (..),
  )
where

import Data.Aeson (Value)
import Kernel.Prelude

-- ═══════════════════════════════════════════════════════════════════════════
-- Finance Environment (Domain provides config)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Configuration key
newtype ConfigKey = ConfigKey {unConfigKey :: Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Configuration value (JSON)
newtype ConfigValue = ConfigValue {unConfigValue :: Value}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Finance environment - domain provides this when initializing
data FinanceEnv domain m = FinanceEnv
  { getConfig :: ConfigKey -> m (Maybe ConfigValue),
    getDomainContext :: m domain
  }

-- ═══════════════════════════════════════════════════════════════════════════
-- Common Types
-- ═══════════════════════════════════════════════════════════════════════════

-- | Time range for queries
data TimeRange = TimeRange
  { rangeFrom :: UTCTime,
    rangeTo :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
