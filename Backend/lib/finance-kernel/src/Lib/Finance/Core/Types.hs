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
    Actor (..),
  )
where

import Data.Aeson (Value)
import qualified Data.List as List
import qualified Data.Text as T
import Kernel.Prelude
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

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

-- we can use ActorInfo ? or expand this type
data Actor = System | Person Text -- should we differentiate roles ADMIN, DRIVER, FLEET_OWNER?
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema) -- , ToParamSchema)

$(mkBeamInstancesForEnum ''Actor)

instance Show Actor where
  show System = "System"
  show (Person personId) = "Person_" <> T.unpack personId

instance Read Actor where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (System, r1)
            | r1 <- stripPrefix "System" r,
              null r1
          ]
            ++ [ (Person v1, r2)
                 | r1 <- stripPrefix "Person_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r
