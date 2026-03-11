{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.ConsequenceEngine.Types
  ( ConsequenceAction (..),
    HardBlockParams (..),
    SoftBlockParams (..),
    FeatureBlockParams (..),
    PermanentBlockParams (..),
    ChargeFeeParams (..),
    NudgeParams (..),
    WarnParams (..),
    ConsequenceHandler (..),
    ConsequenceResult (..),
  )
where

import Data.Aeson (Value)
import Kernel.Prelude

-- | Parsed, typed consequence from rule engine output
data ConsequenceAction
  = NoAction
  | Nudge NudgeParams
  | Warn WarnParams
  | SoftBlock SoftBlockParams
  | FeatureBlock FeatureBlockParams
  | HardBlock HardBlockParams
  | PermanentBlock PermanentBlockParams
  | ChargeFee ChargeFeeParams
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for a nudge (warning only, no restrictions)
data NudgeParams = NudgeParams
  { nudgeKey :: Text -- template key for the nudge message
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for a stronger warning (may show on profile)
data WarnParams = WarnParams
  { warnKey :: Text,
    showOnProfile :: Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for soft block (blocked from specific service tiers/features)
data SoftBlockParams = SoftBlockParams
  { blockDurationHours :: Int,
    blockedFeatures :: [Text], -- service tier names
    blockReason :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for feature-specific block (e.g. toll routes, AC)
data FeatureBlockParams = FeatureBlockParams
  { blockDurationHours :: Int,
    featureName :: Text, -- "TOLL_ROUTES", "AC", etc.
    blockReason :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for hard block (fully blocked from going online)
data HardBlockParams = HardBlockParams
  { blockDurationHours :: Int,
    blockReason :: Text,
    cooldownHours :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for permanent block (requires manual review)
data PermanentBlockParams = PermanentBlockParams
  { blockReason :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for monetary penalty
data ChargeFeeParams = ChargeFeeParams
  { penaltyAmount :: Double,
    currency :: Text,
    chargeReason :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Result of executing a consequence
data ConsequenceResult = ConsequenceResult
  { action :: ConsequenceAction,
    success :: Bool,
    errorMessage :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Typeclass for apps to implement consequence handling.
--
-- The @entityId@ parameter is the entity (driver/rider) that the consequence applies to.
-- The second parameter is type-specific params.
--
-- Apps provide instances that delegate to their domain-specific functions, e.g.:
--   handleHardBlock → SPerson.blockDriverTemporarily (driver app)
--   handleHardBlock → blockCustomerTemporarily (rider app)
class (Monad m) => ConsequenceHandler m where
  handleNudge :: Text -> NudgeParams -> m ()
  handleWarn :: Text -> WarnParams -> m ()
  handleSoftBlock :: Text -> SoftBlockParams -> m ()
  handleFeatureBlock :: Text -> FeatureBlockParams -> m ()
  handleHardBlock :: Text -> HardBlockParams -> m ()
  handlePermanentBlock :: Text -> PermanentBlockParams -> m ()
  handleChargeFee :: Text -> ChargeFeeParams -> m ()
  -- | Optional: called when rule output is NO_ACTION. Default: no-op.
  handleNoAction :: Text -> m ()
  handleNoAction _ = pure ()
  -- | Optional: log/record the consequence for audit. Default: no-op.
  recordConsequence :: Text -> ConsequenceAction -> Value -> m ()
  recordConsequence _ _ _ = pure ()
