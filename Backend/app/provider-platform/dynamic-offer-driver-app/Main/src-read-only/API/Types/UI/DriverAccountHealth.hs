{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverAccountHealth where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Common
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Lib.BehaviorTracker.Types
import Servant
import Tools.Auth
import qualified Tools.Error

data AccountBlock = AccountBlock {blockedUntil :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime, reasonFlag :: Kernel.Prelude.Maybe Tools.Error.BlockReasonFlag}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AccountHealthRes = AccountHealthRes
  { behaviours :: [BehaviourHealth],
    block :: Kernel.Prelude.Maybe AccountBlock,
    blocks :: [ActiveBlock],
    status :: AccountHealthStatus,
    warnings :: [Lib.BehaviorTracker.Types.ProfileWarningInfo]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AccountHealthStatus
  = GOOD
  | WARNING
  | RESTRICTED
  | BLOCKED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ActiveBlock = ActiveBlock
  { blockType :: ActiveBlockType,
    blockedUntil :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    feature :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTiers :: Kernel.Prelude.Maybe [Domain.Types.Common.ServiceTierType],
    tag :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ActiveBlockType
  = HARD
  | PERMANENT
  | SOFT
  | FEATURE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BehaviourHealth = BehaviourHealth {behaviour :: Kernel.Prelude.Text, periods :: [BehaviourPeriod], rules :: [BehaviourRule]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BehaviourPeriod = BehaviourPeriod
  { actionCount :: Kernel.Prelude.Int,
    eligibleCount :: Kernel.Prelude.Int,
    period :: Kernel.Prelude.Text,
    rate :: Kernel.Prelude.Int,
    windowSize :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BehaviourRule = BehaviourRule
  { blockReasonFlag :: Kernel.Prelude.Maybe Tools.Error.BlockReasonFlag,
    conditions :: [RuleCondition],
    hours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    kind :: Kernel.Prelude.Text,
    tag :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RuleCondition = RuleCondition {fact :: Kernel.Prelude.Text, op :: Kernel.Prelude.Text, value :: Data.Aeson.Value}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
