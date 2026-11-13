{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverAccountHealth where

import Data.OpenApi (ToSchema)
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
  { block :: Kernel.Prelude.Maybe AccountBlock,
    cancellation :: [CancellationPeriod],
    rulesVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    warnings :: [Lib.BehaviorTracker.Types.ProfileWarningInfo]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancellationPeriod = CancellationPeriod
  { actionCount :: Kernel.Prelude.Int,
    blockAt :: Kernel.Prelude.Maybe RuleLimit,
    eligibleCount :: Kernel.Prelude.Int,
    period :: Kernel.Prelude.Text,
    rate :: Kernel.Prelude.Int,
    warnAt :: Kernel.Prelude.Maybe RuleLimit
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RuleLimit = RuleLimit
  { hours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minCancellations :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minRides :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rate :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
