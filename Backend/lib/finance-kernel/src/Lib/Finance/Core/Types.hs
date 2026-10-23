{-
  Finance.Core.Types

  Common types shared across the kernel. Account-class configuration lives in
  Lib.Finance.AccountConfig (finance_account_config), not here.
-}
module Lib.Finance.Core.Types
  ( -- * Common Types
    TimeRange (..),
    Image,
    -- Reexport (can be removed, and used directly from Kernel.Utils.Common)
    ActorType (..),
    ActorInfo (..),
    HasActorInfo,
    withActorInfo,
  )
where

import Kernel.Prelude
import Kernel.Utils.Common

-- ═══════════════════════════════════════════════════════════════════════════
-- Common Types
-- ═══════════════════════════════════════════════════════════════════════════

-- | Time range for queries
data TimeRange = TimeRange
  { rangeFrom :: UTCTime,
    rangeTo :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Image
