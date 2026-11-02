{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.Extra.FinanceAccountConfig where

import Data.Aeson
import Data.OpenApi hiding (name)
import Kernel.Prelude
import Kernel.Types.Common

-- Extra code goes here --

-- | When does accrued balance become redeemable. The cutoff instant is the start of
--   the local day (per 'tzOffsetSeconds', optionally shifted to 'timeOfDay' "HH:MM")
--   minus 'days' — entries unsettled before that instant are redeemable, later ones
--   are not. Stored as a json column.
data CutoffSpec = CutoffSpec
  { days :: Int,
    timeOfDay :: Maybe Text,
    tzOffsetSeconds :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Who absorbs an outbound-transfer fee and how it is computed. Effective fee =
--   fixedFee + percentageRate% of gross; missing parts count as zero.
data FeeBearer = OWNER_BEARER | PLATFORM_BEARER
  deriving stock (Generic, Show, Eq, Ord, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FeeSpec = FeeSpec
  { bearer :: FeeBearer,
    fixedFee :: Maybe HighPrecMoney,
    percentageRate :: Maybe Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
