{-# LANGUAGE DerivingStrategies #-}

module Dashboard.Common.Rewards.UnlockResetPeriod
  ( UnlockResetPeriod (..),
  )
where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

-- | How often a cohort's unlock cap resets: every day or every 7 days,
-- both counted in blocks from the campaign's start.
data UnlockResetPeriod = Daily | Weekly
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''UnlockResetPeriod)
