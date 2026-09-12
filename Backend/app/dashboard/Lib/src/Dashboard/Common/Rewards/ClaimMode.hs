{-# LANGUAGE DerivingStrategies #-}

module Dashboard.Common.Rewards.ClaimMode
  ( ClaimMode (..),
  )
where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data ClaimMode = AutoClaim | ManualClaim
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''ClaimMode)
