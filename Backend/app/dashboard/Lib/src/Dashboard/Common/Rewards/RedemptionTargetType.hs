{-# LANGUAGE DerivingStrategies #-}

module Dashboard.Common.Rewards.RedemptionTargetType
  ( RedemptionTargetType (..),
  )
where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data RedemptionTargetType = Internal | ExternalUrl | ExternalManual
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''RedemptionTargetType)
