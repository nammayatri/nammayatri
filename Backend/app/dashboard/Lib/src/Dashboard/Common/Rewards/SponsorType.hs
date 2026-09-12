{-# LANGUAGE DerivingStrategies #-}

module Dashboard.Common.Rewards.SponsorType
  ( SponsorType (..),
  )
where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data SponsorType = Internal | External
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''SponsorType)
