{-# LANGUAGE DerivingStrategies #-}

module Dashboard.Common.Rewards.CampaignStatus
  ( CampaignStatus (..),
  )
where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data CampaignStatus = Draft | Active | Paused | Ended
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''CampaignStatus)
