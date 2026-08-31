{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Domain.Types.Common where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Utils.TH

data Person

data Merchant

data MerchantOperatingCity

data MilestoneRewardType
  = Coins
  | Cash
  | Coupons
  | WalletMoney
  | SubscriptionWaiveOff
  | PoolingPriority
  | NoReward
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''MilestoneRewardType)
$(mkHttpInstancesForEnum ''MilestoneRewardType)
