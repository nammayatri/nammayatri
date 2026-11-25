{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Campaign online-minutes → ij:bm metric attribution (Incentive Journey).
-- Pure policy: which Redis metric / BehaviourMetricTick action to publish.
-- Callers (go-offline, list settle flush) own session split + Redis incr + Kafka wiring.
module Lib.BehaviorTracker.CampaignOnline
  ( CampaignOnlineBump (..),
    onlineMinutesBump,
  )
where

import Kernel.Prelude
import Lib.BehaviorTracker.CampaignNames
  ( CampaignMetric (..),
    CampaignTickAction (..),
    campaignMetricText,
    campaignTickActionText,
  )

-- | Campaign online counter + Kafka actionType for 'BehaviourMetricTick'.
-- Text fields are derived from 'CampaignNames' — do not inline string literals here.
data CampaignOnlineBump = CampaignOnlineBump
  { metric :: Text,
    actionType :: Text
  }
  deriving (Eq, Show, Generic)

-- | Closed / banked online session slice → onlineMinutes + ONLINE_MINUTES tick.
onlineMinutesBump :: CampaignOnlineBump
onlineMinutesBump =
  CampaignOnlineBump
    { metric = campaignMetricText OnlineMinutes,
      actionType = campaignTickActionText OnlineMinutesTick
    }
