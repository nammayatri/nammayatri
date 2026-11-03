{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Typed campaign ij:bm metric + BehaviourMetricTick action names.
-- Wire Text for campaign-only ticks lives here; shared BF action types are
-- owned by 'Lib.BehaviorTracker.ActionTypes' and reused below so campaign
-- renames cannot silently remount city bt: Redis keys.
module Lib.BehaviorTracker.CampaignNames
  ( CampaignMetric (..),
    CampaignTickAction (..),
    campaignMetricText,
    campaignTickActionText,
  )
where

import Kernel.Prelude
import qualified Lib.BehaviorTracker.ActionTypes as BF

-- | Redis / stats bag metric base (ij:bm:{metric}:…).
data CampaignMetric
  = OnlineMinutes
  | RrReceived
  | RrAccepted
  | DxCancel
  | DafCancel
  deriving (Eq, Show, Generic)

-- | BehaviourMetricTick.actionType (and city bt: series where they align).
data CampaignTickAction
  = QuoteResponse
  | QuoteResponseAccept
  | RideCancellation
  | CustomerCancellation
  | OnlineMinutesTick
  | RrReceivedFlush
  deriving (Eq, Show, Generic)

campaignMetricText :: CampaignMetric -> Text
campaignMetricText = \case
  OnlineMinutes -> "onlineMinutes"
  RrReceived -> "rrReceived"
  RrAccepted -> "rrAccepted"
  DxCancel -> "dxCancel"
  DafCancel -> "dafCancel"

campaignTickActionText :: CampaignTickAction -> Text
campaignTickActionText = \case
  QuoteResponse -> BF.quoteResponseActionType
  QuoteResponseAccept -> BF.quoteResponseAcceptActionType
  RideCancellation -> BF.rideCancellationActionType
  CustomerCancellation -> "CUSTOMER_CANCELLATION"
  OnlineMinutesTick -> "ONLINE_MINUTES"
  RrReceivedFlush -> "RR_RECEIVED_FLUSH"
