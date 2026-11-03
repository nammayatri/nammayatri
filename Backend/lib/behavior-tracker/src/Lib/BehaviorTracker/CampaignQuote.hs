{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Campaign quote-response → ij:bm metric attribution (Incentive Journey).
-- Pure policy: receive / accept bumps. Callers (SRD, silent assign, IJ quote recorder)
-- own membership-scoped Redis + Kafka tick wiring — not Domain.UI.Driver.
module Lib.BehaviorTracker.CampaignQuote
  ( CampaignQuoteBump (..),
    quoteReceiveBump,
    quoteAcceptBump,
    quoteReceiveFlushActionType,
  )
where

import Kernel.Prelude
import Lib.BehaviorTracker.CampaignNames
  ( CampaignMetric (..),
    CampaignTickAction (..),
    campaignMetricText,
    campaignTickActionText,
  )

-- | Campaign quote counter + Kafka actionType for 'BehaviourMetricTick'.
-- Text fields are derived from 'CampaignNames' — do not inline string literals here.
data CampaignQuoteBump = CampaignQuoteBump
  { metric :: Text,
    actionType :: Text
  }
  deriving (Eq, Show, Generic)

-- | SRD batch send / silent-assign receive → rrReceived + QUOTE_RESPONSE tick.
quoteReceiveBump :: CampaignQuoteBump
quoteReceiveBump =
  CampaignQuoteBump
    { metric = campaignMetricText RrReceived,
      actionType = campaignTickActionText QuoteResponse
    }

-- | Manual / silent assign Accept → rrAccepted + QUOTE_RESPONSE_ACCEPT tick.
quoteAcceptBump :: CampaignQuoteBump
quoteAcceptBump =
  CampaignQuoteBump
    { metric = campaignMetricText RrAccepted,
      actionType = campaignTickActionText QuoteResponseAccept
    }

-- | Internal stats-sync nudge after accept/cancel (not a primary receive bump).
quoteReceiveFlushActionType :: Text
quoteReceiveFlushActionType = campaignTickActionText RrReceivedFlush
