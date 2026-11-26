{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Campaign cancel → ij:bm metric attribution (Incentive Journey).
-- Pure policy only: which Redis metric / BehaviourMetricTick action to bump.
-- Callers (cancellation orchestrator) still own:
--   * 'wouldBump' gates (fault / cancel-rate matrix)
--   * membership-scoped Redis incr + Kafka tick (driver-app IJ wiring)
--
-- Keep this next to 'CampaignCounters' so cancel campaign behaviour lives in the
-- behaviour-tracker package, not in Beckn/UI cancel handlers.
module Lib.BehaviorTracker.CampaignCancel
  ( CampaignCancelSource (..),
    CampaignCancelBump (..),
    driverCancelBump,
    customerCancelBump,
    otherCancelBump,
    resolveCampaignCancelBump,
  )
where

import Kernel.Prelude
import Lib.BehaviorTracker.CampaignNames
  ( CampaignMetric (..),
    CampaignTickAction (..),
    campaignMetricText,
    campaignTickActionText,
  )

-- | Who pressed cancel — domain 'CancellationSource' mapped at the call site.
data CampaignCancelSource
  = CancelByDriver
  | CancelByCustomer
  | -- | Merchant / allocator / application / fleet / … when metrics would bump.
    CancelByOther
  deriving (Eq, Show, Generic)

-- | Campaign cancel counter + Kafka actionType for 'BehaviourMetricTick'.
-- Text fields are derived from 'CampaignNames' — do not inline string literals here.
data CampaignCancelBump = CampaignCancelBump
  { metric :: Text,
    actionType :: Text
  }
  deriving (Eq, Show, Generic)

driverCancelBump :: CampaignCancelBump
driverCancelBump =
  CampaignCancelBump
    { metric = campaignMetricText DxCancel,
      actionType = campaignTickActionText RideCancellation
    }

-- | Rider pressed cancel but driver is at fault (wouldBump) → dafCancel.
customerCancelBump :: CampaignCancelBump
customerCancelBump =
  CampaignCancelBump
    { metric = campaignMetricText DafCancel,
      actionType = campaignTickActionText CustomerCancellation
    }

-- | Merchant / allocator / … → dafCancel with RIDE_CANCELLATION tick (same as prior behaviour).
otherCancelBump :: CampaignCancelBump
otherCancelBump =
  CampaignCancelBump
    { metric = campaignMetricText DafCancel,
      actionType = campaignTickActionText RideCancellation
    }

-- | Resolve campaign cancel bump from source + whether driver cancel metrics would bump.
--
--   * ByDriver + wouldBump → dxCancel / RIDE_CANCELLATION
--   * ByCustomer + wouldBump → dafCancel / CUSTOMER_CANCELLATION
--     (daf = driver-at-fault only; customer-at-fault rider cancel → no bump)
--   * Other + wouldBump → dafCancel / RIDE_CANCELLATION
--   * Otherwise → Nothing (no campaign bump)
resolveCampaignCancelBump :: CampaignCancelSource -> Bool -> Maybe CampaignCancelBump
resolveCampaignCancelBump source wouldBump =
  case source of
    CancelByDriver
      | wouldBump -> Just driverCancelBump
      | otherwise -> Nothing
    CancelByCustomer
      | wouldBump -> Just customerCancelBump
      | otherwise -> Nothing
    CancelByOther
      | wouldBump -> Just otherCancelBump
      | otherwise -> Nothing
