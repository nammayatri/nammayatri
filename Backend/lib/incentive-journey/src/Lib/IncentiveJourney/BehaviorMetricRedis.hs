{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Typed IJ façade over 'Lib.BehaviorTracker.CampaignCounters'.
-- Redis ownership (ij:bm: keys) lives in behavior-tracker; this module keeps
-- CampaignBehaviorMetric + Id wrappers so call sites stay unchanged.
-- Key: ij:bm:{metric}:{driverId}:{cjmId}:{periodKey}:{timeBoundKey}
module Lib.IncentiveJourney.BehaviorMetricRedis
  ( CampaignBehaviorMetric (..),
    metricToText,
    campaignBehaviorKey,
    incrCampaignBehaviorMetric,
    getCampaignBehaviorMetric,
    getCampaignBehaviorMetrics,
    campaignBehaviorTtlSeconds,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.CampaignCounters as BTCampaign
import qualified Lib.BehaviorTracker.CampaignNames as BTNames
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common

data CampaignBehaviorMetric
  = OnlineMinutes
  | RrReceived
  | RrAccepted
  | DxCancel
  | DafCancel
  deriving (Eq, Show, Generic)

metricToText :: CampaignBehaviorMetric -> Text
metricToText = BTNames.campaignMetricText . toCampaignMetric
  where
    toCampaignMetric = \case
      OnlineMinutes -> BTNames.OnlineMinutes
      RrReceived -> BTNames.RrReceived
      RrAccepted -> BTNames.RrAccepted
      DxCancel -> BTNames.DxCancel
      DafCancel -> BTNames.DafCancel

campaignBehaviorTtlSeconds :: Hedis.ExpirationTime
campaignBehaviorTtlSeconds = BTCampaign.campaignBehaviorTtlSeconds

campaignBehaviorKey ::
  CampaignBehaviorMetric ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  Text ->
  Text
campaignBehaviorKey metric personId cjmId periodKey timeBoundKey =
  BTCampaign.mkCampaignBehaviorKey
    (metricToText metric)
    personId.getId
    cjmId.getId
    periodKey
    timeBoundKey

incrCampaignBehaviorMetric ::
  (Hedis.HedisFlow m r, CacheFlow m r) =>
  CampaignBehaviorMetric ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  Text ->
  Int ->
  m Int ->
  m Int
incrCampaignBehaviorMetric metric personId cjmId periodKey timeBoundKey delta seedAction =
  BTCampaign.incrCampaignBehaviorCounter
    (metricToText metric)
    personId.getId
    cjmId.getId
    periodKey
    timeBoundKey
    delta
    seedAction

getCampaignBehaviorMetric ::
  (Hedis.HedisFlow m r, CacheFlow m r) =>
  CampaignBehaviorMetric ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  Text ->
  m Int
getCampaignBehaviorMetric metric personId cjmId periodKey timeBoundKey =
  BTCampaign.getCampaignBehaviorCounter
    (metricToText metric)
    personId.getId
    cjmId.getId
    periodKey
    timeBoundKey

getCampaignBehaviorMetrics ::
  (Hedis.HedisFlow m r, CacheFlow m r) =>
  CampaignBehaviorMetric ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  [Text] ->
  m Int
getCampaignBehaviorMetrics metric personId cjmId periodKey timeBoundKeys =
  BTCampaign.getCampaignBehaviorCounterSum
    (metricToText metric)
    personId.getId
    cjmId.getId
    periodKey
    timeBoundKeys
