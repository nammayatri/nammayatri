{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Kafka-side full metric bag for JsonLogic (ride Redis + campaign ij:bm).
-- Cancel/accept rates are derived from period-scoped campaign counts (not city bt:).
-- Do not call from HTTP EndRide / go-online.
module SharedLogic.IncentiveJourney.MetricBag
  ( buildMetricContext,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Person as Person
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.BehaviorMetric as BM
import qualified Lib.IncentiveJourney.BehaviorMetricRedis as BMRedis
import qualified Lib.IncentiveJourney.CampaignTag as CampaignTag
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as IJCommon
import qualified Lib.IncentiveJourney.MetricBagRedis as MetricBagRedis
import qualified SharedLogic.IncentiveJourney.CampaignBehaviorBump as CampaignBehaviorBump
import qualified Storage.Queries.Person as QPerson

buildMetricContext ::
  (Hedis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id IJCommon.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  Maybe IJ.RideDeltas ->
  m BM.MetricContext
buildMetricContext personId cjmId periodKey mbDeltas = do
  rideCtx <- MetricBagRedis.loadOrApplyRideDeltas personId cjmId periodKey mbDeltas
  campaignCtx <- loadCampaignBehaviorCounts personId cjmId periodKey
  pure $
    BM.enrichWithDerivedBehaviorRates $
      BM.mergeMetricContext rideCtx campaignCtx

-- | Campaign-scoped counts from ij:bm for this CJM + periodKey (same period as stats rows).
loadCampaignBehaviorCounts ::
  (Hedis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id IJCommon.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  m BM.MetricContext
loadCampaignBehaviorCounts personId cjmId periodKey = do
  mbPerson <- QPerson.findById (cast personId :: Id Person.Person)
  let tbKeys =
        case mbPerson >>= (.driverTag) of
          Just tags ->
            case find (\m -> m.cjmId == cjmId.getId) (CampaignBehaviorBump.membershipsFromPersonTags tags) of
              Just m -> m.timeBoundKeys
              Nothing -> [CampaignTag.timeBoundKeyDaily]
          Nothing -> [CampaignTag.timeBoundKeyDaily]
  online <- BMRedis.getCampaignBehaviorMetrics BMRedis.OnlineMinutes personId cjmId periodKey tbKeys
  rrRecv <- BMRedis.getCampaignBehaviorMetrics BMRedis.RrReceived personId cjmId periodKey tbKeys
  rrAcc <- BMRedis.getCampaignBehaviorMetrics BMRedis.RrAccepted personId cjmId periodKey tbKeys
  dxCancel <- BMRedis.getCampaignBehaviorMetrics BMRedis.DxCancel personId cjmId periodKey tbKeys
  dafCancel <- BMRedis.getCampaignBehaviorMetrics BMRedis.DafCancel personId cjmId periodKey tbKeys
  pure
    BM.MetricContext
      { vars =
          HM.fromList
            [ (BM.varOnlineMinutesTotal, online),
              (BM.varRrReceived, rrRecv),
              (BM.varRrAccepted, rrAcc),
              (BM.varDxCancel, dxCancel),
              (BM.varDafCancel, dafCancel)
            ]
      }
