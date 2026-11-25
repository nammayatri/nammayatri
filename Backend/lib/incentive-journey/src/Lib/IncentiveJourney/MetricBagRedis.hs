{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Period ride-counter Redis bag for JsonLogic (Kafka-side accumulation).
module Lib.IncentiveJourney.MetricBagRedis
  ( periodRideBagKey,
    loadOrApplyRideDeltas,
  )
where

import qualified Data.HashMap.Strict as HM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.BehaviorMetric as BM
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import Lib.IncentiveJourney.Types (RideDeltas)

periodRideBagTtlSeconds :: Hedis.ExpirationTime
periodRideBagTtlSeconds = 45 * 24 * 3600

periodRideBagKey :: Id Common.Person -> Id DCJM.CohortJourneyMapping -> Text -> Text
periodRideBagKey personId cjmId periodKey =
  "ij:metricBag:ride:"
    <> personId.getId
    <> ":"
    <> cjmId.getId
    <> ":"
    <> periodKey

loadOrApplyRideDeltas ::
  (Hedis.HedisFlow m r, CacheFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  Maybe RideDeltas ->
  m BM.MetricContext
loadOrApplyRideDeltas personId cjmId periodKey mbDeltas = do
  let key = periodRideBagKey personId cjmId periodKey
  current <-
    Hedis.withCrossAppRedis (Hedis.safeGet key) >>= \case
      Just (vars :: HM.HashMap Text Int) -> pure vars
      Nothing -> pure HM.empty
  case mbDeltas of
    Nothing -> pure BM.MetricContext {vars = current}
    Just deltas -> do
      let updated = BM.applyRideDeltasToContext deltas BM.MetricContext {vars = current}
      Hedis.runInMultiCloudRedisWrite $
        Hedis.withCrossAppRedis $
          Hedis.setExp key updated.vars periodRideBagTtlSeconds
      pure updated
