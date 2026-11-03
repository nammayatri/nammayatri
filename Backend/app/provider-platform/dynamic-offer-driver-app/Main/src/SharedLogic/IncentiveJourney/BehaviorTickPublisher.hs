{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Publisher for BehaviourMetricTick on Redis Streams (same pattern as ride-events).
-- Call via SharedLogic.IncentiveJourney.publishBehaviorMetricTick (forked from HTTP).
-- Do not check assignments here — consumer no-ops when none.
module SharedLogic.IncentiveJourney.BehaviorTickPublisher
  ( publishBehaviorMetricTick,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (hash)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment (RideEventsPublisherCfg)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Storage.Hedis.Config (HedisFlow)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.BehaviorMetric as BM

-- | Best-effort publish after BT Redis bump. Never throws; never loads bag / writes stats.
publishBehaviorMetricTick ::
  ( HedisFlow m r,
    HasField "ijBehaviorTickPublisherCfg" r (Maybe RideEventsPublisherCfg),
    Metrics.CoreMetrics m
  ) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  m ()
publishBehaviorMetricTick personId merchantId merchantOpCityId actionType = do
  mbCfg <- asks (.ijBehaviorTickPublisherCfg)
  case mbCfg of
    Nothing -> pure ()
    Just cfg -> do
      result <- try @_ @SomeException $ do
        now <- getCurrentTime
        let tick =
              BM.BehaviorMetricTick
                { personId = personId.getId,
                  merchantId = merchantId.getId,
                  merchantOperatingCityId = merchantOpCityId.getId,
                  actionType = actionType,
                  occurredAt = now
                }
            shardId = hash personId.getId `mod` cfg.shardCount
            streamName = cfg.streamPrefix <> show shardId
            payload = BSL.toStrict (A.encode tick)
        void $ Hedis.xAdd streamName "*" [("payload", payload)]
        logInfo $
          "ij-behavior-tick.published personId="
            <> personId.getId
            <> " actionType="
            <> actionType
            <> " stream="
            <> streamName
      case result of
        Right () -> Metrics.incrementGenericMetrics "ij_behavior_tick_published"
        Left e -> do
          logError $
            "ij-behavior-tick.publish-failed personId="
              <> personId.getId
              <> " actionType="
              <> actionType
              <> " err="
              <> show e
          Metrics.incrementGenericMetrics "ij_behavior_tick_publish_failed"
