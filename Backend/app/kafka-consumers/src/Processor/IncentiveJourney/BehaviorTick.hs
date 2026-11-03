{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | BehaviourMetricTick → stats sync only (+ closed-period settle). No mid-period reward.
module Processor.IncentiveJourney.BehaviorTick
  ( handleBehaviorMetricTick,
  )
where

import qualified "dynamic-offer-driver-app" Domain.Types.Merchant as DM
import qualified "dynamic-offer-driver-app" Domain.Types.MerchantOperatingCity as DMOC
import qualified "dynamic-offer-driver-app" Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import "config-pilot" Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.IncentiveJourney.BehaviorMetric as BM
import qualified "dynamic-offer-driver-app" SharedLogic.IncentiveJourney as SLJourney
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

debounceTtlSeconds :: Int
debounceTtlSeconds = 3

handleBehaviorMetricTick ::
  SLJourney.JourneyRewardDispatchFlow m r =>
  BM.BehaviorMetricTick ->
  m ()
handleBehaviorMetricTick tick = do
  -- Online-minute closed sessions publish one tick per day/window slice; do not coalesce them.
  let isOnlineMinutes =
        tick.metricName == Just "onlineMinutes"
          || tick.actionType == "ONLINE_MINUTES"
      debounceKey = BM.behaviorTickDebounceKey tick.personId
  acquired <-
    if isOnlineMinutes
      then pure True
      else
        Hedis.withCrossAppRedis $
          Hedis.setNxExpire debounceKey debounceTtlSeconds ("1" :: Text)
  unless acquired $
    logDebug $
      "BehaviorMetricTick debounced personId=" <> tick.personId
  when acquired $ do
    let driverId = Id @DP.Person tick.personId
        merchantId = Id @DM.Merchant tick.merchantId
        merchantOpCityId = Id @DMOC.MerchantOperatingCity tick.merchantOperatingCityId
    transporterConfig <-
      getOneConfig
        (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
        Nothing
        >>= fromMaybeM (InternalError "TransporterConfig not found for BehaviorMetricTick")
    let metricOrAction = fromMaybe tick.actionType tick.metricName
        delta = fromMaybe 1 tick.delta
        cjmIds = fromMaybe [] tick.cjmIds
    SLJourney.syncDriverBehaviorMetricTick
      driverId
      merchantId
      merchantOpCityId
      transporterConfig
      tick.occurredAt
      cjmIds
      metricOrAction
      delta
