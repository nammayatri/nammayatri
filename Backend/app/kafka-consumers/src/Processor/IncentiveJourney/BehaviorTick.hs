{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | BehaviourMetricTick → stats sync only (+ closed-period settle). No mid-period reward.
-- One Kafka tick = one Redis bump's delta; sync also raises InsideTimebound to Redis absolute
-- so lost messages cannot leave stats permanently behind ij:bm.
module Processor.IncentiveJourney.BehaviorTick
  ( handleBehaviorMetricTick,
  )
where

import qualified "dynamic-offer-driver-app" Domain.Types.Merchant as DM
import qualified "dynamic-offer-driver-app" Domain.Types.MerchantOperatingCity as DMOC
import qualified "dynamic-offer-driver-app" Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import "config-pilot" Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.IncentiveJourney.BehaviorMetric as BM
import qualified Lib.IncentiveJourney.Domain.Action.StatsSync as StatsSync
import qualified "dynamic-offer-driver-app" SharedLogic.IncentiveJourney as SLJourney
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

handleBehaviorMetricTick ::
  SLJourney.JourneyRewardDispatchFlow m r =>
  BM.BehaviorMetricTick ->
  m ()
handleBehaviorMetricTick tick = do
  let metricOrAction = fromMaybe tick.actionType tick.metricName
      delta = fromMaybe 1 tick.delta
      cjmIds = fromMaybe [] tick.cjmIds
  case StatsSync.eventNameToMetricBase metricOrAction of
    Nothing ->
      logDebug $ "BehaviorMetricTick skipped unknown metric/action=" <> metricOrAction
    Just _metricBase -> do
      let driverId = Id @DP.Person tick.personId
          merchantId = Id @DM.Merchant tick.merchantId
          merchantOpCityId = Id @DMOC.MerchantOperatingCity tick.merchantOperatingCityId
      transporterConfig <-
        getOneConfig
          (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
          Nothing
          >>= fromMaybeM (InternalError "TransporterConfig not found for BehaviorMetricTick")
      SLJourney.syncDriverBehaviorMetricTick
        driverId
        merchantId
        merchantOpCityId
        transporterConfig
        tick.occurredAt
        cjmIds
        metricOrAction
        delta
