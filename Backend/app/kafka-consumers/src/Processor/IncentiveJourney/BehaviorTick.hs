{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Redis-stream / Kafka BehaviourMetricTick → full-bag JsonLogic + stats upsert (driver).
-- App HTTP only BT-bumps + publishBehaviorMetricTick (xAdd); this consumer does the heavy work
-- — same isolation as RIDE_EVENTS_CONSUMER for ride-end journey eval.
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
  let debounceKey = BM.behaviorTickDebounceKey tick.personId
  acquired <-
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
    SLJourney.evaluateDriverBehaviorTick
      driverId
      merchantId
      merchantOpCityId
      transporterConfig
      Nothing
      tick.occurredAt
