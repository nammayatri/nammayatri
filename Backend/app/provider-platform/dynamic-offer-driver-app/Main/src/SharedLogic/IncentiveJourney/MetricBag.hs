{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Kafka-side full metric bag for JsonLogic (ride Redis + behaviour-tracker rates).
-- Do not call from HTTP EndRide / go-online.
module SharedLogic.IncentiveJourney.MetricBag
  ( buildMetricContext,
  )
where

import qualified Data.HashMap.Strict as HM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.Accumulator as BTAcc
import qualified Lib.BehaviorTracker.Types as BTT
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.BehaviorMetric as BM
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as IJCommon
import qualified Lib.IncentiveJourney.MetricBagRedis as MetricBagRedis

defaultRateWindowDays :: Integer
defaultRateWindowDays = 7

defaultRatePeriodDays :: Integer
defaultRatePeriodDays = 1

buildMetricContext ::
  (Hedis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id IJCommon.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  Maybe IJ.RideDeltas ->
  m BM.MetricContext
buildMetricContext personId cjmId periodKey mbDeltas = do
  rideCtx <- MetricBagRedis.loadOrApplyRideDeltas personId cjmId periodKey mbDeltas
  behaviourCtx <- loadBehaviourRates personId
  pure $ BM.mergeMetricContext rideCtx behaviourCtx

loadBehaviourRates ::
  (Hedis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id IJCommon.Person ->
  m BM.MetricContext
loadBehaviourRates personId = do
  cancel <-
    BTAcc.buildCounterValues
      True
      BTT.DRIVER
      "RIDE_CANCELLATION"
      personId.getId
      defaultRatePeriodDays
      defaultRateWindowDays
  accept <-
    BTAcc.buildCounterValuesWithEligible
      True
      BTT.DRIVER
      "QUOTE_RESPONSE_ACCEPT"
      (Just "QUOTE_RESPONSE")
      personId.getId
      defaultRatePeriodDays
      defaultRateWindowDays
  pure
    BM.MetricContext
      { vars =
          HM.fromList
            [ (BM.varCancellationRate, fromIntegral cancel.rate),
              (BM.varAcceptanceRate, fromIntegral accept.rate)
            ]
      }
