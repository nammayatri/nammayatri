{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Cohort perk: when a rider cancels and the fault verdict is CustomerAtFault (the
-- rider is at fault), a driver who belongs to a cohort with @riderCancellationCompensation@
-- enabled is credited a wallet compensation. It reuses the existing fault verdict (no new
-- rule), the incentive-journey cohort membership, and the driver wallet-credit rail. The
-- campaign reward OVERRIDES any generic consequence-matrix driver compensation: the
-- orchestrator suppresses the matrix credit when a campaign applies, so the driver gets
-- exactly the campaign payout as a single credit (no stacking).
module SharedLogic.CohortCancellationCompensation
  ( tryAwardCohortCancellationCompensation,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import SharedLogic.CancellationOrchestrator (CancellationDecision (..), ConsequenceCtx (..))
import qualified SharedLogic.DriverCancellationPenalty as DCP
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Finance.PostActions (PostActionsEnv)
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Tools.Metrics as Metrics
import TransactionLogs.Types (KeyConfig, TokenConfig)

-- | Pay the driver a cohort-configured wallet compensation when a rider-fault cancel
-- occurs. No-op unless the verdict is CustomerAtFault and the driver is in a cohort whose
-- @riderCancellationCompensation@ is enabled. The matrix's generic driver compensation is
-- suppressed upstream when this applies, so the campaign payout is the sole credit.
-- Safe to fork — any failure is contained by the caller and it never blocks the cancel.
tryAwardCohortCancellationCompensation ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    TranslateFlow m r,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasShortDurationRetryCfg r c,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r,
    Finance.HasActorInfo m r,
    PostActionsEnv r
  ) =>
  ConsequenceCtx ->
  m ()
tryAwardCohortCancellationCompensation ctx =
  -- The payout is resolved ONCE in decideCancellationConsequences (only on a CustomerAtFault
  -- cancel, driver in an enabled cohort, positive payout) and threaded on the decision. The
  -- same value drives the orchestrator's matrix-suppression, so suppress and pay stay in
  -- lockstep — no second cohort read, no cross-fork race. CAMPAIGN OVERRIDE: the matrix's own
  -- CustomerAtFault driver compensation is suppressed upstream, so this is the sole credit.
  whenJust ctx.decision.cohortCancellationPayout $ \amount -> do
    let isWalletEnabled = fromMaybe False ctx.merchant.prepaidSubscriptionAndWalletEnabled || ctx.transporterConfig.driverWalletConfig.enableDriverWallet
    logInfo $ "Cohort rider-cancellation compensation (override): crediting driver " <> ctx.ride.driverId.getId <> " amount " <> show amount <> " for ride " <> ctx.ride.id.getId
    -- negative signed amount routes to the wallet-credit branch; no-ops (logs) when
    -- wallet is disabled for the merchant.
    DCP.accumulateCancellationPenalty isWalletEnabled ctx.booking ctx.ride (Just (negate amount)) ctx.transporterConfig ctx.driver
