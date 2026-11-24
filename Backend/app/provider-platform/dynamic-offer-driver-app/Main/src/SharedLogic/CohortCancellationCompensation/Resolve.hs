{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Cycle-free resolver for the cohort rider-cancellation compensation amount. Kept in its
-- own module (depends only on the incentive-journey adapter, NOT on CancellationOrchestrator)
-- so both the orchestrator (to suppress the matrix's generic compensation when a campaign
-- overrides it) and SharedLogic.CohortCancellationCompensation (to pay it) can use it.
module SharedLogic.CohortCancellationCompensation.Resolve
  ( resolveRiderCancellationCompensation,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Assignment as IJA
import qualified Lib.IncentiveJourney.Domain.Types.AdditionalRewards as AR
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified SharedLogic.IncentiveJourney as SLJourney

-- | The rider-cancellation compensation payout for a driver: the max @payout@ across the
-- driver's active cohorts (UCM or auto-apply) whose campaign is enabled and whose
-- @additionalRewards.riderCancellationCompensation@ is enabled. Nothing when none apply.
resolveRiderCancellationCompensation ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  m (Maybe HighPrecMoney)
resolveRiderCancellationCompensation driverId = do
  assignments <- SLJourney.findAssignmentsByUserId driverId
  let payouts :: [HighPrecMoney]
      payouts =
        [ cfg.payout
          | assignment <- assignments,
            let cjm = assignment.cohortJourneyMapping,
            cjm.enabled,
            Just cfg <- [AR.parseRiderCancellationCompensation cjm.additionalRewards],
            cfg.enabled,
            -- Guard against a misconfigured negative payout: a negative amount would flip to
            -- a wallet DEBIT downstream (negate → positive), charging the driver on a
            -- rider-fault cancel. Only positive payouts are valid compensation.
            cfg.payout > 0
        ]
  pure $ if null payouts then Nothing else Just (maximum payouts)
