{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Post-transaction dispatch, run automatically after every successful 'runFinance' in this app.
--   Call sites must import 'runFinance' from here, not Lib.Finance directly, or dispatch is silently skipped.
module SharedLogic.Finance.PostActions
  ( runFinance,
    runPostActionsForAccount,
  )
where

import Data.List (nubBy)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, logError)
import qualified Lib.Finance as Finance
import qualified Lib.Finance.Domain.Types.Account as FAccount
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified SharedLogic.VehicleServiceTier as SVST

-- | Same shape as the kernel's own runFinance -- call sites don't change their call pattern.
--   Reads ctx.enableWalletGatedTierCheck (caller-resolved) to gate the recheck below.
runFinance ::
  (BeamFlow m r, Finance.HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Finance.FinanceCtx ->
  Finance.FinanceM m a ->
  m (Either Finance.FinanceError (a, [Id Finance.LedgerEntry]))
runFinance ctx action = do
  result <- Finance.runFinanceWithState ctx action
  case result of
    Left err -> pure (Left err)
    Right (a, finalState) -> do
      runPostActions ctx.enableWalletGatedTierCheck finalState `catchAny` \e ->
        logError ("PostActions failed after runFinance, ignoring: " <> show e)
      pure (Right (a, finalState.collectedEntryIds))

-- | Runs 'runPostActionsForAccount' against every account a runFinance call touched.
runPostActions ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Bool ->
  Finance.FinanceState ->
  m ()
runPostActions walletGateEnabled finalState =
  forM_ (nubBy (\a b -> a.id == b.id) finalState.affectedAccounts) $ \account ->
    runPostActionsForAccount walletGateEnabled account `catchAny` \e ->
      logError ("PostActions: processing accountId=" <> account.id.getId <> " failed, skipping: " <> show e)

-- | Like 'runPostActions' but for a single, already-known account -- the entry point for
--   SharedLogic.Finance.Wallet.createWalletEntryDelta, which never goes through runFinance.
runPostActionsForAccount ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Bool ->
  Finance.AffectedAccount ->
  m ()
runPostActionsForAccount walletGateEnabled account =
  when walletGateEnabled $
    case (account.accountType, account.counterpartyType, account.counterpartyId) of
      (FAccount.Liability, Just FAccount.DRIVER, Just driverIdText) ->
        SVST.checkAndAutoDisableWalletGatedTiers (Id driverIdText) (Id account.merchantOperatingCityId)
      _ -> pure ()
