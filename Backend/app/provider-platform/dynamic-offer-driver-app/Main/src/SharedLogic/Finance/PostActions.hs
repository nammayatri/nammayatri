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
    PostActionsEnv,
  )
where

import Data.List (nubBy)
import qualified Domain.Types.TransporterConfig as DTC
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fork, logError)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance as Finance
import qualified Lib.Finance.Domain.Types.Account as FAccount
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow as IJBeamFlow
import SharedLogic.Finance.WalletBalanceCache (syncWalletBalanceToPool)
import SharedLogic.DriverPool.DriverPoolStateEvent (DriverPoolStateEnv)
import qualified SharedLogic.Finance.WalletFloor as WalletFloor
import qualified SharedLogic.VehicleServiceTier as SVST
import Storage.Beam.IncentiveJourney ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

-- | What the hook needs beyond the finance kernel: taking a driver offline below the wallet floor
--   changes driver mode (ClickHouse fleet counters + IJ onlineMinutes closed-session publish).
type PostActionsEnv r =
  ( HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "ijBehaviorTickPublisherCfg" r (Maybe Environment.RideEventsPublisherCfg)
  )

-- | Same shape as the kernel's own runFinance -- call sites don't change their call pattern.
--   Reads ctx.enableWalletGatedTierCheck (caller-resolved) to gate the recheck below.
runFinance ::
  ( BeamFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    PostActionsEnv r,
    IJBeamFlow.BeamFlow m r,
    Metrics.CoreMetrics m,
    DriverPoolStateEnv r
  ) =>
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
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    PostActionsEnv r,
    IJBeamFlow.BeamFlow m r,
    Metrics.CoreMetrics m,
    DriverPoolStateEnv r
  ) =>
  Bool ->
  Finance.FinanceState ->
  m ()
runPostActions walletGateEnabled finalState = do
  let accounts = nubBy (\a b -> a.id == b.id) finalState.affectedAccounts
  -- The pool caches driver wallet balances only where the wallet is on (it reads the cache only where
  -- the minimum can be enforced); the city's config is read only when a driver wallet moved.
  mbWalletCityConfig <- case find isDriverWallet accounts of
    Nothing -> pure Nothing
    Just account ->
      (mfilter (.driverWalletConfig.enableDriverWallet) <$> getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = account.merchantOperatingCityId}) Nothing) `catchAny` \e -> do
        logError ("PostActions: wallet config read failed for city " <> account.merchantOperatingCityId <> ", skipping the pool balance sync: " <> show e)
        pure Nothing
  forM_ accounts $ \account ->
    runPostActionsForAccount walletGateEnabled mbWalletCityConfig account `catchAny` \e ->
      logError ("PostActions: processing accountId=" <> account.id.getId <> " failed, skipping: " <> show e)

-- | Like 'runPostActions' but for a single, already-known account. No callers remain
--   outside this module (createWalletEntryDelta now goes through runFinance); kept as
--   the single-account entry point for future direct dispatch needs.
runPostActionsForAccount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    PostActionsEnv r,
    IJBeamFlow.BeamFlow m r,
    Metrics.CoreMetrics m,
    DriverPoolStateEnv r
  ) =>
  Bool -> -- caller-resolved driverWalletConfig.enableWalletGatedTierCheck
  Maybe DTC.TransporterConfig -> -- the city's config when it has driverWalletConfig.enableDriverWallet
  Finance.AffectedAccount ->
  m ()
runPostActionsForAccount walletGateEnabled mbWalletCityConfig account = do
  -- Only driver wallets are cached in the pool (fleet owner postings never change it).
  whenJust mbWalletCityConfig $ \transporterConfig ->
    when (isDriverWallet account) $ do
      syncWalletBalanceToPool account.id `catchAny` \e ->
        logError ("PostActions: wallet balance cache sync failed for accountId=" <> account.id.getId <> ", ignoring: " <> show e)
      -- The pool stops offering rides as soon as the balance is below the minimum; take an online,
      -- free driver offline too, after whichever posting crossed it. A driver on a ride is skipped and
      -- re-checked by the ride-end postings.
      whenJust account.counterpartyId $ \driverId ->
        fork "wallet floor after posting" $
          WalletFloor.enforceWalletFloorForOwner transporterConfig FAccount.DRIVER driverId
  when walletGateEnabled $
    case (account.accountType, account.counterpartyType, account.counterpartyId) of
      (FAccount.Liability, Just FAccount.DRIVER, Just driverIdText) ->
        SVST.checkAndAutoDisableWalletGatedTiers (Id driverIdText) (Id account.merchantOperatingCityId)
      _ -> pure ()

isDriverWallet :: Finance.AffectedAccount -> Bool
isDriverWallet account = account.accountType == FAccount.Liability && account.counterpartyType == Just FAccount.DRIVER
