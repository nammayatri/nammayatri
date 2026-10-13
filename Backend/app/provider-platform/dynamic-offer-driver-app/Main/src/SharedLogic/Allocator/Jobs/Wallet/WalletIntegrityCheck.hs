{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Daily integrity check for a city: scheduled after the nightly fee run in a wallet-subscription
--   city, and once a day from ride end in any other city with the wallet on. It only ever repairs by
--   re-running an idempotent step -- it never edits a balance:
--
--   1. (wallet-subscription cities) every fee marked collected from the wallet in the window must have all of its debit entries
--      (fee, GST, folded penalty); any missing one is posted, so a debit that stopped half way is
--      completed; and a recent day fee the nightly run left pending because its
--      wallet debit failed is debited again;
--   2. (cities with the wallet on) the pool's cached wallet balance is corrected where it drifted.
module SharedLogic.Allocator.Jobs.Wallet.WalletIntegrityCheck
  ( walletIntegrityCheck,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.Person as DP
import Domain.Types.Plan (ServiceNames (YATRI_SUBSCRIPTION))
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Beam.Functions (findAllWithOptionsKV)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Account as FAccount
import qualified Lib.Finance.Storage.Beam.Account as BeamAccount
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Finance.Storage.Queries.Account ()
import Lib.Scheduler
import qualified Sequelize as Se
import SharedLogic.Allocator
import qualified SharedLogic.DriverPool.DriverPoolData as DPD
import SharedLogic.Finance.Wallet (makeWalletRunningBalanceLockKey)
import SharedLogic.Finance.WalletAccount (getWalletBalanceByOwner, isWalletSubscriptionCity)
import SharedLogic.Finance.WalletBalanceCache (setDriverWalletBalanceInPool)
import qualified SharedLogic.Finance.WalletSubscription as WalletSubscription
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverFee as QDF

pageSize :: Int
pageSize = 500

walletIntegrityCheck ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Job 'WalletIntegrityCheck ->
  m ExecutionResult
walletIntegrityCheck Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      cityId = jobData.merchantOperatingCityId
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound cityId.getId)
  let dwc = transporterConfig.driverWalletConfig
  now <- getCurrentTime
  -- Scheduled only in a wallet-subscription city; a city that left it since has nothing to check.
  when (isWalletSubscriptionCity dwc) $ do
    -- 1. Wallet-collected fees must have their debit.
    let checkFees mbLastFeeId = do
          -- collectedAt is true UTC while the fee window (startTime/endTime) is local-shifted, and the
          -- nightly debit lands after the window closes: bound by actual collection time instead.
          let collectedFrom = addUTCTime (negate $ secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) jobData.startTime
          fees <- QDF.findWalletCollectedInRange cityId WalletSubscription.walletCollectedBy collectedFrom now pageSize mbLastFeeId
          -- One fee's failed repair must not stop the rest; it is checked again on the next run.
          forM_ fees $ \fee ->
            withTryCatch "walletIntegrity:repairFeeDebit" (repairFeeDebit fee) >>= \case
              Left err -> logError $ "WalletIntegrity: repairing the debit of fee " <> fee.id.getId <> " failed: " <> show err
              Right () -> pure ()
          when (length fees == pageSize) $ checkFees ((.id) <$> lastMay fees)
        repairFeeDebit fee = do
          incomplete <- WalletSubscription.walletDebitIncomplete fee
          when incomplete $ do
            logError $ "WalletIntegrity: fee " <> fee.id.getId <> " is collected from the wallet but its debit is missing or partial; posting what is missing"
            WalletSubscription.debitDriverFeeFromWallet transporterConfig WalletSubscription.DailySubscription fee
    checkFees Nothing
    retryFailedNightlyDebits transporterConfig cityId jobData.startTime

    -- 2. Pool-cache drift: the cached wallet balance must match the ledger.
    -- Keyset paging (accounts after the last id seen), not offset.
    let correctDrift mbLastId = do
          accounts <- findCityWalletAccounts cityId.getId pageSize mbLastId
          correctDriftPage accounts
          when (length accounts == pageSize) $ correctDrift ((.id.getId) <$> lastMay accounts)
    correctDrift Nothing
  pure Complete
  where
    correctDriftPage accounts = do
      let driverAccounts = [(Id ownerId :: Id DP.Person, account.balance) | account <- accounts, account.counterpartyType == Just FAccount.DRIVER, Just ownerId <- [account.counterpartyId]]
      poolEntries <- if null driverAccounts then pure [] else DPD.getDriverPoolDataBatch (cast . fst <$> driverAccounts)
      let poolByDriver = HashMap.fromList [(cast e.driverId :: Id DP.Person, e) | e <- poolEntries]
      forM_ driverAccounts $ \(driverId, balance) ->
        whenJust (HashMap.lookup driverId poolByDriver) $ \entry ->
          when (entry.walletBalance /= Just balance) $
            -- Under the wallet lock with the balance read again: a posting that landed after this page was
            -- read must not be overwritten with the older figure.
            Redis.withWaitAndLockRedis (makeWalletRunningBalanceLockKey driverId.getId) 10 1000 $ do
              currentBalance <- fromMaybe 0 <$> getWalletBalanceByOwner FAccount.DRIVER driverId.getId
              logWarning $ "WalletIntegrity: pool wallet balance drifted for driver " <> driverId.getId <> "; correcting"
              setDriverWalletBalanceInPool driverId currentBalance

-- | Day fees the nightly run left PAYMENT_PENDING because their wallet debit failed (the debit is
--   idempotent per fee, so trying again is safe). Only recent ones: older pending dues are the
--   dashboard's dues migration.
retryFailedNightlyDebits ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DTC.TransporterConfig ->
  Id MerchantOperatingCity ->
  UTCTime ->
  m ()
retryFailedNightlyDebits transporterConfig cityId windowStart = do
  pendingFees <- QDF.findPendingWalletDayFeesInCity cityId YATRI_SUBSCRIPTION (addUTCTime (negate $ 7 * 24 * 3600) windowStart) pageSize
  forM_ pendingFees $ \fee ->
    withTryCatch "walletIntegrity:retryFailedNightlyDebit" (WalletSubscription.debitDriverFeeFromWallet transporterConfig WalletSubscription.DailySubscription fee) >>= \case
      Left err -> logError $ "WalletIntegrity: nightly debit of fee " <> fee.id.getId <> " still failing: " <> show err
      Right () -> logInfo $ "WalletIntegrity: nightly debit of fee " <> fee.id.getId <> " retried and collected"

-- | A city's driver wallets (Liability accounts without a sub-ledger), one page of ids after the last
--   one seen.
findCityWalletAccounts :: (BeamFlow m r) => Text -> Int -> Maybe Text -> m [FAccount.Account]
findCityWalletAccounts cityId limit mbLastId =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamAccount.merchantOperatingCityId $ Se.Eq cityId,
          Se.Is BeamAccount.accountType $ Se.Eq FAccount.Liability,
          Se.Is BeamAccount.counterpartyType $ Se.Eq (Just FAccount.DRIVER),
          Se.Is BeamAccount.subLedger $ Se.Eq Nothing
        ]
          <> [Se.Is BeamAccount.id $ Se.GreaterThan lastId | Just lastId <- [mbLastId]]
    ]
    (Se.Asc BeamAccount.id)
    (Just limit)
    Nothing
