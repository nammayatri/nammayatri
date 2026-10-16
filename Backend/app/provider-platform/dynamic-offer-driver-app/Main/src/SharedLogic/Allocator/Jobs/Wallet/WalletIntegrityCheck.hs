{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Daily wallet integrity check for a wallet-subscription city, scheduled after the nightly fee
--   run. It only ever repairs by re-running an idempotent step -- it never edits a balance:
--
--   1. end-ride finance jobs still pending an hour on (failed past their retry budget) are
--      enqueued again;
--   2. every fee marked collected from the wallet in the window must have all of its debit entries
--      (fee, GST, folded penalty); any missing one is posted, so a debit that stopped half way is
--      completed; and a recent day fee the nightly run left pending because its
--      wallet debit failed is debited again;
--   3. the pool's cached wallet balance is corrected where it drifted.
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
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Sequelize as Se
import SharedLogic.Allocator
import qualified SharedLogic.DriverPool.DriverPoolData as DPD
import SharedLogic.Finance.WalletAccount (isWalletSubscriptionCity)
import SharedLogic.Finance.WalletBalanceCache (setDriverWalletBalanceInPool)
import SharedLogic.Finance.WalletEndRidePending
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
    Redis.HedisLTSFlowEnv r,
    JobCreatorEnv r,
    HasField "schedulerType" r SchedulerType
  ) =>
  Job 'WalletIntegrityCheck ->
  m ExecutionResult
walletIntegrityCheck Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      cityId = jobData.merchantOperatingCityId
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound cityId.getId)
  if not (isWalletSubscriptionCity transporterConfig.driverWalletConfig)
    then pure Complete
    else do
      now <- getCurrentTime
      -- 1. End-ride finance stuck past its retry budget.
      stale <- findStaleEndRideFinance cityId (addUTCTime (-3600) now)
      forM_ stale $ \(rideId, bookingId) -> do
        logError $ "WalletIntegrity: end-ride finance still pending for ride " <> rideId <> "; enqueuing again"
        createJobIn @_ @'ProcessEndRideFinance (Just jobData.merchantId) (Just cityId) 0 $
          ProcessEndRideFinanceJobData {rideId = Id rideId, bookingId = Id bookingId}
        markEndRideFinancePending cityId rideId bookingId

      -- 2. Wallet-collected fees must have their debit.
      let checkFees offset = do
            -- collectedAt is true UTC while the fee window (startTime/endTime) is local-shifted, and the
            -- nightly debit lands after the window closes: bound by actual collection time instead.
            let collectedFrom = addUTCTime (negate $ secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) jobData.startTime
            fees <- QDF.findWalletCollectedInRange cityId WalletSubscription.walletCollectedBy collectedFrom now pageSize offset
            forM_ fees $ \fee -> do
              incomplete <- WalletSubscription.walletDebitIncomplete fee
              when incomplete $ do
                logError $ "WalletIntegrity: fee " <> fee.id.getId <> " is collected from the wallet but its debit is missing or partial; posting what is missing"
                WalletSubscription.debitDriverFeeFromWallet transporterConfig WalletSubscription.DailySubscription fee
            when (length fees == pageSize) $ checkFees (offset + pageSize)
      checkFees 0
      retryFailedNightlyDebits transporterConfig cityId jobData.startTime

      -- 3. Pool-cache drift: the cached wallet balance must match the ledger.
      let correctDrift offset = do
            accounts <- findCityWalletAccounts cityId.getId pageSize offset
            correctDriftPage accounts
            when (length accounts == pageSize) $ correctDrift (offset + pageSize)
      correctDrift 0
      pure Complete
  where
    correctDriftPage accounts = do
      let driverAccounts = [(Id ownerId :: Id DP.Person, account.balance) | account <- accounts, account.counterpartyType == Just FAccount.DRIVER, Just ownerId <- [account.counterpartyId]]
      poolEntries <- if null driverAccounts then pure [] else DPD.getDriverPoolDataBatch (cast . fst <$> driverAccounts)
      let poolByDriver = HashMap.fromList [(cast e.driverId :: Id DP.Person, e) | e <- poolEntries]
      forM_ driverAccounts $ \(driverId, balance) ->
        whenJust (HashMap.lookup driverId poolByDriver) $ \entry ->
          when (entry.walletBalance /= Just balance) $ do
            logWarning $ "WalletIntegrity: pool wallet balance drifted for driver " <> driverId.getId <> "; correcting"
            setDriverWalletBalanceInPool driverId balance

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

-- | A city's driver wallets (Liability accounts without a sub-ledger), paged.
findCityWalletAccounts :: (BeamFlow m r) => Text -> Int -> Int -> m [FAccount.Account]
findCityWalletAccounts cityId limit offset =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamAccount.merchantOperatingCityId $ Se.Eq cityId,
          Se.Is BeamAccount.accountType $ Se.Eq FAccount.Liability,
          Se.Is BeamAccount.counterpartyType $ Se.Eq (Just FAccount.DRIVER),
          Se.Is BeamAccount.subLedger $ Se.Eq Nothing
        ]
    ]
    (Se.Asc BeamAccount.id)
    (Just limit)
    (Just offset)
