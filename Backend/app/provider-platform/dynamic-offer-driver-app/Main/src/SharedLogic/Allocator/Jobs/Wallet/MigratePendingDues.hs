{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | One-time move of a city's unpaid subscription dues into the driver wallets, so each driver
--   sees them as a negative balance. Safe to re-run: a moved fee is CLEARED (and its debit is
--   reference-checked), and a due whose Autopay charge is in flight is left until it settles.
module SharedLogic.Allocator.Jobs.Wallet.MigratePendingDues
  ( migratePendingDuesToWallet,
    markDuesMigrationRunning,
  )
where

import qualified Data.List as DL
import qualified Domain.Types.DriverFee as DDF
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Plan (ServiceNames (YATRI_SUBSCRIPTION))
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Finance.WalletAccount (isWalletSubscriptionCity)
import qualified SharedLogic.Finance.WalletFloor as WalletFloor
import qualified SharedLogic.Finance.WalletSubscription as WalletSubscription
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson

duesMigrationBatchSize :: Int
duesMigrationBatchSize = 200

-- | One migration per city at a time: the trigger API takes this key and the job keeps it alive on
--   every run and drops it when it ends. A job that dies is let go when the key expires. Shared across
--   apps (cross-app Redis), since the API runs in driver-app and the job in the allocator.
duesMigrationRunningKey :: Id MerchantOperatingCity -> Text
duesMigrationRunningKey cityId = "WalletDuesMigration:Running:" <> cityId.getId

-- | Longer than the job's longest wait between runs (an hour while Autopay charges are in flight).
duesMigrationRunningTtl :: Int
duesMigrationRunningTtl = 2 * 3600

-- | Take the city's migration slot; False when a migration is already running there.
markDuesMigrationRunning :: (MonadFlow m, Redis.HedisFlow m r) => Id MerchantOperatingCity -> m Bool
markDuesMigrationRunning cityId = Redis.withCrossAppRedis $ Redis.setNxExpire (duesMigrationRunningKey cityId) duesMigrationRunningTtl True

refreshDuesMigrationRunning :: (MonadFlow m, Redis.HedisFlow m r) => Id MerchantOperatingCity -> m ()
refreshDuesMigrationRunning cityId = Redis.withCrossAppRedis $ Redis.setExp (duesMigrationRunningKey cityId) True duesMigrationRunningTtl

clearDuesMigrationRunning :: (MonadFlow m, Redis.HedisFlow m r) => Id MerchantOperatingCity -> m ()
clearDuesMigrationRunning cityId = Redis.withCrossAppRedis $ Redis.del (duesMigrationRunningKey cityId)

migratePendingDuesToWallet ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Job 'MigratePendingDuesToWallet ->
  m ExecutionResult
migratePendingDuesToWallet Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  if not (isWalletSubscriptionCity transporterConfig.driverWalletConfig)
    then do
      clearDuesMigrationRunning merchantOpCityId
      pure $ Terminate "Wallet subscription is not on for this city; dues not moved."
    else do
      refreshDuesMigrationRunning merchantOpCityId
      candidates <- QDF.findPendingDuesForWalletMigration merchantOpCityId YATRI_SUBSCRIPTION duesMigrationBatchSize
      -- A due the driver is paying by link right now (order created, not yet processed) is left to that
      -- payment: moved later if the payment doesn't go through, never if it clears the fee.
      fees <- filterM (fmap not . isManualPaymentInProgress) candidates
      if null fees
        then do
          inFlight <- QDF.findInFlightDuesForWalletMigration merchantOpCityId YATRI_SUBSCRIPTION 1
          if null candidates && null inFlight
            then do
              logInfo $ "Wallet dues migration complete for city " <> merchantOpCityId.getId
              clearDuesMigrationRunning merchantOpCityId
              pure Complete
            else -- wait for in-flight Autopay charges and link payments to settle (paid -> nothing to move;
            -- failed -> moved next run). A link payment's marker lasts manualPaymentInProgressTtl.
              ReSchedule <$> getRescheduledTime (if null candidates then 3600 else fromIntegral DDF.manualPaymentInProgressTtl)
        else do
          -- One fee's failed debit must not stop the batch: it stays pending and is tried again next run.
          moved <- fmap catMaybes . forM fees $ \fee ->
            withTryCatch "debitDriverFeeFromWallet:migration" (WalletSubscription.debitDriverFeeFromWallet transporterConfig WalletSubscription.MigratedDues fee) >>= \case
              Left err -> do
                logError $ "Wallet dues migration: debit of fee " <> fee.id.getId <> " failed, retried next run: " <> show err
                pure Nothing
              Right () -> pure (Just fee)
          logInfo $ "Wallet dues migration for city " <> merchantOpCityId.getId <> ": moved " <> show (length moved) <> " of " <> show (length fees) <> " fees, amount " <> show (sum (feeTotal <$> moved))
          forM_ (DL.nub $ (.driverId) <$> moved) $ \driverId -> do
            mbDriver <- QPerson.findById (cast driverId)
            whenJust mbDriver $ \driver -> do
              -- Floor first (a blocked-but-online driver goes offline), then lift the old postpaid
              -- block once nothing of theirs is left on the old rail.
              WalletFloor.afterWalletDebit transporterConfig driver
              stillPending <- QDF.findAllFeeByTypeServiceStatusAndDriver YATRI_SUBSCRIPTION driverId [DDF.RECURRING_INVOICE, DDF.RECURRING_EXECUTION_INVOICE, DDF.CANCELLATION_PENALTY] [DDF.PAYMENT_PENDING, DDF.PAYMENT_OVERDUE]
              when (null stillPending) $ do
                QDI.updateSubscription True (cast driverId)
                QDI.updatePendingPayment False (cast driverId)
          -- A batch where every debit failed would only fail the same way again in 5s: back off.
          if null moved
            then do
              logError $ "Wallet dues migration for city " <> merchantOpCityId.getId <> ": no fee of the batch could be debited; retrying in an hour"
              ReSchedule <$> getRescheduledTime 3600
            else ReSchedule <$> getRescheduledTime 5
  where
    feeTotal fee = fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst + fee.govtCharges + fromMaybe 0 fee.cancellationPenaltyAmount
    getRescheduledTime gap = addUTCTime gap <$> getCurrentTime

isManualPaymentInProgress :: (CacheFlow m r, MonadFlow m) => DDF.DriverFee -> m Bool
isManualPaymentInProgress driverFee = do
  mbInProgress <- Redis.runInMasterCloudRedisCell $ Redis.get (DDF.manualPaymentInProgressKey driverFee.id.getId)
  pure $ mbInProgress == Just True
