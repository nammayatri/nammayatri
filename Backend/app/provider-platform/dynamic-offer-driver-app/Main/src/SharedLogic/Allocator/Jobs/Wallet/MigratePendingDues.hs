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
    WalletDuesMigrationStatus (..),
    getWalletDuesMigrationStatus,
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

data WalletDuesMigrationStatus = WalletDuesMigrationStatus
  { feesMoved :: Int,
    amountMoved :: HighPrecMoney,
    driversMoved :: Int,
    pendingInFlight :: Int,
    remaining :: Int,
    completed :: Bool
  }
  deriving (Generic, Show)

duesMigrationBatchSize :: Int
duesMigrationBatchSize = 200

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
    then pure $ Terminate "Wallet subscription is not on for this city; dues not moved."
    else do
      fees <- QDF.findPendingDuesForWalletMigration merchantOpCityId YATRI_SUBSCRIPTION duesMigrationBatchSize
      if null fees
        then do
          inFlight <- QDF.findInFlightDuesForWalletMigration merchantOpCityId YATRI_SUBSCRIPTION 1
          if null inFlight
            then do
              void $ Redis.set (completedKey merchantOpCityId) True
              logInfo $ "Wallet dues migration complete for city " <> merchantOpCityId.getId
              pure Complete
            else -- wait for the in-flight Autopay charges to settle (paid -> nothing to move; failed -> moved next run)
              ReSchedule <$> getRescheduledTime 3600
        else do
          forM_ fees $ \fee -> do
            WalletSubscription.debitDriverFeeFromWallet transporterConfig WalletSubscription.MigratedDues fee
            void $ Redis.incr (feesMovedKey merchantOpCityId)
            let movedAmount = feeTotal fee
            void $ Redis.incrby (amountMovedPaiseKey merchantOpCityId) (round (movedAmount.getHighPrecMoney * 100))
          forM_ (DL.nub $ (.driverId) <$> fees) $ \driverId -> do
            mbDriver <- QPerson.findById (cast driverId)
            whenJust mbDriver $ \driver -> do
              -- Floor first (a blocked-but-online driver goes offline), then lift the old postpaid
              -- block once nothing of theirs is left on the old rail.
              WalletFloor.afterWalletDebit transporterConfig driver
              stillPending <- QDF.findAllFeeByTypeServiceStatusAndDriver YATRI_SUBSCRIPTION driverId [DDF.RECURRING_INVOICE, DDF.RECURRING_EXECUTION_INVOICE, DDF.CANCELLATION_PENALTY] [DDF.PAYMENT_PENDING, DDF.PAYMENT_OVERDUE]
              when (null stillPending) $ do
                QDI.updateSubscription True (cast driverId)
                QDI.updatePendingPayment False (cast driverId)
                void $ Redis.incr (driversMovedKey merchantOpCityId)
          ReSchedule <$> getRescheduledTime 5
  where
    feeTotal fee = fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst + fee.govtCharges + fromMaybe 0 fee.cancellationPenaltyAmount
    getRescheduledTime gap = addUTCTime gap <$> getCurrentTime

-- | Progress for Control Centre.
getWalletDuesMigrationStatus ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  m WalletDuesMigrationStatus
getWalletDuesMigrationStatus merchantOpCityId = do
  feesMoved <- fromMaybe 0 <$> Redis.get (feesMovedKey merchantOpCityId)
  amountPaise <- fromMaybe (0 :: Integer) <$> Redis.get (amountMovedPaiseKey merchantOpCityId)
  driversMoved <- fromMaybe 0 <$> Redis.get (driversMovedKey merchantOpCityId)
  completed <- fromMaybe False <$> Redis.get (completedKey merchantOpCityId)
  inFlight <- QDF.findInFlightDuesForWalletMigration merchantOpCityId YATRI_SUBSCRIPTION duesMigrationBatchSize
  remainingFees <- QDF.findPendingDuesForWalletMigration merchantOpCityId YATRI_SUBSCRIPTION duesMigrationBatchSize
  pure
    WalletDuesMigrationStatus
      { feesMoved,
        amountMoved = HighPrecMoney (fromIntegral amountPaise / 100),
        driversMoved,
        pendingInFlight = length inFlight,
        remaining = length remainingFees,
        completed
      }

feesMovedKey, amountMovedPaiseKey, driversMovedKey, completedKey :: Id MerchantOperatingCity -> Text
feesMovedKey cityId = "WalletDuesMigration:" <> cityId.getId <> ":feesMoved"
amountMovedPaiseKey cityId = "WalletDuesMigration:" <> cityId.getId <> ":amountMovedPaise"
driversMovedKey cityId = "WalletDuesMigration:" <> cityId.getId <> ":driversMoved"
completedKey cityId = "WalletDuesMigration:" <> cityId.getId <> ":completed"
