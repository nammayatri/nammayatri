{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.CalculatePayoutVendorSettlement where

import qualified Data.Map.Strict as Map
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PayoutInstanceCalculation as DPIC
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.VendorFee as DVF
import qualified Domain.Types.VendorSettlement as DVS
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified SharedLogic.DriverFee as SLDriverFee
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverFeeExtra as QDFE
import qualified Storage.Queries.PayoutInstanceCalculation as QPIC
import qualified Storage.Queries.VendorFee as QVF
import qualified Storage.Queries.VendorFeeExtra as QVFE
import qualified Storage.Queries.VendorSettlement as QVS
import Tools.Error

-- Job handler for Step 1: Process Cash Collected Fees
calculatePayoutVendorSettlementCashCollected ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    JobCreatorEnv r
  ) =>
  Job 'VendorPayoutSettlementCashCollected ->
  m ExecutionResult
calculatePayoutVendorSettlementCashCollected Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOpCityId = jobData.merchantOperatingCityId
      startTime = jobData.startTime
      endTime = jobData.endTime
      vendorId = jobData.vendorId

  logInfo $ "Processing Step 1: Cash Collected Fees for vendorId: " <> vendorId

  -- Step 1 Collected Cash

  ---------------- Get subscription config for batch size ----------------
  subscriptionConfig <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId Nothing DPlan.YATRI_SUBSCRIPTION >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show DPlan.YATRI_SUBSCRIPTION)
  let batchSize = Just subscriptionConfig.genericBatchSizeForJobs

  cashCollectedDriverFees <- QDFE.findAllCashCollectedFeesInRangeWithStatusAndServiceName (Just merchantId) merchantOpCityId startTime endTime [DDF.COLLECTED_CASH] batchSize DPlan.YATRI_SUBSCRIPTION vendorId
  logInfo $ "Found " <> show (length cashCollectedDriverFees) <> " cash-collected driver fees to process"

  now <- getCurrentTime

  forM_ cashCollectedDriverFees $ \driverFee -> do
    vendorFeesForThisDriverFee <- QVF.findAllByDriverFeeId driverFee.id
    logInfo $ "Found " <> show (length vendorFeesForThisDriverFee) <> " vendor fee entries for driver fee " <> driverFee.id.getId

    let unprocessedVendorFees = filter (\vf -> isNothing vf.isVendorFeeProcessedAt) vendorFeesForThisDriverFee
    logInfo $ "Unprocessed vendor fees: " <> show (length unprocessedVendorFees) <> " out of " <> show (length vendorFeesForThisDriverFee)

    let totalVendorFeesAmount = sum $ map (.amount) vendorFeesForThisDriverFee
        driverFeeAmount = SLDriverFee.roundToHalf driverFee.currency (driverFee.govtCharges + driverFee.platformFee.fee + driverFee.platformFee.cgst + driverFee.platformFee.sgst + fromMaybe 0 driverFee.cancellationPenaltyAmount)
        marketPlaceAmount = totalVendorFeesAmount - driverFeeAmount

    logInfo $ "Driver fee " <> driverFee.id.getId <> ": totalVendorFees=" <> show totalVendorFeesAmount <> ", driverFee=" <> show driverFeeAmount <> ", marketPlace=" <> show marketPlaceAmount

    let marketPlaceIdempotencyKey = "MarketPlace:CashSettlement:" <> driverFee.id.getId <> ":" <> show startTime <> ":" <> show endTime

    -- Process unprocessed vendor fees
    forM_ unprocessedVendorFees $ \vendorFee -> do
      let idempotencyKey = "VendorFee:CashSettlement:" <> vendorFee.driverFeeId.getId <> ":" <> vendorFee.vendorId <> ":" <> show startTime <> ":" <> show endTime
      withIdempotencyKey
        idempotencyKey
        3600
        ( when (vendorFee.vendorId /= vendorId) $ do
            logInfo $ "Creating/Updating instance calc entry for vendor fee: " <> vendorId <> " → " <> vendorFee.vendorId <> ": " <> show vendorFee.amount
            createOrUpdateInstanceCalcEntry merchantId merchantOpCityId vendorId vendorFee.vendorId vendorFee.amount startTime endTime now
        )
        (QVF.updateIsVendorFeeProcessedAt (Just now) vendorFee.driverFeeId vendorFee.vendorId)

    -- update marketPlace amount
    withIdempotencyKey
      marketPlaceIdempotencyKey
      3600
      ( do
          when (marketPlaceAmount < 0) $ do
            logError $ "Negative market place amount detected for driver fee " <> driverFee.id.getId <> ": " <> show marketPlaceAmount
            throwError $ InternalError $ "Negative market place amount detected for driver fee " <> driverFee.id.getId <> ": " <> show marketPlaceAmount
          logInfo $ "Creating/Updating MARKET_PLACE entry: " <> vendorId <> " → MARKET_PLACE: " <> show marketPlaceAmount
          createOrUpdateInstanceCalcEntry merchantId merchantOpCityId vendorId "MARKET_PLACE" marketPlaceAmount startTime endTime now
      )
      (QDF.updateDriverConsideredInPayoutSettlementAt (Just now) driverFee.id)

  case listToMaybe cashCollectedDriverFees of
    Nothing -> do
      logInfo $ "No cash collected driver fees found. Scheduling Step 2: Online Collection for vendorId: " <> vendorId
      createJobIn @_ @'VendorPayoutSettlementOnlineCollection (Just merchantId) (Just merchantOpCityId) 0 $
        VendorPayoutSettlementOnlineCollectionJobData
          { merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            vendorId = vendorId,
            startTime = startTime,
            endTime = endTime
          }
      return Complete
    _ -> do
      logInfo $ "Processed " <> show (length cashCollectedDriverFees) <> " cash collected driver fees. Rescheduling to check for more records."
      ReSchedule <$> getRescheduledTime 5

-- Job handler for Step 2: Process Online Collections
calculatePayoutVendorSettlementOnlineCollection ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    JobCreatorEnv r
  ) =>
  Job 'VendorPayoutSettlementOnlineCollection ->
  m ExecutionResult
calculatePayoutVendorSettlementOnlineCollection Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOpCityId = jobData.merchantOperatingCityId
      startTime = jobData.startTime
      endTime = jobData.endTime
      vendorId = jobData.vendorId

  logInfo $ "Processing Step 2: Online Collections for vendorId: " <> vendorId

  ------------------------------- Step 2 : Process Online Collections-------------------

  -- find vendor Fee whose splitType Payout  and isVendorFeeProcessedAt is Nothing
  -- fetch DriverFee Id from vendorFee and check if driverFee status is cleared
  -- now keep only those vfee whose dfee status was cleared
  -- create queries in VendorFeeExtra and DriverFeeExtra if not already present

  ---------------- Get subscription config for batch size ----------------
  subscriptionConfig <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId Nothing DPlan.YATRI_SUBSCRIPTION >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show DPlan.YATRI_SUBSCRIPTION)
  let batchSize = Just subscriptionConfig.genericBatchSizeForJobs

  unprocessedPayoutVendorFees <- QVFE.findUnprocessedPayoutVendorFeesByVendorId vendorId batchSize

  logInfo $ "Found " <> show (length unprocessedPayoutVendorFees) <> " unprocessed PAYOUT vendor fees"

  -- Extract driver fee IDs and check their status
  let driverFeeIds = map (.driverFeeId) unprocessedPayoutVendorFees
  driverFees <- mapM QDF.findById driverFeeIds
  let clearedDriverFees = [dfee | Just dfee <- driverFees, dfee.status == DDF.CLEARED]
      clearedDriverFeeIds = map (.id) clearedDriverFees

  logInfo $ "Found " <> show (length clearedDriverFees) <> " driver fees with CLEARED status"

  -- Filter vendor fees to only those whose driver fees are cleared
  let clearedPayoutVendorFees = filter (\vf -> vf.driverFeeId `elem` clearedDriverFeeIds) unprocessedPayoutVendorFees

  logInfo $ "Processing " <> show (length clearedPayoutVendorFees) <> " vendor fees with cleared driver fees"

  now <- getCurrentTime

  --- Process each vendor fee individually with Redis-based idempotency
  forM_ clearedPayoutVendorFees $ \vendorFee -> do
    let idempotencyKey = "VendorFee:Processed:" <> vendorFee.driverFeeId.getId <> ":" <> vendorFee.vendorId <> ":" <> show startTime <> ":" <> show endTime
    withIdempotencyKey
      idempotencyKey
      3600
      (createOrUpdateInstanceCalcEntry merchantId merchantOpCityId "NAMMAYATRI" vendorId vendorFee.amount startTime endTime now)
      (QVF.updateIsVendorFeeProcessedAt (Just now) vendorFee.driverFeeId vendorFee.vendorId)

  -- Reschedule if we found any vendor fees, indicating there might be more records
  case listToMaybe unprocessedPayoutVendorFees of
    Nothing -> do
      logInfo $ "No unprocessed vendor fees found. Starting Step 3 & 4: NY Settlement Calculation"
      -- STEP 3: Calculate NY Settlement
      -- 3.1 & 3.2: Fetch instance calculations and calculate net amount
      mbNyToVendor <- QPIC.findByFromAndToVendorId "NAMMAYATRI" vendorId startTime endTime
      mbVendorToNy <- QPIC.findByFromAndToVendorId vendorId "NAMMAYATRI" startTime endTime

      let nyToVendorAmount = maybe 0 (.instanceBalance) mbNyToVendor
          vendorToNyAmount = maybe 0 (.instanceBalance) mbVendorToNy
          netAmount = nyToVendorAmount - vendorToNyAmount

      logInfo $ "NY→Vendor: " <> show nyToVendorAmount <> ", Vendor→NY: " <> show vendorToNyAmount <> ", Net: " <> show netAmount

      -- 3.3: Fetch previous SettleInNext amount (chaining logic)
      mbPreviousSIN <- QVS.findLatestSettleInNextByVendor "NAMMAYATRI" vendorId
      let previousSettleInNext = maybe 0 (.runningBalance) mbPreviousSIN
      let runningBalance = netAmount + previousSettleInNext

      logInfo $ "Final running balance: " <> show runningBalance

      -- STEP 4: Create Payout Entry (VendorSettlement) based on settlement decision
      let paymentIdempotencyKey = "VendorSettlement:Payment:" <> vendorId <> ":" <> show startTime <> ":" <> show endTime

      settlementId <- generateGUID
      let initialStatus = if runningBalance > 0 then DVS.Pending else if runningBalance == 0 then DVS.Settled else DVS.SettleInNext
          settlementMode = if runningBalance > 0 then DVS.Online else DVS.Manual

      -- Step 1: Create settlement entry with initial status
      QVS.create
        DVS.VendorSettlement
          { id = settlementId,
            fromVendorId = "NAMMAYATRI",
            toVendorId = vendorId,
            runningBalance,
            status = initialStatus,
            settlementMode,
            settlementDate = if runningBalance == 0 then Just now else Nothing,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

      logInfo $ "Created VendorSettlement entry with status: " <> show initialStatus

      -- Step 2: For positive balance, call payment API (idempotent) and update status
      when (runningBalance > 0) $
        withIdempotencyKey
          paymentIdempotencyKey
          3600
          ( do
              logInfo $ "Calling payment API for vendor settlement: " <> show runningBalance
              -- TODO: Integrate Juspay payout API call here
              logInfo $ "Payment API call successful (mocked)"

              -- Update settlement to SETTLED after payment succeeds
              QVS.updateStatus settlementId DVS.Settled now
              QVS.updateSettlementDate settlementId (Just now)
              logInfo $ "Updated settlement status to SETTLED for settlementId: " <> settlementId.getId
          )
          (pure ())

      -- Step 3: Update all previous SettleInNext entries to SETTLED (only if current is SETTLED)
      finalStatus <- if runningBalance > 0 then runInMasterDbAndRedis $ QVS.findByPrimaryKey "NAMMAYATRI" settlementId vendorId >>= \mb -> return $ maybe DVS.Pending (.status) mb else return initialStatus
      when (finalStatus == DVS.Settled) $ do
        QVS.updateAllSettleInNextToSettled "NAMMAYATRI" vendorId now
        logInfo $ "Marked all previous SettleInNext entries as Settled for vendorId: " <> vendorId

      logInfo $ "Completed Step 3 & 4: NY Settlement for vendorId: " <> vendorId

      return Complete
    _ -> do
      logInfo $ "Processed " <> show (length unprocessedPayoutVendorFees) <> " vendor fees. Rescheduling to check for more records."
      ReSchedule <$> getRescheduledTime 5 -- Reschedule after 5 seconds

getRescheduledTime :: (MonadFlow m) => NominalDiffTime -> m UTCTime
getRescheduledTime gap = addUTCTime gap <$> getCurrentTime

-- Build a map of obligations: vendorId -> total amount owed
buildObligationsMap :: [DVF.VendorFee] -> Map.Map Text Common.HighPrecMoney
buildObligationsMap vendorFees =
  Map.fromListWith (+) [(vf.vendorId, vf.amount) | vf <- vendorFees]

-- Generic idempotency wrapper using Redis setNxExpire
-- Executes protectedAction only if idempotency key was set successfully (first time)
-- Always executes alwaysAction regardless of whether key was set or not
withIdempotencyKey ::
  ( CacheFlow m r,
    MonadFlow m
  ) =>
  Text -> -- idempotency key
  Int -> -- TTL in seconds
  m () -> -- protected action (runs only if key was set for first time)
  m () -> -- always action (runs regardless of idempotency check)
  m ()
withIdempotencyKey key ttl protectedAction alwaysAction = do
  keyWasSet <- Hedis.setNxExpire key ttl True
  when keyWasSet protectedAction
  alwaysAction

createOrUpdateInstanceCalcEntry ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  Common.HighPrecMoney ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  m ()
createOrUpdateInstanceCalcEntry merchantId merchantOpCityId fromVendorId toVendorId amount startTime endTime now = do
  let cacheKey = "PayoutInstanceCalc:Amount:" <> fromVendorId <> ":" <> toVendorId <> ":" <> show startTime <> ":" <> show endTime
      lockKey = "PayoutInstanceCalc:Init:" <> fromVendorId <> ":" <> toVendorId <> ":" <> show startTime <> ":" <> show endTime
      updateBalance newBal = QPIC.updateInstanceBalanceByVendorIds fromVendorId toVendorId startTime endTime (Common.HighPrecMoney (toRational newBal)) now

  (mbCached :: Maybe Double) <- Hedis.get cacheKey
  case mbCached of
    Just _ -> Hedis.incrByFloat cacheKey (realToFrac amount) >>= updateBalance
    Nothing -> Hedis.whenWithLockRedis lockKey 60 $ do
      (mbRecheck :: Maybe Double) <- Hedis.get cacheKey
      case mbRecheck of
        Just _ -> Hedis.incrByFloat cacheKey (realToFrac amount) >>= updateBalance
        Nothing -> do
          mbDbInstance <- runInMasterDbAndRedis $ QPIC.findByFromAndToVendorId fromVendorId toVendorId startTime endTime
          case mbDbInstance of
            Just existing -> do
              Hedis.setExp cacheKey (realToFrac existing.instanceBalance :: Double) 3600
              Hedis.incrByFloat cacheKey (realToFrac amount) >>= updateBalance
            Nothing -> do
              instanceId <- generateGUID
              QPIC.create
                DPIC.PayoutInstanceCalculation
                  { id = instanceId,
                    fromVendorId,
                    toVendorId,
                    instanceBalance = amount,
                    startTime,
                    endTime,
                    merchantId = Just merchantId,
                    merchantOperatingCityId = Just merchantOpCityId,
                    createdAt = now,
                    updatedAt = now
                  }
              Hedis.setExp cacheKey (realToFrac amount :: Double) 3600
