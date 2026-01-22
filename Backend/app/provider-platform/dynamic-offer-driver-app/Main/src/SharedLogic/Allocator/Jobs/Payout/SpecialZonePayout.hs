{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.SpecialZonePayout where

import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.ScheduledPayout as DSP
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payout.Types as PT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DLP
import Lib.Scheduler
import SharedLogic.Allocator
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ScheduledPayout as QSP
import qualified Storage.Queries.ScheduledPayoutExtra as QSPE
import qualified Storage.Queries.Vehicle as QV
import qualified Tools.Payout as TP

sendSpecialZonePayout ::
  ( EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r
  ) =>
  Job 'SpecialZonePayout ->
  m ExecutionResult
sendSpecialZonePayout Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let SpecialZonePayoutJobData {scheduledPayoutId} = jobInfo.jobData
  let lockKey = "payout:lock:" <> scheduledPayoutId.getId

  -- 1. Try to acquire Redis lock (5 min TTL)
  acquired <- Redis.tryLockRedis lockKey 300
  if not acquired
    then do
      logWarning $ "Could not acquire lock for payout: " <> show scheduledPayoutId
      pure Complete
    else do
      -- Process with lock, release on exit
      flip finally (Redis.unlockRedis lockKey) $ do
        -- 2. Fetch ScheduledPayout record
        mbScheduledPayout <- QSP.findById scheduledPayoutId

        case mbScheduledPayout of
          Nothing -> do
            logInfo $ "ScheduledPayout record not found for id: " <> show scheduledPayoutId
            pure Complete
          Just scheduledPayout -> do
            -- 3. Check status (idempotency + cancellation check)
            case scheduledPayout.status of
              DSP.FAILED -> do
                logInfo $ "Payout was cancelled/failed, skipping: " <> show scheduledPayoutId
                pure Complete
              DSP.CREDITED -> do
                logInfo $ "Payout already processed: " <> show scheduledPayoutId
                pure Complete
              DSP.PROCESSING -> do
                logInfo $ "Payout already being processed: " <> show scheduledPayoutId
                pure Complete
              DSP.AUTO_PAY_FAILED -> do
                logInfo $ "Payout auto-pay failed, needs admin retry: " <> show scheduledPayoutId
                pure Complete
              DSP.RETRYING -> do
                logInfo $ "Payout is being retried: " <> show scheduledPayoutId
                pure Complete
              DSP.INITIATED -> do
                -- 4. Execute payout logic
                executeSpecialZonePayout scheduledPayout

executeSpecialZonePayout ::
  ( EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r
  ) =>
  DSP.ScheduledPayout ->
  m ExecutionResult
executeSpecialZonePayout scheduledPayout = do
  let driverId = Id scheduledPayout.driverId
  let merchantId = scheduledPayout.merchantId
  let merchantOpCityId = scheduledPayout.merchantOperatingCityId

  -- 1. Mark as PROCESSING
  QSPE.updateStatusWithHistoryById DSP.PROCESSING (Just "Payment in progress") scheduledPayout

  -- 2. Fetch driver info
  driverInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  -- 3. Check if driver has VPA for payout
  case driverInfo.payoutVpa of
    Nothing -> do
      -- No VPA configured, mark as failed
      logWarning $ "Driver " <> driverId.getId <> " has no payout VPA configured"
      QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just "Driver has no payout VPA configured") scheduledPayout
      pure Complete
    Just vpa -> do
      -- 4. Get payout config
      mbVehicle <- QV.findById driverId
      let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)

      case merchantOpCityId of
        Nothing -> do
          logWarning $ "No merchant operating city for payout: " <> show scheduledPayout.id
          QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just "Missing merchant operating city") scheduledPayout
          pure Complete
        Just opCityId -> do
          mbPayoutConfig <- CPC.findByPrimaryKey opCityId vehicleCategory Nothing
          case mbPayoutConfig of
            Nothing -> do
              logWarning $ "No payout config found for category: " <> show vehicleCategory
              QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just "Payout config not found") scheduledPayout
              pure Complete
            Just payoutConfig -> do
              -- 5. Validate payout is enabled
              unless payoutConfig.isPayoutEnabled $ do
                logInfo $ "Payout is disabled for this merchant"
                QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just "Payout disabled for merchant") scheduledPayout

              if not payoutConfig.isPayoutEnabled
                then pure Complete
                else do
                  -- 6. Get amount and create payout order
                  let amount = fromMaybe 0 scheduledPayout.amount
                  if amount <= 0
                    then do
                      logWarning $ "Invalid payout amount: " <> show amount
                      QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just "Invalid payout amount") scheduledPayout
                      pure Complete
                    else do
                      -- 7. Create payout order
                      uid <- generateGUID
                      phoneNo <- mapM decrypt person.mobileNumber
                      merchantOperatingCity <- CQMOC.findById opCityId >>= fromMaybeM (MerchantOperatingCityNotFound opCityId.getId)

                      let entityName = DLP.MANUAL -- can get later for now manual
                          createPayoutOrderReq = Payout.mkCreatePayoutOrderReq uid amount phoneNo person.email driverId.getId payoutConfig.remark (Just person.firstName) vpa payoutConfig.orderType False

                      case merchantId of
                        Nothing -> do
                          logWarning $ "No merchant ID for payout: " <> show scheduledPayout.id
                          QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just "Missing merchant ID") scheduledPayout
                          pure Complete
                        Just mId -> do
                          -- 8. Call payout service
                          logInfo $ "Calling payout service for driver: " <> driverId.getId <> " | amount: " <> show amount <> " | orderId: " <> uid
                          payoutServiceName <- TP.decidePayoutService (DEMSC.PayoutService PT.Juspay) person.clientSdkVersion person.merchantOperatingCityId
                          let createPayoutOrderCall = TP.createPayoutOrder mId opCityId payoutServiceName (Just person.id.getId)

                          result <- try $ Payout.createPayoutService (cast mId) (Just $ cast opCityId) (cast driverId) (Just [scheduledPayout.id.getId]) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall

                          case result of
                            Left (err :: SomeException) -> do
                              -- 9. Handle failure
                              logError $ "Payout service call failed: " <> show err
                              QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just $ "Payout service error: " <> show err) scheduledPayout
                              pure Complete
                            Right (mbPayoutResp, mbPayoutOrder) -> do
                              -- ye wala flow thoda different hai idk kya bheju to ek baar isko dekh lena.
                              let payoutOrderId = maybe "unknown" (\po -> po.id.getId) mbPayoutOrder
                                  -- Extract transactionRef from response: fulfillments[].transactions[].transactionRef
                                  mbTransactionRef = do
                                    resp <- mbPayoutResp
                                    fulfillment <- listToMaybe =<< resp.fulfillments
                                    txn <- listToMaybe =<< fulfillment.transactions
                                    pure txn.transactionRef
                                  transactionRef = fromMaybe payoutOrderId mbTransactionRef

                              -- Update scheduled payout with actual transaction reference
                              QSPE.updatePayoutTransactionId (Just transactionRef) scheduledPayout.id

                              QSPE.updateStatusWithHistoryById DSP.CREDITED (Just $ "Payment credited to bank. TxnRef: " <> transactionRef) scheduledPayout
                              logInfo $ "Special Zone Payout processed successfully for id: " <> show scheduledPayout.id <> " | transactionRef: " <> transactionRef
                              pure Complete
