{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The driver app's side of the bulk payout lifecycle in "Lib.Payment.Payout.Bulk": running a
--   city's cycle, instant payouts on the bulk rail, and settling finished items.
module SharedLogic.Payout.Bulk.Driver
  ( runBulkPayoutCycle,
    driverBulkHandle,
    runInstantPayout,
    settleBulkItemWith,
    rerunWalletLedgerStep,
  )
where

import Control.Applicative ((<|>))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Action.UI.Payout as UIPayout
import Domain.Types.Extra.Plan (ServiceNames (PREPAID_SUBSCRIPTION))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPaymentCommon
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Payout.Bulk.Cycle as Bulk
import Lib.Payment.Payout.Bulk.Types (BulkFinalOutcome (..), finalStatusOf)
import qualified Lib.Payment.Payout.Bulk.Types as Bulk
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobInWithCheck)
import SharedLogic.Allocator
import SharedLogic.Finance.Wallet (makeWalletRunningBalanceLockKey)
import SharedLogic.Finance.WalletPayout (PayoutContext (..), PayoutPrefetch (..), WalletPayoutFlow, WalletPayoutParams (..), initiateWalletPayoutWith, runWalletPayout, runWalletPayoutWith)
import SharedLogic.Payout.Bulk.Claim (beneficiaryBankOf, claimBeneficiary)
import SharedLogic.Payout.Bulk.Eligibility (BulkCandidate, usableBankDetails)
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.ScheduledPayoutConfig (ScheduledPayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as TPayout

-- | One bulk cycle over already-eligible beneficiaries. The batch is created before its rows, so every
--   order and excluded request carries its batchId from birth; HDFC is called only after that.
runBulkPayoutCycle ::
  ( UIPayout.PayoutSettlementFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r,
    PaymentBeamFlow.BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    Redis.HedisLTSFlowEnv r,
    JobCreator r m
  ) =>
  DSPC.ScheduledPayoutConfig ->
  DEMSC.ServiceName ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DPayoutBatch.PayoutBatchOrigin ->
  PR.PayoutType ->
  DTConf.TransporterConfig ->
  [BulkCandidate] ->
  m [(BulkCandidate, Bulk.BulkClaimOutcome)]
runBulkPayoutCycle config payoutServiceName merchantId merchantOpCityId origin payoutType transporterConfig candidates = do
  (partner, cycleConfig) <-
    bulkCycleSetup payoutServiceName merchantId merchantOpCityId origin config.defaultPayoutRail config.timeDiffFromUtc config.itemsPerBatchLimit
  Bulk.runBulkCycle
    driverBulkHandle
    partner
    cycleConfig
    (claimBeneficiary config payoutType transporterConfig payoutServiceName merchantId merchantOpCityId)
    candidates

-- | A driver's instant payout: a batch of one in a bulk (HDFC CBX) city, the usual wallet payout elsewhere.
runInstantPayout ::
  ( WalletPayoutFlow m r,
    UIPayout.PayoutSettlementFlow m r,
    JobCreator r m
  ) =>
  PayoutContext ->
  WalletPayoutParams ->
  m ()
runInstantPayout ctx params = do
  route <- try (TPayout.getPayoutServiceFlowForMerchant (.createPayoutOrder) (TPayout.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService ctx.person.merchantOperatingCityId)
  case route of
    Right (Payout.BulkFlow, payoutServiceName) -> runInstantBulkPayout ctx params payoutServiceName
    Right _ -> runWalletPayout ctx params
    Left (e :: SomeException) -> do
      -- The wallet payout reads the route again and raises its own error if it must.
      logWarning $ "Instant payout for " <> ctx.driverId.getId <> ": could not read the payout route (" <> show e <> "); running the usual wallet payout"
      runWalletPayout ctx params

-- | Instant payout on the bulk rail: a batch of one, opened under the usual wallet lock and checks.
--   The file is sent only after the lock is released.
runInstantBulkPayout ::
  ( WalletPayoutFlow m r,
    UIPayout.PayoutSettlementFlow m r,
    JobCreator r m
  ) =>
  PayoutContext ->
  WalletPayoutParams ->
  DEMSC.ServiceName ->
  m ()
runInstantBulkPayout ctx params payoutServiceName = do
  let mocId = ctx.person.merchantOperatingCityId
  bankAccount <- QDBA.findByPrimaryKey ctx.driverId >>= fromMaybeM (InvalidRequest "Bank account not added")
  whenJust (usableBankDetails bankAccount) $ throwError . InvalidRequest
  bank <- either (throwError . InvalidRequest) pure (beneficiaryBankOf bankAccount)
  -- Rail and day offset as the sweep takes them; the transporter config's offset if the city has no config.
  mbScheduledConfig <-
    getOneConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = mocId.getId, isEnabled = Nothing, payoutCategory = Just DPaymentCommon.DRIVER_WALLET_TRANSACTION}) Nothing
  (partner, cycleConfig) <-
    bulkCycleSetup
      payoutServiceName
      ctx.merchantId
      mocId
      DPayoutBatch.INSTANT
      (mbScheduledConfig >>= (.defaultPayoutRail))
      (maybe ctx.transporterConfig.timeDiffFromUtc (.timeDiffFromUtc) mbScheduledConfig)
      (Just 1)
  customerPhone <- mapM decrypt ctx.person.mobileNumber
  openedRef <- liftIO $ newIORef Nothing
  runWalletPayoutWith ctx params $ \plan -> do
    mbOpened <- Bulk.openSingleBulkPayout driverBulkHandle partner cycleConfig plan.payoutableBalance $ \batch -> do
      mbOrder <-
        initiateWalletPayoutWith
          ctx
          plan.payoutableBalance
          params.payoutType
          Nothing
          plan.cutoff
          plan.redeemableEntryIds
          plan.merchantTransferAmount
          (Just batch.id)
          (Just PayoutPrefetch {route = (Payout.BulkFlow, payoutServiceName, Just bankAccount), customerPhone})
          -- No per-order status check on the bulk rail: the batch is checked as a whole.
          (\_ -> pure ())
      pure ((,bank) <$> mbOrder)
    liftIO $ writeIORef openedRef mbOpened
  -- The money is held now. A failed send is left to the status check, which asks HDFC whether the
  -- file arrived, so the driver is not shown an error.
  liftIO (readIORef openedRef) >>= mapM_ \opened -> do
    sent <- try (Bulk.sendSingleBulkPayout driverBulkHandle opened)
    case sent of
      Right () -> pure ()
      Left (e :: SomeException) ->
        logError $ "Instant bulk payout batch " <> opened.singleBatch.id.getId <> " for " <> ctx.driverId.getId <> ": send failed (" <> show e <> "); the status check will chase it"

-- | The partner and batch settings for a city: rail, execution date and items per batch.
--   A payout service with no bulk API is refused here, before anything is opened.
bulkCycleSetup ::
  (ServiceFlow m r) =>
  DEMSC.ServiceName ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DPayoutBatch.PayoutBatchOrigin ->
  Maybe DPayoutBatch.PayoutBatchRail -> -- the city's rail, if it sets one
  Seconds -> -- the city's offset from UTC
  Maybe Int -> -- the city's items-per-batch limit, if it sets one
  m (Payout.PayoutServiceConfig, Bulk.BulkCycleConfig)
bulkCycleSetup payoutServiceName merchantId merchantOpCityId origin mbRail timeDiffFromUtc mbItemsPerBatchLimit = do
  partner <- TPayout.getPayoutServiceConfig payoutServiceName merchantOpCityId
  caps <-
    Payout.bulkPartnerCapsOf partner
      & fromMaybeM (InternalError $ "Payout service " <> show payoutServiceName <> " is not a bulk payout partner")
  now <- getCurrentTime
  pure
    ( partner,
      Bulk.BulkCycleConfig
        { payoutServiceName = show payoutServiceName,
          merchantId = merchantId.getId,
          merchantOperatingCityId = merchantOpCityId.getId,
          origin = origin,
          -- HDFC may still pay intra-bank when the beneficiary banks with them; that is read per item.
          rail = fromMaybe DPayoutBatch.NEFT mbRail,
          -- The city's own day: the UTC day would send yesterday's date before 05:30 IST.
          executionDate = Time.utctDay (addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now),
          -- The city's limit can only shrink the partner's cap.
          chunkSize = max 1 (maybe caps.maxItemsPerBatch (min caps.maxItemsPerBatch) mbItemsPerBatchLimit)
        }
    )

driverBulkHandle ::
  ( UIPayout.PayoutSettlementFlow m r,
    JobCreator r m
  ) =>
  Bulk.Handle m
driverBulkHandle =
  Bulk.Handle
    { -- Empty: the lib adds no prefix of its own; the app's Redis key prefix still applies.
      keyPrefix = "",
      partnerFor = \batch -> case readMaybe (T.unpack batch.payoutServiceName) of
        Nothing -> pure $ Left ("Unknown payout service: " <> batch.payoutServiceName)
        Just payoutServiceName -> Right <$> TPayout.getPayoutServiceConfig payoutServiceName (Id batch.merchantOperatingCityId),
      -- One job per city. It keeps rescheduling itself near now, so a day either side always finds it.
      createCityStatusCheckJob = \merchantId cityId -> do
        now <- getCurrentTime
        createJobInWithCheck @_ @'BulkPayoutStatusCheck
          (Just (Id merchantId))
          (Just (Id cityId))
          5
          (addUTCTime (-86400) now)
          (addUTCTime 86400 now)
          "BulkPayoutStatusCheck"
          (Just 1)
          BulkPayoutStatusCheckJobData {merchantId = Id merchantId, merchantOperatingCityId = Id cityId},
      settleItem = settleBulkItem
    }

-- | Settle one item through the shared payout settlement; raises if it could not finish, so it is retried.
--   The request is written last, as the "fully settled" marker. If the order is already final, only
--   the ledger step, push and request write run again.
settleBulkItem ::
  (UIPayout.PayoutSettlementFlow m r) =>
  Text -> -- the batch's merchantOperatingCityId
  DPayoutOrder.PayoutOrder ->
  Maybe PR.PayoutRequest ->
  BulkFinalOutcome ->
  m ()
settleBulkItem = settleBulkItemWith True

-- | 'settleBulkItem', choosing whether the driver gets the outcome push.
settleBulkItemWith ::
  (UIPayout.PayoutSettlementFlow m r) =>
  Bool -> -- send the outcome push
  Text -> -- the batch's merchantOperatingCityId
  DPayoutOrder.PayoutOrder ->
  Maybe PR.PayoutRequest ->
  BulkFinalOutcome ->
  m ()
settleBulkItemWith sendPush merchantOpCityId order mbRequest outcome
  -- Without a request the shared settle would mark the order final but skip the ledger step, leaving
  -- the money held behind a settled-looking order. Refuse, so the item ends in manual review.
  | isNothing mbRequest = throwError (InternalError ("Payout request not found for order " <> order.orderId <> "; not settling it"))
  | order.status `elem` [Payout.SUCCESS, Payout.FAILURE] = rerun
  | otherwise = do
    -- Set inside the wallet lock. Still False means the settle did not run, so the request stays open
    -- for the next status check.
    settleRan <- liftIO (newIORef False)
    UIPayout.payoutSettlementActionWith (bulkHooks settleRan) (Id order.merchantId) mocId status amount order.orderId callHdfcPayoutServiceAction
    didRun <- liftIO (readIORef settleRan)
    unless didRun $
      throwError (InternalError ("Settle did not run (wallet lock busy, or order already settled); will try again: " <> order.orderId))
    writeRequest status
  where
    mocId = Id merchantOpCityId
    amount = order.amount.amount
    status = finalStatusOf outcome
    failureDetail = case outcome of
      BulkRejected _ detail _ _ -> detail
      BulkPaid {} -> Nothing

    pushKindFor updStatus
      | updStatus == Payout.SUCCESS = BulkPushPaid
      | otherwise = BulkPushFailed

    -- HDFC's answer is already in hand, so this only writes it onto the order. Either way, keep the
    -- reference we already have if this answer carries none.
    callHdfcPayoutServiceAction _orderId _driverId _payoutConfig = do
      case outcome of
        BulkPaid mbSettlementRef refType mbSettleStatus mbCode mbNote ->
          QPayoutOrder.updateBulkSettled Payout.SUCCESS mbSettleStatus (mbSettlementRef <|> order.settlementRef) (Just refType) mbCode mbNote order.id
        BulkRejected mbCode detail mbSettleStatus mbRef -> do
          let (newRef, newRefType) = case mbRef of
                Just (ref, refType) -> (Just ref, Just refType)
                Nothing -> (order.settlementRef, order.settlementRefType)
          QPayoutOrder.updateBulkSettled Payout.FAILURE mbSettleStatus newRef newRefType mbCode detail order.id
      pure (status, order.orderId)

    -- Called inside the wallet lock right after the ledger step, so the flag proves the settle ran.
    bulkHooks settleRan =
      UIPayout.SettlementHooks
        { notifyWalletPayout = \person _ updStatus _ -> do
            liftIO (writeIORef settleRan True)
            pushOutcome person updStatus
        }

    pushOutcome person updStatus =
      when sendPush $
        fork ("BulkPayoutNotify:" <> order.orderId) $ notifyBulkPayoutOutcome person order (pushKindFor updStatus) failureDetail

    rerun = whenJust mbRequest $ \request -> do
      rerunWalletLedgerStep merchantOpCityId order.merchantId order.customerId request amount order.status (`pushOutcome` order.status)
      writeRequest order.status

    -- On a failure the partner's reason also goes to failure_reason, which the API and recon read.
    writeRequest finalStatus = whenJust mbRequest $ \request -> do
      DPayment.updatePayoutRequestStatusFromOrder order finalStatus
      when (finalStatus == Payout.FAILURE) $
        QPR.updateStatusWithReasonById PR.AUTO_PAY_FAILED (maybe order.responseMessage Just failureDetail) request.id

-- | Rerun the ledger step for a known outcome: pay out the hold on SUCCESS, else return it to the wallet.
--   Safe to repeat, since no leg is posted twice. Raises when the wallet lock stays busy.
rerunWalletLedgerStep ::
  (UIPayout.PayoutSettlementFlow m r) =>
  Text -> -- merchantOperatingCityId
  Text -> -- merchantId
  Text -> -- the payee's person id
  PR.PayoutRequest ->
  HighPrecMoney ->
  Payout.PayoutOrderStatus ->
  (DP.Person -> m ()) -> -- run inside the wallet lock, right after the ledger step
  m ()
rerunWalletLedgerStep merchantOpCityId merchantId personId request amount status afterLedger = do
  settleRan <- liftIO (newIORef False)
  Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey personId) 10 10 $ do
    person <- QPerson.findById (Id personId) >>= fromMaybeM (PersonNotFound personId)
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId)
    UIPayout.settleDriverWalletPayoutLedgerFor (Id merchantOpCityId) merchantId person transporterConfig request amount status
    afterLedger person
    liftIO (writeIORef settleRan True)
  didRun <- liftIO (readIORef settleRan)
  unless didRun $
    throwError (InternalError ("Wallet lock busy; will try again: payout request " <> request.id.getId))

-- | Which bulk push to send.
data BulkPushKind
  = BulkPushPaid
  | BulkPushFailed

-- | The bulk payout push, with HDFC's reason on a failure. Fleet owners get none (no channel yet).
notifyBulkPayoutOutcome ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisLTSFlowEnv r) =>
  DP.Person ->
  DPayoutOrder.PayoutOrder ->
  BulkPushKind ->
  Maybe Text -> -- the partner's reason on a failure, if they sent one
  m ()
notifyBulkPayoutOutcome person order kind mbFailureDetail =
  when (person.role `notElem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS]) $ do
    let amount = order.amount.amount
        (notificationTitle, notificationMessage, notificationType) = case kind of
          BulkPushPaid ->
            ("Payout Complete", "Your payout of Rs." <> show amount <> " has been successfully settled to your bank account.", FCM.PAYOUT_COMPLETED)
          BulkPushFailed ->
            ( "Payout Failed",
              "Your payout of Rs." <> show amount <> " has failed" <> maybe "" (": " <>) mbFailureDetail <> ". Please retry or contact support.",
              FCM.PAYOUT_FAILED
            )
    Notify.sendNotificationToDriver person.merchantOperatingCityId FCM.SHOW Nothing notificationType notificationTitle notificationMessage person person.deviceToken
