{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- NOTE (reviewer, remove before merge): NEW FILE (main has no bulk payout code). This is the driver app's glue for the HDFC
--   bulk payout lifecycle, which runs in the payment lib ("Lib.Payment.Payout.Bulk.*"):
--   open batch -> claim (Claim.hs) -> submit -> status check -> settle (settleBulkItem, below).
--   The money steps (hold, settle, give back) use the same shared code as Juspay/Stripe; only the partner-specific steps
--   (HDFC calls, status mapping, batching) are bulk-only.
--   Juspay/Stripe impact: bulk-only. The only callers are
--     * ScheduledBatchPayout.processWalletPayouts, inside `when (payoutServiceFlow == Payout.BulkFlow)`;
--     * AdhocPayout, which refuses every city whose payout flow is not BulkFlow (resolveUrlCity);
--     * the BulkPayoutStatusCheck job, which only exists for a city that has opened a payout_batch;
--     * DriverWallet.postWalletPayout (the instant payout of EVERY city) -> runInstantPayout. That
--       is the one entry a Juspay/Stripe city passes through: it reads the city's route and, for anything that is not
--       a bulk partner, calls main's WalletPayout.runWalletPayout unchanged.
--   Apart from that router, no Juspay/Stripe code path runs code from this module.

-- | The driver app's side of the bulk payout lifecycle, which runs in "Lib.Payment.Payout.Bulk":
--
--   * 'runBulkPayoutCycle' -- one cycle for a city: the partner and the city's limits are resolved
--     here, and the lib runs the batch with the driver's claim ("SharedLogic.Payout.Bulk.Claim").
--     Shared by the scheduled sweep and the ad-hoc admin flow.
--   * 'runInstantPayout' -- a driver's instant payout: the usual wallet payout in a Juspay/Stripe
--     city, a batch of one in a bulk city.
--   * 'driverBulkHandle' -- what the lib asks the driver app for: the partner config of a batch,
--     the status-check job, and the settle of a finished item.
--   * the settle itself, through main's Juspay/Stripe settlement ('UIPayout.payoutSettlementActionWith').
module SharedLogic.Payout.Bulk.Driver
  ( runBulkPayoutCycle,
    driverBulkHandle,
    runInstantPayout,
  )
where

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

-- NOTE (reviewer, remove before merge): One bulk cycle for one city. The partner's caps come from the partner config (a payout
--   service with no bulk API is refused here); the rail and the batch size come from the city's ScheduledPayoutConfig
--   (itemsPerBatchLimit can only shrink the partner's cap; no defaultPayoutRail means NEFT); the HDFC execution date is the
--   city's local day. The lib's runBulkCycle then opens the batch (openBulkBatch, which also makes sure the city's status-check
--   job exists), claims each person with claimBeneficiary, and submits the file.
--   The adhoc caller (AdhocPayout.resolvePersons) makes sure every person passed in is in this city.
--   Juspay/Stripe impact: bulk-only (see the NOTE at the top of this file).

-- | Run one bulk payout cycle over beneficiaries that have already passed the eligibility pass, in
--   this order: all checks first, then the batch, then the rows
--   underneath it -- so an order and an excluded request both carry their batchId from birth --
--   and only then the submission to HDFC.
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

-- NOTE (reviewer, remove before merge): DriverWallet.postWalletPayout calls this for every city. The city's route is
--   read first; anything that is not a bulk partner -- or a failed read -- runs main's WalletPayout.runWalletPayout,
--   untouched. A bulk-rail city runs the SAME payout lock (key, TTL) and the same amount, minimum
--   (minimumWalletPayoutAmount) and daily-limit checks through runWalletPayoutWith; the only difference is the batch of
--   one (origin INSTANT). The HDFC file is sent after the lock is released, as the sweep does: the lock lasts 10 s and
--   the HDFC call up to 60 s. The bulk claim is not reused: it takes the same payout lock itself, and it records
--   exclusions instead of returning the instant payout's errors. In a bulk city this is on for every role by default;
--   instantPayoutExcludedRoles turns it off for the roles it lists.

-- | A driver's instant payout. A city that pays through a bulk partner (HDFC CBX) sends it in a
--   batch of its own; every other city runs the usual wallet payout.
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
      -- The usual payout reads the route again itself, and fails with its own error if it must.
      logWarning $ "Instant payout for " <> ctx.driverId.getId <> ": could not read the payout route (" <> show e <> "); running the usual wallet payout"
      runWalletPayout ctx params

-- | Instant payout on the bulk rail. The bank details and the batch settings are read first;
--   then, under the same wallet lock and amount checks as every instant payout, the batch of one
--   is opened with its request, hold and order. The file is sent once the lock is released.
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
  -- Rail and the city's day as the sweep takes them, from the city's wallet payout config; the
  -- transporter config's offset when the city has none. One item, so a batch of one.
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
          (Just plan.cutoff)
          plan.redeemableEntryIds
          plan.merchantTransferAmount
          (Just batch.id)
          (Just PayoutPrefetch {route = (Payout.BulkFlow, payoutServiceName, Just bankAccount), customerPhone})
          -- No per-order status check on the bulk rail: the batch is checked as a whole.
          (\_ -> pure ())
      pure ((\order -> (order, beneficiaryBankOf bankAccount)) <$> mbOrder)
    liftIO $ writeIORef openedRef mbOpened
  -- The lock is released; the money is held. A send that fails here leaves the batch for the
  -- status check, which asks the bank whether the file arrived. As with an unclear partner answer
  -- on any instant payout, the driver is not shown an error for it.
  liftIO (readIORef openedRef) >>= mapM_ \opened -> do
    sent <- try (Bulk.sendSingleBulkPayout driverBulkHandle opened)
    case sent of
      Right () -> pure ()
      Left (e :: SomeException) ->
        logError $ "Instant bulk payout batch " <> opened.singleBatch.id.getId <> " for " <> ctx.driverId.getId <> ": send failed (" <> show e <> "); the status check will chase it"

-- NOTE (reviewer, remove before merge): the partner lookup and cycle settings, shared by the sweep/adhoc cycle and
--   instant payout's batch of one so both use the same rules (one place for the rail default, the city's execution date
--   and the bulk-partner check). Bulk-only.

-- | The partner and the settings for a city's bulk batches: the rail (NEFT unless the city says
--   otherwise), the date the partner executes on in the city's own day, and the items per batch.
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
  -- Read the partner's limits without naming the partner. A payout service with no bulk API is
  -- refused here, once, instead of silently inheriting someone else's ceiling.
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
          -- NEFT unless the city says otherwise. HDFC may still execute an item intra-bank when the
          -- beneficiary banks with them; that is read per item from the response.
          rail = fromMaybe DPayoutBatch.NEFT mbRail,
          -- The date HDFC executes on, in the city's own day: taking the UTC day would send
          -- yesterday's date for anything submitted before 05:30 IST.
          executionDate = Time.utctDay (addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now),
          -- The partner's cap is authoritative; the city's limit can only shrink it, never exceed it.
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
    { -- Empty: the lib adds nothing to the driver's Redis keys (batch locks, file-number counters); the app's own
      -- Redis key prefix still applies.
      keyPrefix = "",
      partnerFor = \batch -> case readMaybe (T.unpack batch.payoutServiceName) of
        Nothing -> pure $ Left ("Unknown payout service: " <> batch.payoutServiceName)
        Just payoutServiceName -> Right <$> TPayout.getPayoutServiceConfig payoutServiceName (Id batch.merchantOperatingCityId),
      -- NOTE (reviewer, remove before merge): ONE status-check job PER CITY, created only when a batch is opened (lib
      --   openBulkBatch -> ensureCityJob, under a 60 s per-city lock -> this function). createJobInWithCheck only creates
      --   a job when no Pending "BulkPayoutStatusCheck" job with exactly the same job data is scheduled between now - 1 day
      --   and now + 1 day; `Just 1` = at most one; 5 = first run in 5 seconds. The lookup compares the job data JSON, which
      --   is why the city is in it ({merchantId, merchantOperatingCityId}, BulkPayoutStatusCheckJobData).
      --   A second copy of the job (e.g. from the scheduler's own recovery) is harmless: each batch has its own lock and
      --   is re-read, so a copy makes no extra HDFC call.
      --   Juspay/Stripe impact: bulk-only. Juspay/Stripe keep main's per-order PayoutStatusCheck job, untouched.
      -- One city's job, created only when no pending job for that city exists. The city's job keeps
      -- rescheduling itself, so its scheduled time is always within minutes of now, or a little in
      -- the past when a run is late; a day either side always finds it.
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

-- NOTE (reviewer, remove before merge): The "settle" step of the bulk lifecycle, for one item HDFC has finished with.
--   It goes through the shared settle, UIPayout.payoutSettlementActionWith (main's settle code, also used by the Juspay and
--   Stripe webhooks), with two bulk parts plugged in: callHdfcPayoutServiceAction (writes the answer HDFC already gave onto
--   the order, like Stripe's webhook body does) and bulkHooks (bulk's own push).
--   The shared settle waits up to 10 s for the wallet lock and then skips, so the settleRan flag is set by the push hook,
--   which runs inside the wallet lock right after the ledger step. If it is still False, we raise; the lib then keeps the
--   item open and the next planned status check settles it again (nothing is re-sent to HDFC).
--   The `rerun` branch (order already final, request not) re-runs only the ledger step, the push and the request write.
--   Juspay/Stripe impact: bulk-only (reached only through driverBulkHandle.settleItem, from the bulk status check or a
--   gateway-refused submit). The hook parameter of the shared settle (Domain/Action/UI/Payout.hs) is new; Juspay/Stripe
--   call it through payoutSettlementAction = payoutSettlementActionWith defaultSettlementHooks, i.e. main's push.

-- | Settle one item through main's shared settlement. Raises when it could not complete; the
--   caller keeps the item pending so the next inquiry of a still-open batch runs it again.
--
--   First pass (order not yet final): the shared settlement takes the wallet lock, writes HDFC's
--   answer onto the order through 'callHdfcPayoutServiceAction', runs the ledger step, updates the
--   driver stats and sends the push. Then the request is written -- last, as the "fully settled"
--   marker.
--
--   Re-run (order already final, request not): an earlier pass wrote the order but its ledger step
--   failed. Only the ledger step, push and request write are run again; the ledger step looks every
--   leg up before posting it, so running it twice posts nothing twice.
settleBulkItem ::
  (UIPayout.PayoutSettlementFlow m r) =>
  Text -> -- the batch's merchantOperatingCityId
  DPayoutOrder.PayoutOrder ->
  Maybe PR.PayoutRequest ->
  BulkFinalOutcome ->
  m ()
settleBulkItem merchantOpCityId order mbRequest outcome
  | order.status `elem` [Payout.SUCCESS, Payout.FAILURE] = rerun
  | otherwise = do
    -- Set to True inside the wallet lock. If it is still False afterwards, the settle did not run:
    -- the lock was busy, or the order was already settled. The request is then left open, so the next
    -- planned status check runs the settle again.
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

    -- Which push to send.
    pushKindFor updStatus
      | updStatus == Payout.SUCCESS = BulkPushPaid
      | otherwise = BulkPushFailed

    -- The HDFC counterpart of Juspay's status call and Stripe's webhook body: the answer is already
    -- in hand, so this only writes it onto the order.
    callHdfcPayoutServiceAction _orderId _driverId _payoutConfig = do
      case outcome of
        BulkPaid settlementRef refType mbSettleStatus mbCode mbNote ->
          QPayoutOrder.updateBulkSettled Payout.SUCCESS mbSettleStatus (Just settlementRef) (Just refType) mbCode mbNote order.id order.orderId
        BulkRejected mbCode detail mbSettleStatus mbRef -> do
          -- Keep the reference we already have if HDFC sent none this time.
          let (newRef, newRefType) = case mbRef of
                Just (ref, refType) -> (Just ref, Just refType)
                Nothing -> (order.settlementRef, order.settlementRefType)
          QPayoutOrder.updateBulkSettled Payout.FAILURE mbSettleStatus newRef newRefType mbCode detail order.id order.orderId
      pure (status, order.orderId)

    -- The shared settle calls this inside the wallet lock, right after the ledger step, so setting
    -- the flag here proves the settle really ran.
    bulkHooks settleRan =
      UIPayout.SettlementHooks
        { notifyWalletPayout = \person _ updStatus _ -> do
            liftIO (writeIORef settleRan True)
            fork ("BulkPayoutNotify:" <> order.orderId) $ notifyBulkPayoutOutcome person order (pushKindFor updStatus) failureDetail
        }

    rerun = whenJust mbRequest $ \request -> do
      settleRan <- liftIO (newIORef False)
      Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey order.customerId) 10 10 $ do
        person <- QPerson.findById (Id order.customerId) >>= fromMaybeM (PersonNotFound order.customerId)
        transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId)
        UIPayout.settleDriverWalletPayoutLedger mocId order person transporterConfig request amount order.status
        (bulkHooks settleRan).notifyWalletPayout person order order.status amount
      didRun <- liftIO (readIORef settleRan)
      unless didRun $
        throwError (InternalError ("Wallet lock busy; will try again: " <> order.orderId))
      writeRequest order.status

    -- Main's request update ('CREDITED' / 'AUTO_PAY_FAILED', with history). On a failure the
    -- partner's reason also goes to failure_reason, which the API and recon read.
    writeRequest finalStatus = whenJust mbRequest $ \request -> do
      DPayment.updatePayoutRequestStatusFromOrder order finalStatus
      when (finalStatus == Payout.FAILURE) $
        QPR.updateStatusWithReasonById PR.AUTO_PAY_FAILED (maybe order.responseMessage Just failureDetail) request.id

-- NOTE (reviewer, remove before merge): Bulk's own push: HDFC's reason in the text; fleet owners skipped, as no channel is
--   decided for them yet. Juspay/Stripe impact: bulk-only. Juspay/Stripe keep main's push through defaultSettlementHooks;
--   they never reach this code.

-- | Which bulk push to send.
data BulkPushKind
  = BulkPushPaid
  | BulkPushFailed

-- | The bulk push. Names HDFC's own reason on a failure; fleet owners are skipped (no channel
--   decided for them yet).
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
