module SharedLogic.Finance.WalletPayout
  ( PayoutContext (..),
    WalletPayoutParams (..),
    WalletPayoutFlow,
    loadPayoutContext,
    ensurePayoutsEnabled,
    instantPayoutAllowedFor,
    ensureInstantPayoutAllowed,
    computePayoutFee,
    runWalletPayout,
    runWalletPayoutWith,
    PayoutPrefetch (..),
    initiateWalletPayoutWith,
    reversalPendingPrefix,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.DriverBankAccount as DDBA
import Domain.Types.Extra.Plan
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payout.Interface as IPayout
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance (LedgerEntryMetadata (..))
import Lib.Finance.Balance (TransferAmount (..))
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Transfer as DTransfer
import Lib.Finance.Error.Types (FinanceError (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Transfer.Outbound as Outbound
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Payout.Bulk.Types as Bulk
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import SharedLogic.Finance.PostActions (runFinance)
import qualified SharedLogic.Finance.Rails as Rails
import SharedLogic.Finance.Wallet
import SharedLogic.Finance.WalletAccount (makeWalletOfferHoldsKey)
import qualified SharedLogic.Payment as SPayment
import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOI
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as Payout

data PayoutContext = PayoutContext
  { driverId :: Id DP.Person,
    merchantId :: Id Domain.Types.Merchant.Merchant,
    mocId :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    person :: DP.Person,
    payoutVpa :: Maybe Text,
    transporterConfig :: DTConf.TransporterConfig
  }

-- | The per-day payout cap moved to the kernel (finance_account_config
--   maxTransferOutPerDay, enforced on instant transfers inside initiateTransferOut).
data WalletPayoutParams = WalletPayoutParams
  { payoutType :: PR.PayoutType,
    minimumPayoutAmount :: HighPrecMoney,
    throwOnBelowMinimum :: Bool
  }

data WalletPayoutPlan = WalletPayoutPlan
  { payoutableBalance :: HighPrecMoney,
    cutoff :: UTCTime,
    redeemableEntryIds :: [Text],
    merchantTransferAmount :: HighPrecMoney
  }

type WalletPayoutFlow m r =
  ( EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    BeamFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    Redis.HedisLTSFlowEnv r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  )

loadPayoutContext ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Maybe (Id DP.Person) ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m PayoutContext
loadPayoutContext mbPersonId merchantId mocId = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  payoutVpa <- case person.role of
    DP.FLEET_OWNER -> do
      mbFleetInfo <- QFOI.findByPersonIdAndEnabledAndVerified Nothing Nothing driverId
      pure (mbFleetInfo >>= (.payoutVpa))
    DP.FLEET_BUSINESS -> do
      mbFleetInfo <- QFOI.findByPersonIdAndEnabledAndVerified Nothing Nothing driverId
      pure (mbFleetInfo >>= (.payoutVpa))
    _ -> do
      mbDriverInfo <- QDI.findById driverId
      pure (mbDriverInfo >>= (.payoutVpa))
  pure PayoutContext {..}

ensurePayoutsEnabled :: (MonadFlow m) => PayoutContext -> m ()
ensurePayoutsEnabled ctx =
  unless ctx.transporterConfig.driverWalletConfig.enableWalletPayout $
    throwError $ InvalidRequest "Payouts are disabled"

-- | Whether this role may take an instant payout in the city.
--   FLEET_BUSINESS counts as FLEET_OWNER, as everywhere else in the wallet.
instantPayoutAllowedFor :: DTConf.DriverWalletConfig -> DP.Role -> Bool
instantPayoutAllowedFor walletConfig role = asFleetOwner role `notElem` map asFleetOwner (fromMaybe [] walletConfig.instantPayoutExcludedRoles)
  where
    asFleetOwner DP.FLEET_BUSINESS = DP.FLEET_OWNER
    asFleetOwner r = r

-- | Instant payouts only; scheduled and adhoc payouts skip it. Kept apart from 'enableWalletPayout'
--   so a city can leave payouts to admins and the sweep.
ensureInstantPayoutAllowed :: (MonadFlow m) => PayoutContext -> m ()
ensureInstantPayoutAllowed ctx =
  unless (instantPayoutAllowedFor ctx.transporterConfig.driverWalletConfig ctx.person.role) $
    throwError InstantPayoutNotAllowed

-- | Reason prefix for held money given back because the payout was never sent.
payoutNotSentPrefix :: Text
payoutNotSentPrefix = "Payout not sent: "

-- | Failure reason prefix when the payout was never sent and its hold could not be given back.
--   Ops find these by this prefix and reverse the hold.
reversalPendingPrefix :: Text
reversalPendingPrefix = "REVERSAL_PENDING: "

-- | Mark the request failed with a reason (one history row).
failRequest ::
  (BeamFlow m r, PaymentBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Id PR.PayoutRequest ->
  Text ->
  m ()
failRequest requestId reason = do
  mbRequest <- QPR.findById requestId
  whenJust mbRequest $ \request -> do
    unless (request.status == PR.AUTO_PAY_FAILED) $
      PayoutRequest.updateStatusWithHistoryById PR.AUTO_PAY_FAILED (Just reason) request
    QPR.updateStatusWithReasonById PR.AUTO_PAY_FAILED (Just reason) request.id

-- | Bulk only: fail this request's order, if one was made, so it never goes in a file.
failBulkOrderIfAny ::
  (PaymentBeamFlow.BeamFlow m r) =>
  Maybe (Id DPayoutBatch.PayoutBatch) ->
  Id PR.PayoutRequest ->
  Text ->
  m ()
failBulkOrderIfAny mbBatchId requestId reason =
  whenJust mbBatchId $ \batchId -> do
    orders <- QPayoutOrder.findAllByBatchId (Just batchId)
    let ordersOfRequest = [order | order <- orders, (listToMaybe =<< order.entityIds) == Just requestId.getId]
    forM_ ordersOfRequest $ \order ->
      QPayoutOrder.updateBulkSettled IPayout.FAILURE Nothing Nothing Nothing (Just "NOT_SENT") (Just reason) order.id

resolvePayoutVpa :: (MonadFlow m) => PayoutContext -> m Text
resolvePayoutVpa ctx =
  fromMaybeM (InternalError $ "Payout vpa not present for " <> ctx.driverId.getId) ctx.payoutVpa

computePayoutFee :: Maybe DTConf.PayoutFeeConfig -> HighPrecMoney -> HighPrecMoney
computePayoutFee Nothing _ = 0
computePayoutFee (Just feeConfig) amount =
  min amount . SPayment.roundToTwoDecimalPlaces $ computeStripePayoutFee feeConfig amount

-- | One wallet payout attempt for a payee under the wallet lock: the amount finder and the
--   initiate (payout request + OwnerPayoutLiability hold) both run inside it.
--   The kernel's initiateTransferOut additionally locks "Finance:BalanceLock:<accountId>"
--   — a different key from this driverId-keyed one, so nesting cannot deadlock; this
--   outer lock goes away once every wallet read/write is behind the kernel lock.
runWalletPayout :: (WalletPayoutFlow m r) => PayoutContext -> WalletPayoutParams -> m ()
runWalletPayout ctx params =
  PayoutRequest.runPayoutUnderLock (makeWalletRunningBalanceLockKey ctx.driverId.getId) 10 (findWalletPayoutAmount ctx params) (initiateWalletPayout ctx params.payoutType)

-- | 'runWalletPayout' with a caller-supplied initiate step, under the same wallet lock and amount checks.
runWalletPayoutWith :: (WalletPayoutFlow m r) => PayoutContext -> WalletPayoutParams -> (WalletPayoutPlan -> m ()) -> m ()
runWalletPayoutWith ctx params initiate =
  PayoutRequest.runPayoutUnderLock (makeWalletRunningBalanceLockKey ctx.driverId.getId) 10 (findWalletPayoutAmount ctx params) initiate

findWalletPayoutAmount :: (WalletPayoutFlow m r) => PayoutContext -> WalletPayoutParams -> m (Maybe WalletPayoutPlan)
findWalletPayoutAmount ctx params = do
  now <- getCurrentTime
  let counterparty = counterpartyFromRole ctx.person.role
      timeDiff = secondsToNominalDiffTime ctx.transporterConfig.timeDiffFromUtc
      cutOffDays = ctx.transporterConfig.driverWalletConfig.payoutCutOffDays
      cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
  -- The per-day payout cap is enforced by the kernel (finance_account_config
  -- maxTransferOutPerDay, instant transfers only) inside initiateTransferOut.
  pb <- computePayoutableBalance counterparty ctx.driverId.getId
  let eligibility = pb.eligibility
      payoutableBalance = pb.payoutableBalance
  logInfo $ "Payout eligibility for " <> ctx.driverId.getId <> ": walletBalance=" <> show pb.walletBalance <> ", nonRedeemable=" <> show eligibility.nonRedeemableBalance <> ", processingPayout=" <> show eligibility.processingPayoutBalance <> ", rideHold=" <> show pb.rideHoldBalance <> ", offerHold=" <> show pb.offerHoldBalance <> ", payoutableBalance=" <> show payoutableBalance <> ", minimum=" <> show params.minimumPayoutAmount <> ", redeemableEntryIds=" <> show eligibility.redeemableEntryIds
  if payoutableBalance < params.minimumPayoutAmount
    then do
      when params.throwOnBelowMinimum $ throwError $ InvalidRequest ("Minimum payout amount is " <> show params.minimumPayoutAmount)
      pure Nothing
    else
      pure $
        Just
          WalletPayoutPlan
            { payoutableBalance,
              cutoff,
              redeemableEntryIds = map (.getId) eligibility.redeemableEntryIds,
              merchantTransferAmount = eligibility.merchantTransferAmount
            }

-- | The initiate step of 'runWalletPayout' (instant payouts and the Juspay/Stripe sweep):
--   no batch, coverage up to the cutoff, and a per-order status check.
initiateWalletPayout :: (WalletPayoutFlow m r) => PayoutContext -> PR.PayoutType -> WalletPayoutPlan -> m ()
initiateWalletPayout ctx payoutType WalletPayoutPlan {..} =
  void $ initiateWalletPayoutWith ctx payoutableBalance payoutType Nothing (Just cutoff) redeemableEntryIds merchantTransferAmount Nothing Nothing afterPayoutOrderCreated

-- | Payout route and decrypted phone, resolved before the wallet lock so they are not fetched while holding it.
data PayoutPrefetch = PayoutPrefetch
  { route :: (IPayout.PayoutServiceFlow, DEMSC.ServiceName, Maybe DDBA.DriverBankAccount),
    customerPhone :: Maybe Text
  }

-- | Create the request and order for one beneficiary and hold the amount, so a second payout cannot
--   see the same money. Juspay/Stripe call the partner and then hold; the bulk rail holds first and
--   gives the money back if the payout does not go ahead.
initiateWalletPayoutWith ::
  ( EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    BeamFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    Redis.HedisLTSFlowEnv r
  ) =>
  PayoutContext ->
  HighPrecMoney -> -- payoutable balance
  PR.PayoutType -> -- INSTANT, SCHEDULED or ADHOC
  Maybe UTCTime -> -- coverageFrom
  Maybe UTCTime -> -- coverageTo
  [Text] -> -- redeemable entry ids, stashed for the settlement path
  HighPrecMoney -> -- merchant transfer amount (VAT input + discounts)

  -- | The batch this payout is claimed into on the bulk rail; Nothing elsewhere.
  Maybe (Id DPayoutBatch.PayoutBatch) ->
  -- | Resolved by the caller outside the lock (the bulk claim); Nothing resolves it here.
  Maybe PayoutPrefetch ->
  -- | Run once the order is saved, e.g. a per-order status check. Passed in so this function needs
  --   no job-creator constraint. Ignored on the bulk rail.
  (DPayoutOrder.PayoutOrder -> m ()) ->
  m (Maybe DPayoutOrder.PayoutOrder)
initiateWalletPayoutWith ctx payoutableBalance payoutType coverageFrom coverageTo redeemableEntryIds merchantTransferAmount mbBatchId mbPrefetch afterOrderCreated = do
  phoneNo <- maybe (mapM decrypt ctx.person.mobileNumber) (pure . (.customerPhone)) mbPrefetch
  merchantOperatingCity <- CQMOC.findById (cast ctx.person.merchantOperatingCityId) >>= fromMaybeM (MerchantOperatingCityNotFound ctx.person.merchantOperatingCityId.getId)
  (payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <-
    maybe
      (Payout.getCreatePayoutServiceFlow (Payout.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService ctx.person.clientSdkVersion ctx.person.merchantOperatingCityId ctx.person.id)
      (pure . (.route))
      mbPrefetch
  vpa <- case payoutServiceFlow of
    IPayout.JuspayFlow -> Just <$> resolvePayoutVpa ctx
    IPayout.StripeFlow -> pure Nothing
    -- No VPA on the bulk rail: HDFC pays to the account number and IFSC resolved above.
    IPayout.BulkFlow -> do
      when (isNothing mbBatchId) $
        throwError (InvalidRequest "A payout on the bulk rail must belong to a payout batch")
      pure Nothing
  let fee = computePayoutFee ctx.transporterConfig.driverWalletConfig.payoutFee payoutableBalance
      -- Floor (not round-half-up) to whole cents so the disbursed amount never exceeds the
      -- wallet balance; otherwise the hold can overdraw the wallet by a sub-cent rounding delta.
      netAmount = SPayment.floorToTwoDecimalPlaces (payoutableBalance - fee)
      -- HDFC CBX has no per-order status API, so a per-order status-check job could never be answered.
      onOrderCreated = case payoutServiceFlow of
        IPayout.BulkFlow -> \_ -> pure ()
        _ -> afterOrderCreated
      (payoutBankName, payoutBankAccountLast4) = bankLabel payoutServiceFlow mbPersonBankAccount
      submission =
        PayoutRequest.PayoutSubmission
          { batchId = mbBatchId,
            beneficiaryId = ctx.driverId.getId,
            entityName = DPayment.DRIVER_WALLET_TRANSACTION,
            entityId = ctx.driverId.getId,
            entityRefId = Nothing,
            amount = netAmount,
            currency = ctx.transporterConfig.currency,
            payoutFee = if fee > 0 then Just fee else Nothing,
            transferAmount = Just merchantTransferAmount,
            merchantId = ctx.merchantId.getId,
            merchantOpCityId = ctx.mocId.getId,
            city = show merchantOperatingCity.city,
            vpa = vpa,
            -- mbPersonBankAccount is the account the payout is actually sent to. For a fleet driver
            -- that is the fleet owner's on Stripe, and their own on the bulk rail.
            bankName = payoutBankName,
            bankAccountLast4 = payoutBankAccountLast4,
            customerName = Just ctx.person.firstName,
            customerPhone = phoneNo,
            customerEmail = ctx.person.email,
            remark = "Settlement for wallet",
            orderType = "FULFILL_ONLY",
            scheduledAt = Nothing,
            payoutType = Just payoutType,
            coverageFrom = coverageFrom,
            coverageTo = coverageTo,
            -- persisted so a lost Redis stash can still resolve the covered entries
            ledgerEntryIds = redeemableEntryIds,
            payoutServiceFlow
          }
      -- The bulk rail has no single-order API: the order is written locally and sent later in a batch file.
      payoutCall =
        if payoutServiceFlow == IPayout.BulkFlow
          then Bulk.localOrderCall
          else Payout.createPayoutOrder payoutServiceName ctx.person.merchantOperatingCityId ctx.person.id mbPersonBankAccount
      counterparty = counterpartyFromRole ctx.person.role
      ownerPayoutCtxFor pr = buildDriverChargeCtx counterparty ctx.driverId.getId ctx.merchantId.getId ctx.mocId.getId ctx.transporterConfig.currency pr.id.getId (fromMaybe False ctx.transporterConfig.driverWalletConfig.enableWalletGatedTierCheck)
      metadata =
        LedgerEntryMetadata
          { driverPayable = Just (negate netAmount),
            payoutOrderId = Nothing,
            reason = Nothing,
            subscriptionAllocations = Nothing,
            d2cReferralEarnings = Nothing,
            d2dReferralEarnings = Nothing,
            dailyStatsId = Nothing
          }
      initiatedPush = Notify.sendNotificationToDriver ctx.person.merchantOperatingCityId FCM.SHOW Nothing FCM.PAYOUT_INITIATED "Payout Initiated" ("Your payout of " <> show netAmount <> " has been initiated." <> if fee > 0 then " (Fee: " <> show fee <> ")" else "") ctx.person ctx.person.deviceToken

  if netAmount <= 0.0
    then pure Nothing
    else do
      -- The PayoutRequest is created first for every rail: its id anchors the kernel
      -- transfer, every ledger leg, and the PayoutOrder. Hold/submit ordering, the
      -- compensation ladder and the canonical status live in the kernel now; the rail
      -- closure keeps writing PayoutRequest/PayoutOrder statuses as derived views.
      pr <- PayoutRequest.buildPayoutRequest submission
      PayoutRequest.createPayoutRequest pr
      walletAccount <-
        getWalletAccountByOwner counterparty ctx.driverId.getId
          >>= fromMaybeM (InternalError $ "Wallet account missing for payout " <> pr.id.getId)
      let rail =
            Rails.mkWalletPayoutRail
              payoutServiceFlow
              pr
              submission.transferAmount
              ctx.transporterConfig.currency
              payoutCall
              onOrderCreated
              (\reason -> failBulkOrderIfAny mbBatchId pr.id (payoutNotSentPrefix <> reason))
          spec =
            Outbound.OutboundSpec
              { ctx = ownerPayoutCtxFor pr,
                account = walletAccount,
                -- netAmount, not payoutableBalance: the hold has always been the disbursed
                -- amount; the payout fee posts its own legs at settlement.
                amount = Exact netAmount,
                referenceType = walletReferencePayout,
                referenceId = pr.id.getId,
                ledgerRefs = walletPayoutLedgerRefs,
                instant = payoutType == PR.INSTANT,
                legacyOfferHoldKeys = [makeWalletOfferHoldsKey ctx.driverId.getId],
                coveredEntryIds = Just (map Id redeemableEntryIds),
                holdLedgerMetadata = Just metadata,
                metadata = DTransfer.emptyTransferMetadata
              }
      result <- Outbound.initiateTransferOut runFinance rail spec
      case result of
        Left err -> do
          -- Pre-submit rejection: nothing was sent and nothing is held.
          failRequest pr.id (payoutNotSentPrefix <> "KERNEL_REJECTED")
          logError $ "Wallet payout rejected by finance kernel for payoutRequest " <> pr.id.getId <> ": " <> show err
          case err of
            -- The domain gate (enableWalletPayout) passed, so a kernel refusal means the
            -- finance_account_config row is missing or wrong — be loud, not silent.
            DomainError "TRANSFER_OUT_DISABLED" _ -> throwError $ InvalidRequest "Payouts are disabled for this wallet class (finance_account_config)"
            DomainError "INSTANT_TRANSFER_NOT_ALLOWED" _ -> throwError InstantPayoutNotAllowed
            _ -> pure Nothing
        Right transfer -> case transfer.status of
          DTransfer.FAILED -> do
            -- Kernel already compensated: hold reversed, covered entries released.
            failRequest pr.id (payoutNotSentPrefix <> fromMaybe "FAILED" transfer.lastError)
            logError $ "Wallet payout not sent for " <> ctx.driverId.getId <> " | payoutRequest " <> pr.id.getId <> ": " <> fromMaybe "FAILED" transfer.lastError
            pure Nothing
          DTransfer.NEEDS_REVIEW -> do
            -- Money stays parked on OwnerPayoutLiability; ops resolve via the transfer row.
            -- The request keeps whatever status the rail left (PROCESSING on ambiguous sends).
            logError $ "Wallet payout needs review for " <> ctx.driverId.getId <> " | payoutRequest " <> pr.id.getId <> ": " <> fromMaybe "" transfer.lastError
            pure Nothing
          _ -> do
            -- Forked so the batch claim, under each wallet lock, does not wait on FCM.
            fork ("WalletPayoutInitiatedNotify:" <> pr.id.getId) initiatedPush
            maybe (pure Nothing) QPayoutOrder.findByOrderId transfer.railOrderId
  where
    -- HDFC CBX gives no bank name: use the IFSC bank code and the account's last 4 digits.
    bankLabel :: IPayout.PayoutServiceFlow -> Maybe DDBA.DriverBankAccount -> (Maybe Text, Maybe Text)
    bankLabel IPayout.BulkFlow mbAccount =
      ( T.toUpper . T.take 4 <$> nonBlank (mbAccount >>= (.ifscCode)),
        T.takeEnd 4 <$> nonBlank ((.accountId) <$> mbAccount)
      )
    bankLabel _ mbAccount = (mbAccount >>= (.bankName), mbAccount >>= (.bankAccountLast4))

    nonBlank :: Maybe Text -> Maybe Text
    nonBlank = mfilter (not . T.null) . fmap T.strip
