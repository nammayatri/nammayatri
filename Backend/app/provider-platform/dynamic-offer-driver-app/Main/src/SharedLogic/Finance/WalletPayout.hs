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
import Lib.Finance (Account, EntryType (Reversal), LedgerEntryMetadata (..), findByAccountWithFilters)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Ledger.PayoutSettlement (PayoutOutcome (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Payout.Bulk.Types as Bulk
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import SharedLogic.DriverPool.DriverPoolStateEvent (DriverPoolStateEnv)
import SharedLogic.Finance.Wallet
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

data WalletPayoutParams = WalletPayoutParams
  { payoutType :: PR.PayoutType,
    minimumPayoutAmount :: HighPrecMoney,
    enforceDailyLimit :: Bool,
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
    DriverPoolStateEnv r,
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

ensurePayoutLimitNotReached ::
  (BeamFlow m r) =>
  PayoutContext ->
  Maybe (Id Account) ->
  UTCTime ->
  m ()
ensurePayoutLimitNotReached ctx mbAccountId now = do
  let timeDiff = secondsToNominalDiffTime ctx.transporterConfig.timeDiffFromUtc
      (utcStartOfDay, utcEndOfDay) = todayRangeUTC timeDiff now
  whenJust ctx.transporterConfig.driverWalletConfig.maxWalletPayoutsPerDay $ \maxPayoutsPerDay -> do
    payoutsToday <- case mbAccountId of
      Nothing -> pure []
      Just accountId -> do
        entries <- findByAccountWithFilters accountId (Just utcStartOfDay) (Just utcEndOfDay) Nothing Nothing Nothing (Just [walletReferencePayout])
        -- Payouts never sent (hold given back) do not use up the daily limit.
        let wasNotSent e = maybe False (T.isPrefixOf payoutNotSentPrefix) (e.metadataV2 >>= (.reason))
            notSentHoldIds = [holdId | e <- entries, e.entryType == Reversal, wasNotSent e, Just holdId <- [e.reversalOf]]
        pure [e | e <- entries, e.entryType /= Reversal, e.id `notElem` notSentHoldIds]
    when (length payoutsToday >= maxPayoutsPerDay) $
      throwError $ InvalidRequest "Maximum payouts per day reached"

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
  pb <- computePayoutableBalance counterparty ctx.driverId.getId cutoff now
  when params.enforceDailyLimit $ ensurePayoutLimitNotReached ctx pb.mbAccountId now
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
    Redis.HedisLTSFlowEnv r,
    DriverPoolStateEnv r
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
            bankName = mbPersonBankAccount >>= (.bankName),
            bankAccountLast4 = mbPersonBankAccount >>= (.bankAccountLast4),
            customerName = Just ctx.person.firstName,
            customerPhone = phoneNo,
            customerEmail = ctx.person.email,
            remark = "Settlement for wallet",
            orderType = "FULFILL_ONLY",
            scheduledAt = Nothing,
            payoutType = Just payoutType,
            coverageFrom = coverageFrom,
            coverageTo = coverageTo,
            ledgerEntryIds = [],
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

      -- Juspay/Stripe: create the request and order, call the partner, then hold.
      sendThenHold = do
        result <- PayoutRequest.submitPayoutRequest submission payoutCall onOrderCreated
        case result of
          PayoutRequest.PayoutInitiated pr order -> do
            postOwnerPayoutLiability (ownerPayoutCtxFor pr) netAmount (Just metadata)
              >>= either (\err -> logError $ "Failed to move payout amount to owner payout liability for payoutRequest " <> pr.id.getId <> ": " <> show err) (const (pure ()))
            PayoutRequest.stashPayoutLedgerEntryIds pr.id.getId redeemableEntryIds
            initiatedPush
            pure (Just order)
          PayoutRequest.PayoutProcessing pr status -> do
            logInfo $ "Wallet payout already in flight for " <> ctx.driverId.getId <> " | payoutRequestId: " <> pr.id.getId <> " | status: " <> show status
            pure Nothing
          PayoutRequest.PayoutFailed _ err -> do
            logError $ "Wallet payout failed for " <> ctx.driverId.getId <> ": " <> err
            pure Nothing
          -- Only the bulk rail returns this, and the bulk rail does not come here.
          PayoutRequest.PayoutAmbiguous pr err -> do
            logError $ "Wallet payout outcome unknown for " <> ctx.driverId.getId <> " | payoutRequestId: " <> pr.id.getId <> ": " <> err
            pure Nothing

      -- Bulk (HDFC CBX): request, then hold, then local order, so the money is held before any file goes out.
      holdThenSend = do
        pr <- PayoutRequest.buildPayoutRequest submission
        PayoutRequest.createPayoutRequest pr
        let ownerPayoutCtx = ownerPayoutCtxFor pr

            -- Give the held money back to the wallet; True when it is back. Nothing held counts as back.
            giveMoneyBack code = do
              let giveBackFailed err = do
                    logError $ "WALLET_PAYOUT_GIVE_BACK_FAILED for payoutRequest " <> pr.id.getId <> ": " <> err
                    pure False
              result <- try (settleWalletPayoutLedger ownerPayoutCtx netAmount (Just metadata) (PayoutFailed (payoutNotSentPrefix <> code)))
              case result of
                Right (Right _) -> pure True
                Right (Left err) -> giveBackFailed (show err)
                Left (err :: SomeException) -> giveBackFailed (show err)

            -- Give the money back and fail the request and any order. A hold that could not be given
            -- back gets the REVERSAL_PENDING reason for ops. Only the short code is stored; the raw
            -- detail goes to the log.
            stopNotSent code detail = do
              logError $ "Wallet payout not sent for " <> ctx.driverId.getId <> " | payoutRequest " <> pr.id.getId <> " | " <> code <> ": " <> detail
              reversed <- giveMoneyBack code
              let reason = (if reversed then payoutNotSentPrefix else reversalPendingPrefix) <> code
              failRequest pr.id reason
              failBulkOrderIfAny mbBatchId pr.id reason
              when reversed $ PayoutRequest.clearPayoutLedgerEntryIds pr.id.getId
              pure Nothing

        holdResult <- try (postOwnerPayoutLiability ownerPayoutCtx netAmount (Just metadata))
        case holdResult of
          Left (err :: SomeException) -> stopNotSent "HOLD_FAILED" (show err)
          Right (Left err) -> stopNotSent "HOLD_FAILED" (show err)
          Right (Right _) -> do
            -- If this fails, give the money back rather than leave it held with no order.
            stashResult <- try (PayoutRequest.stashPayoutLedgerEntryIds pr.id.getId redeemableEntryIds)
            case stashResult of
              Left (err :: SomeException) -> stopNotSent "HOLD_RECORD_FAILED" (show err)
              Right () -> do
                -- Nothing reaches HDFC here, so any failure is ours and the money can go back.
                sendResult <- try (PayoutRequest.executePayoutRequestWithOutcome submission.transferAmount submission.currency payoutServiceFlow pr payoutCall onOrderCreated)
                case sendResult of
                  Right (PayoutRequest.Executed order) -> do
                    -- Forked so the batch claim, under each wallet lock, does not wait on FCM.
                    fork ("WalletPayoutInitiatedNotify:" <> pr.id.getId) initiatedPush
                    pure (Just order)
                  Right (PayoutRequest.ConfirmedFailure err) -> stopNotSent "SEND_FAILED" err
                  Right (PayoutRequest.AmbiguousFailure err) -> stopNotSent "SEND_FAILED" err
                  Right (PayoutRequest.NotExecutable status) -> stopNotSent ("REQUEST_" <> show status) ("request is " <> show status)
                  Left (err :: SomeException) -> stopNotSent "SEND_FAILED" (show err)

  if netAmount <= 0.0
    then pure Nothing
    else
      if payoutServiceFlow == IPayout.BulkFlow
        then holdThenSend
        else sendThenHold
