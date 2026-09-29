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
  )
where

-- NOTE (reviewer, remove before merge): what this file changes vs main, in one place.
--   * New exports: the instant-payout role gate (instantPayoutAllowedFor / ensureInstantPayoutAllowed);
--     WalletPayoutFlow, runWalletPayoutWith, PayoutPrefetch and initiateWalletPayoutWith (the one initiate behind the
--     instant payout, the Juspay/Stripe sweep and the HDFC bulk claim used by the HDFC sweep and adhoc).
--   * Juspay/Stripe enter as on main: the instant payout (DriverWallet.postWalletPayout, via
--     Bulk.Driver.runInstantPayout) and the scheduled sweep (ScheduledBatchPayout) call runWalletPayout ->
--     findWalletPayoutAmount -> initiateWalletPayout. Their one intended money-flow change is the order of steps
--     inside the initiate (hold before the partner call); see the NOTEs below.
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
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
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

-- NOTE (reviewer, remove before merge): per-city instant-payout role gate, set by the optional DriverWalletConfig
--   field instantPayoutExcludedRoles. postWalletPayout calls ensureInstantPayoutAllowed; the wallet screen uses
--   instantPayoutAllowedFor to hide the withdraw button. The sweep, adhoc and the bulk claim never call either.
--   Shared with Juspay/Stripe -- no change unless a city sets the field (absent or empty = nobody blocked = main).

-- | Whether this role may take an instant (on-demand) payout under the city's wallet config.
--   'instantPayoutExcludedRoles' bars roles per city without touching 'enableWalletPayout', which the
--   scheduled sweep also reads. FLEET_BUSINESS is a fleet owner everywhere else in the wallet, so a
--   config that bars FLEET_OWNER bars it too.
instantPayoutAllowedFor :: DTConf.DriverWalletConfig -> DP.Role -> Bool
instantPayoutAllowedFor walletConfig role = asFleetOwner role `notElem` map asFleetOwner (fromMaybe [] walletConfig.instantPayoutExcludedRoles)
  where
    asFleetOwner DP.FLEET_BUSINESS = DP.FLEET_OWNER
    asFleetOwner r = r

-- | Instant payouts only: scheduled, bulk and adhoc payouts never pass through here. A city where
--   payouts must be started only by an admin or the scheduled sweep lists the self-service roles in
--   'instantPayoutExcludedRoles'.
ensureInstantPayoutAllowed :: (MonadFlow m) => PayoutContext -> m ()
ensureInstantPayoutAllowed ctx =
  unless (instantPayoutAllowedFor ctx.transporterConfig.driverWalletConfig ctx.person.role) $
    throwError InstantPayoutNotAllowed

-- NOTE (reviewer, remove before merge): daily payout limit (maxWalletPayoutsPerDay). Main counts every non-Reversal
--   walletReferencePayout ledger row of today; a refused payout leaves no row there, as main holds only after the
--   partner accepts. Here the hold comes first, so a payout that was never sent leaves a hold plus a reversal whose
--   reason starts with payoutNotSentPrefix (written only by stopNotSent below). Such holds are skipped. A payout
--   that went out and failed later (reversal "Payout failed: ...") still counts, as on main.
--   Shared with Juspay/Stripe -- intended change that keeps main's count.
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
        -- A payout that was never sent (its hold given back at once, with the "not sent" reason)
        -- doesn't count.
        let wasNotSent e = maybe False (T.isPrefixOf payoutNotSentPrefix) (e.metadataV2 >>= (.reason))
            notSentHoldIds = [holdId | e <- entries, e.entryType == Reversal, wasNotSent e, Just holdId <- [e.reversalOf]]
        pure [e | e <- entries, e.entryType /= Reversal, e.id `notElem` notSentHoldIds]
    when (length payoutsToday >= maxPayoutsPerDay) $
      throwError $ InvalidRequest "Maximum payouts per day reached"

-- NOTE (reviewer, remove before merge): the not-sent helpers start here (new; main never holds money before the
--   partner call, so it needs none). stopNotSent writes payoutNotSentPrefix into the reversal reason and into
--   payout_request.failure_reason; the daily limit above reads it.
--   Shared with Juspay/Stripe -- intended: a refused payout gets failure_reason "Payout not sent: partner refused:
--   ..." (main leaves it NULL; the text is only in the history row).

-- | The start of the reason written when held money is given back because the payout was never
--   sent. The daily payout limit skips these, so a refused payout doesn't use up a daily payout.
payoutNotSentPrefix :: Text
payoutNotSentPrefix = "Payout not sent: "

-- NOTE (reviewer, remove before merge): only stopNotSent calls failRequest. When the partner refused, the lib
--   (Lib.Payment.Payout.Request.executePayoutRequestInternal) has already set AUTO_PAY_FAILED with a history row, so
--   the `unless` skips a second history row and only failure_reason is added. Shared with Juspay/Stripe -- intended:
--   same final status as main (AUTO_PAY_FAILED), plus failure_reason. Open point: if a success webhook later makes
--   the request CREDITED, nothing clears this failure_reason (main leaves it NULL).

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

-- NOTE (reviewer, remove before merge): bulk-only. It acts only when mbBatchId is Just, and only the bulk callers pass
--   one (the HDFC bulk claim in SharedLogic/Payout/Bulk/Claim.hs and the bulk-rail instant payout). Juspay/Stripe go
--   through initiateWalletPayout below, which passes Nothing, so for them this does nothing.

-- | Bulk only: if an order was already made for this request, mark it failed so it never goes in a
--   file. Nothing else has an order in a batch.
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
      QPayoutOrder.updateBulkSettled IPayout.FAILURE Nothing Nothing Nothing (Just "NOT_SENT") (Just reason) order.id order.orderId

resolvePayoutVpa :: (MonadFlow m) => PayoutContext -> m Text
resolvePayoutVpa ctx =
  fromMaybeM (InternalError $ "Payout vpa not present for " <> ctx.driverId.getId) ctx.payoutVpa

computePayoutFee :: Maybe DTConf.PayoutFeeConfig -> HighPrecMoney -> HighPrecMoney
computePayoutFee Nothing _ = 0
computePayoutFee (Just feeConfig) amount =
  min amount . SPayment.roundToTwoDecimalPlaces $ computeStripePayoutFee feeConfig amount

-- NOTE (reviewer, remove before merge): runWalletPayout is main's, word for word. Callers: the Juspay/Stripe sweep
--   (ScheduledBatchPayout.processOneWalletPayout) and Bulk.Driver.runInstantPayout for any city not on a bulk partner.
--   The bulk-rail instant payout uses runWalletPayoutWith below.

-- | One wallet payout attempt for a payee under the wallet lock: the amount finder and the
--   initiate (payout request + OwnerPayoutLiability hold) both run inside it.
runWalletPayout :: (WalletPayoutFlow m r) => PayoutContext -> WalletPayoutParams -> m ()
runWalletPayout ctx params =
  PayoutRequest.runPayoutUnderLock (makeWalletRunningBalanceLockKey ctx.driverId.getId) 10 (findWalletPayoutAmount ctx params) (initiateWalletPayout ctx params.payoutType)

-- NOTE (reviewer, remove before merge): new, bulk-only. Same lock (key, TTL) and same amount, minimum and daily-limit
--   checks as runWalletPayout, with the last step passed in. Only Bulk.Driver.runInstantBulkPayout calls it, to open
--   a batch of one under the lock. Juspay/Stripe still run runWalletPayout, main's code.

-- | 'runWalletPayout' with the step that makes the payout passed in: the same wallet lock and the
--   same amount checks, whatever rail the payout then goes out on.
runWalletPayoutWith :: (WalletPayoutFlow m r) => PayoutContext -> WalletPayoutParams -> (WalletPayoutPlan -> m ()) -> m ()
runWalletPayoutWith ctx params initiate =
  PayoutRequest.runPayoutUnderLock (makeWalletRunningBalanceLockKey ctx.driverId.getId) 10 (findWalletPayoutAmount ctx params) initiate

-- NOTE (reviewer, remove before merge): main does the balance reads inline here (wallet account, wallet balance,
--   eligibility, PENDING ride holds, offer holds, then max 0 (redeemable - rideHold - offerHold)). Here they are
--   SharedLogic.Finance.Wallet.computePayoutableBalance, same calls and formula, which every wallet payout path uses
--   (instant, both sweeps, adhoc), so all pay main's Juspay/Stripe amount.
--   Shared with Juspay/Stripe -- no behaviour change: same reads, same formula, same log line. Only the order differs:
--   main checks the daily limit between the balance reads, here after them; both only read, so the result is the
--   same.
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

-- NOTE (reviewer, remove before merge): what runWalletPayout runs (the Juspay/Stripe instant payout and sweep). It
--   calls initiateWalletPayoutWith with main's values: no batch, coverageFrom Nothing, coverageTo = cutoff, no
--   prefetch (route and phone resolved inside, as on main) and main's afterPayoutOrderCreated. The one intended
--   change is in initiateWalletPayoutWith (hold before the partner call).

-- | The initiate 'runWalletPayout' runs: the instant payout and the Juspay/Stripe scheduled sweep.
--   Same code as every other wallet payout path ('initiateWalletPayoutWith'), with their values: no
--   batch, coverage up to the cutoff, and a per-order status-check job once the order exists.
initiateWalletPayout :: (WalletPayoutFlow m r) => PayoutContext -> PR.PayoutType -> WalletPayoutPlan -> m ()
initiateWalletPayout ctx payoutType WalletPayoutPlan {..} =
  void $ initiateWalletPayoutWith ctx payoutableBalance payoutType Nothing (Just cutoff) redeemableEntryIds merchantTransferAmount Nothing Nothing afterPayoutOrderCreated

-- NOTE (reviewer, remove before merge): new type, bulk-only in practice. Only the bulk callers pass Just: the HDFC
--   bulk claim (SharedLogic/Payout/Bulk/Claim.hs) and the bulk-rail instant payout (Bulk.Driver.runInstantBulkPayout).
--   Both already have the route (the batch's service and the bank account they checked) and decrypt the phone before
--   the lock, so nothing is looked up again under it. Juspay/Stripe pass Nothing and both are resolved inside, as on
--   main.

-- | What a caller has already resolved before taking the wallet lock, so 'initiateWalletPayoutWith'
--   does not redo it while holding the lock: the payout route (flow, service name, bank account) and
--   the decrypted phone number, which is a call to the encryption service.
data PayoutPrefetch = PayoutPrefetch
  { route :: (IPayout.PayoutServiceFlow, DEMSC.ServiceName, Maybe DDBA.DriverBankAccount),
    customerPhone :: Maybe Text
  }

-- NOTE (reviewer, remove before merge): the main change in this file for Juspay/Stripe (all their flows).
--   Main: create the request -> call the partner (submitPayoutRequest) -> only on PayoutInitiated write the hold; a
--   failed hold is only logged, so money can be paid out and still look payable.
--   Here: 1. create the request (INITIATED); 2. write the hold -- if that fails, stopNotSent and nothing is sent;
--   3. create the order and call the partner (executePayoutRequestWithOutcome); 4. partner refused -> stopNotSent
--   (money given back, request AUTO_PAY_FAILED); 5. unclear error after the call -> keepHold.
--   Same as main for Juspay/Stripe: fee and floor to cents, every submission field (batchId = Nothing; coverage
--   values from the caller equal main's), payoutCall, hold metadata, entry-id stash, the inline "Payout Initiated"
--   push and the afterPayoutOrderCreated status job.
--   Juspay/Stripe -- intended change: hold before the partner call; net money is the same as main. Two open points
--   are in the NOTEs on the case at the end.

-- | Create the payout request and order for one beneficiary and hold the amount. The single
--   implementation behind every wallet payout path: instant and the Juspay/Stripe sweep
--   ('initiateWalletPayout'), and the bulk (HDFC CBX) claim behind the HDFC sweep and adhoc, which
--   also needs the created 'PayoutOrder' back so a file can be assembled out of everything claimed
--   in a cycle.
--
--   The order of steps: the request, then the hold, then the order and the partner call. The hold
--   ('postOwnerPayoutLiability') moves the amount OwnerLiability -> OwnerPayoutLiability, so the
--   wallet balance already excludes it and a second payout cannot see the same money as payable.
--   Writing it before the partner call means a payout never goes out without its hold: if the hold
--   fails, nothing is sent; if the payout isn't sent for any reason, the money is given back.
--   The entry ids are stashed for the settlement path, and 'ledgerEntryIds' is left empty in the
--   submission.
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

  -- | The batch this payout is claimed into on the bulk path, where the batch is opened before its
  --   members. Nothing everywhere else -- there is no batch.
  Maybe (Id DPayoutBatch.PayoutBatch) ->
  -- | Resolved by the caller outside the lock (the bulk claim); Nothing resolves it here.
  Maybe PayoutPrefetch ->
  -- | What to run once the order is persisted, e.g. scheduling a per-order status check. Taken as a
  --   parameter rather than resolved here so this function needs no job-creator constraint, which
  --   would otherwise cascade through the whole bulk claim path. Ignored on the bulk rail -- see
  --   'onOrderCreated' below.
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
    -- No VPA on the bulk (HDFC CBX) rail: it pays to an account number and IFSC, resolved above
    -- (passed in by the bulk claim, else by Tools.Payout.getCreatePayoutServiceFlow).
    IPayout.BulkFlow -> do
      -- NOTE (reviewer, remove before merge): bulk-only arm (Juspay/Stripe arms above are main's). A bulk-rail order
      --   goes out only inside a batch; one made without a batch would be held and never sent. Every bulk caller
      --   passes its batch, so this only stops a misrouted call, before any request or hold is written.
      when (isNothing mbBatchId) $
        throwError (InvalidRequest "A payout on the bulk rail must belong to a payout batch")
      pure Nothing
  let fee = computePayoutFee ctx.transporterConfig.driverWalletConfig.payoutFee payoutableBalance
      -- Floor (not round-half-up) to whole cents so the disbursed amount never exceeds the
      -- wallet balance; otherwise the hold can overdraw the wallet by a sub-cent rounding delta.
      netAmount = SPayment.floorToTwoDecimalPlaces (payoutableBalance - fee)
      -- HDFC CBX resolves a whole file at a time and has no per-order status API, so an order on
      -- that rail must never get a per-order status-check job -- it could never be answered. A
      -- caller that passes one anyway is ignored. Every other rail keeps main's hook.
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
            -- mbPersonBankAccount is the account the payout is actually sent to, which for a
            -- fleet driver is the fleet owner's rather than their own.
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
      payoutCall = Payout.createPayoutOrder payoutServiceName ctx.person.merchantOperatingCityId ctx.person.id mbPersonBankAccount

  -- NOTE (reviewer, remove before merge): main does `when (netAmount > 0.0) ...` and returns (). This returns Nothing
  --   when there is nothing to pay, so the bulk callers can tell "no order". Same for Juspay/Stripe: initiateWalletPayout
  --   `void`s the result.
  if netAmount <= 0.0
    then pure Nothing
    else do
      -- 1. Create the payout request (INITIATED).
      pr <- PayoutRequest.buildPayoutRequest submission
      PayoutRequest.createPayoutRequest pr
      let counterparty = counterpartyFromRole ctx.person.role
          ownerPayoutCtx = buildDriverChargeCtx counterparty ctx.driverId.getId ctx.merchantId.getId ctx.mocId.getId ctx.transporterConfig.currency pr.id.getId (fromMaybe False ctx.transporterConfig.driverWalletConfig.enableWalletGatedTierCheck)
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
          isBulk = payoutServiceFlow == IPayout.BulkFlow

          -- NOTE (reviewer, remove before merge): the give-money-back helpers (all new).
          --   giveMoneyBack reverses the hold with settleWalletPayoutLedger ... PayoutFailed, the same call main's
          --   webhook uses for a failed payout; only the reason differs ("Payout not sent: ..."). A ledger error is
          --   only logged (WALLET_PAYOUT_GIVE_BACK_FAILED): the hold stays and nothing retries it.
          --   stopNotSent (hold failed, partner refused, or a bulk local error): money back, request AUTO_PAY_FAILED
          --   with the reason, bulk order failed (bulk only), stashed entry ids cleared.
          --   keepHold (Juspay/Stripe only, step 5): the hold stays for the webhook or the per-order status job.
          --   sendInitiatedPush: main's push and text, inline for Juspay/Stripe as on main, forked only for bulk.
          -- Give the held money back to the wallet. Does nothing if nothing is held.
          giveMoneyBack why = do
            result <- settleWalletPayoutLedger ownerPayoutCtx netAmount (Just metadata) (PayoutFailed (payoutNotSentPrefix <> why))
            case result of
              Right _ -> pure ()
              Left err -> logError $ "WALLET_PAYOUT_GIVE_BACK_FAILED for payoutRequest " <> pr.id.getId <> ": " <> show err

          -- The payout was never sent: give the money back, and fail the request and its bulk order
          -- (if one was made, so it never goes in a file).
          stopNotSent why = do
            logError $ "Wallet payout not sent for " <> ctx.driverId.getId <> " | payoutRequest " <> pr.id.getId <> ": " <> why
            giveMoneyBack why
            failRequest pr.id (payoutNotSentPrefix <> why)
            failBulkOrderIfAny mbBatchId pr.id (payoutNotSentPrefix <> why)
            PayoutRequest.clearPayoutLedgerEntryIds pr.id.getId
            pure Nothing

          -- Juspay/Stripe, after the partner call: the money may already be on its way, so the hold
          -- stays. The webhook or the status check settles it.
          keepHold why = do
            logError $ "Wallet payout error after calling the partner for " <> ctx.driverId.getId <> " | payoutRequest " <> pr.id.getId <> ": " <> why <> " -- the hold stays until the webhook or status check settles it"
            pure Nothing

          sendInitiatedPush = do
            let push = Notify.sendNotificationToDriver ctx.person.merchantOperatingCityId FCM.SHOW Nothing FCM.PAYOUT_INITIATED "Payout Initiated" ("Your payout of " <> show netAmount <> " has been initiated." <> if fee > 0 then " (Fee: " <> show fee <> ")" else "") ctx.person ctx.person.deviceToken
            -- Juspay/Stripe send it inline, as they always have. The bulk claim forks it: it runs for
            -- every beneficiary of a batch under their wallet lock, and should not wait on FCM.
            if isBulk
              then fork ("WalletPayoutInitiatedNotify:" <> pr.id.getId) push
              else push

      -- 2. Hold the money BEFORE anything is sent, so a payout never goes out without its hold.
      holdResult <- try (postOwnerPayoutLiability ownerPayoutCtx netAmount (Just metadata))
      case holdResult of
        Left (err :: SomeException) -> stopNotSent ("Wallet hold failed: " <> show err)
        Right (Left err) -> stopNotSent ("Wallet hold failed: " <> show err)
        Right (Right _) -> do
          PayoutRequest.stashPayoutLedgerEntryIds pr.id.getId redeemableEntryIds
          -- 3. Create the order and call the partner (Juspay / Stripe / HDFC's local step).
          sendResult <- try (PayoutRequest.executePayoutRequestWithOutcome submission.transferAmount submission.currency payoutServiceFlow pr payoutCall onOrderCreated)
          case sendResult of
            -- NOTE (reviewer, remove before merge): open point, same on main. The shared kernel catches a Stripe
            --   refusal of POST /v1/payouts and returns a FAILURE order, so it lands here: request PROCESSING, hold
            --   kept, "Payout Initiated" push, and nothing settles it (FAILURE is final, so no status job; no Stripe
            --   payout, so no webhook). Main strands the hold the same way. Possible fix: treat a FAILURE order with no
            --   external id as "not sent".
            Right (PayoutRequest.Executed order) -> do
              sendInitiatedPush
              pure (Just order)
            -- NOTE (reviewer, remove before merge): open point. For Juspay/Stripe the lib turns every exception in the
            --   partner-call step into ConfirmedFailure (executePayoutRequestInternal, the `/= BulkFlow` guard): a
            --   timeout, a 5xx, a decode error, even our own DB write failing after Juspay accepted. All of these give
            --   the money back here instead of keeping the hold. Net effect = main (no hold; request AUTO_PAY_FAILED).
            --   If the partner did pay, the later success webhook posts a fresh hold and settles it; that CREDITED
            --   request keeps the "Payout not sent" failure_reason.
            -- The partner refused it: nothing was paid.
            Right (PayoutRequest.ConfirmedFailure err) -> stopNotSent ("partner refused: " <> err)
            -- NOTE (reviewer, remove before merge): the three arms below. For Juspay/Stripe keepHold is reached only via
            --   NotExecutable (no order row came back from the partner call) or an exception that escapes
            --   executePayoutRequestWithOutcome (e.g. the post-call transaction-id / status write failing). Both come
            --   after the partner call, so the hold is kept. Main writes no hold and lets the exception reach the
            --   caller; here it is logged, the hold stays, and the call returns normally. For bulk all three are
            --   stopNotSent: nothing has gone to HDFC yet (the "partner call" is the local Bulk.localOrderCall).
            -- Only bulk returns this. On bulk nothing goes to HDFC here, so it is our own error.
            Right (PayoutRequest.AmbiguousFailure err)
              | isBulk -> stopNotSent err
              | otherwise -> keepHold err
            Right (PayoutRequest.NotExecutable status)
              | isBulk -> stopNotSent ("request is " <> show status)
              | otherwise -> keepHold ("request is " <> show status)
            Left (err :: SomeException)
              | isBulk -> stopNotSent (show err)
              | otherwise -> keepHold (show err)
