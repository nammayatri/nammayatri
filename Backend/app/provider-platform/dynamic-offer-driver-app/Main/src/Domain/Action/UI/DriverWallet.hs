{-# OPTIONS_GHC -Wno-orphans #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverWallet
  ( getWalletBalance,
    getWalletTransactions,
    postWalletPayout,
    postWalletTopup,
    recordAirportCashRecharge,
    getWalletPayoutHistory,
    -- Exported for scheduled batch payout
    PayoutContext (..),
    loadPayoutContext,
    counterpartyFromRole,
    computePayoutFee,
    resolvePayoutVpa,
    initiateWalletPayout,
    makePayoutEntryIdsKey,
    mkDriverWalletFinanceCtx,
  )
where

import qualified API.Types.UI.DriverWallet as DriverWallet
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Time
import Domain.Action.UI.Plan hiding (mkDriverFee)
import Domain.Types.Extra.Plan
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payout.Types as TPayout
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.HideSecrets
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance
  ( Account,
    AccountRole (OwnerLiability, PlatformAsset),
    CounterpartyType,
    FinanceCtx (..),
    InvoiceConfig (..),
    InvoiceLineItem (..),
    findByAccountWithFilters,
    getEntriesByReference,
    invoice,
    runFinance,
    transfer,
  )
import qualified Lib.Finance.Domain.Types.Account as FAccount
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Payout.PayoutItems as PayoutItems
import qualified Lib.Payment.Payout.Request as PayoutRequest
import SharedLogic.Finance.Prepaid (counterpartyDriver, counterpartyFleetOwner)
import SharedLogic.Finance.Wallet
import qualified SharedLogic.Payment as SPayment
import Storage.Cac.TransporterConfig (findByMerchantOpCityId)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOI
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as Payout

instance Kernel.Types.HideSecrets.HideSecrets DriverWallet.TopUpRequest where
  hideSecrets = Kernel.Prelude.identity

-- | Pick the counterparty type based on the person's role.
counterpartyFromRole :: DP.Role -> CounterpartyType
counterpartyFromRole DP.FLEET_OWNER = counterpartyFleetOwner
counterpartyFromRole DP.FLEET_BUSINESS = counterpartyFleetOwner
counterpartyFromRole _ = counterpartyDriver

--------------------------------------------------------------------------------
-- getWalletBalance
--------------------------------------------------------------------------------

getWalletBalance ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow DriverWallet.WalletBalanceResponse
  )
getWalletBalance (mbPersonId, _merchantId, _mocId) = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let counterparty = counterpartyFromRole person.role
  currentBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty driverId.getId
  pure $ DriverWallet.WalletBalanceResponse {currentBalance}

--------------------------------------------------------------------------------
-- getWalletTransactions
--------------------------------------------------------------------------------

getWalletTransactions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Environment.Flow DriverWallet.WalletSummaryResponse
  )
getWalletTransactions (mbPersonId, _merchantId, mocId) mbFromDate mbToDate = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let counterparty = counterpartyFromRole person.role
  transporterConfig <- findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  now <- getCurrentTime
  let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
      fromDate = fromMaybe (Data.Time.UTCTime (Data.Time.utctDay now) 0) mbFromDate
      toDate = fromMaybe now mbToDate
      cutOffDays = transporterConfig.driverWalletConfig.payoutCutOffDays
      cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
  mbAccount <- getWalletAccountByOwner counterparty driverId.getId
  case mbAccount of
    Nothing -> pure emptyWalletSummary
    Just acc -> do
      currentBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty driverId.getId
      (additions, deductions, nonRedeemableBalance) <- classifyEntries acc.id fromDate toDate cutoff
      let redeemableBalance = max 0 (currentBalance - nonRedeemableBalance)
      pure $
        DriverWallet.WalletSummaryResponse
          { currentBalance,
            redeemableBalance,
            nonRedeemableBalance,
            additions,
            deductions
          }

emptyWalletSummary :: DriverWallet.WalletSummaryResponse
emptyWalletSummary =
  DriverWallet.WalletSummaryResponse
    { currentBalance = 0,
      redeemableBalance = 0,
      nonRedeemableBalance = 0,
      additions = DriverWallet.WalletItemGroup {totalAmount = 0, items = []},
      deductions = DriverWallet.WalletItemGroup {totalAmount = 0, items = []}
    }

-- | Fetch entries in a date range, partition into additions/deductions,
--   group by reference type, and compute per-item redeemable/nonRedeemable.
--   Also returns the top-level nonRedeemableBalance (sum of additions after cutoff).
classifyEntries ::
  (BeamFlow m r) =>
  Id Account ->
  UTCTime ->
  UTCTime ->
  UTCTime -> -- payout cutoff time
  m (DriverWallet.WalletItemGroup, DriverWallet.WalletItemGroup, HighPrecMoney)
classifyEntries accountId fromDate toDate cutoff = do
  -- Use walletCreditRefs (single source of truth) + debit-only refs for full picture
  let allRefs = walletCreditRefs ++ [walletReferencePayout]
  entries <- findByAccountWithFilters accountId (Just fromDate) (Just toDate) Nothing Nothing Nothing (Just allRefs)
  let (addEntries, dedEntries) = partition (\e -> e.toAccountId == accountId) entries

      -- For additions: compute total and non-redeemable (after cutoff) per reference type
      addRefMap = foldl' (\acc e -> Map.insertWith (\(a1, b1) (a2, b2) -> (a1 + a2, b1 + b2)) e.referenceType (e.amount, if e.timestamp >= cutoff then e.amount else 0) acc) Map.empty addEntries
      addItems =
        map
          ( \(ref, (total, nonRedeem)) ->
              DriverWallet.WalletItem
                { itemReference = ref,
                  itemName = referenceTypeToItemName ref,
                  itemValue = total,
                  redeemableBalance = max 0 (total - nonRedeem),
                  nonRedeemableBalance = nonRedeem
                }
          )
          (Map.toList addRefMap)
      addTotal = sum (map (\(_, (t, _)) -> t) (Map.toList addRefMap))
      topLevelNonRedeemable = sum (map (\(_, (_, nr)) -> nr) (Map.toList addRefMap))

      -- For deductions: all balance is redeemable
      dedRefMap = foldl' (\acc e -> Map.insertWith (+) e.referenceType e.amount acc) Map.empty dedEntries
      dedItems =
        map
          ( \(ref, val) ->
              DriverWallet.WalletItem
                { itemReference = ref,
                  itemName = referenceTypeToItemName ref,
                  itemValue = val,
                  redeemableBalance = val,
                  nonRedeemableBalance = 0
                }
          )
          (Map.toList dedRefMap)
      dedTotal = sum (Map.elems dedRefMap)

  pure (DriverWallet.WalletItemGroup {totalAmount = addTotal, items = addItems}, DriverWallet.WalletItemGroup {totalAmount = dedTotal, items = dedItems}, topLevelNonRedeemable)

referenceTypeToItemName :: Text -> Text
referenceTypeToItemName ref
  | ref == walletReferenceBaseRide = "Ride Earnings"
  | ref == walletReferenceTollCharges = "Toll Charges"
  | ref == walletReferenceParkingCharges = "Parking Charges"
  | ref == walletReferenceTopup = "Wallet Top-up"
  | ref == walletReferenceGSTOnline = "GST (Online)"
  | ref == walletReferenceGSTCash = "GST (Cash)"
  | ref == walletReferenceTDSDeductionOnline = "TDS (Online)"
  | ref == walletReferenceTDSDeductionCash = "TDS (Cash)"
  | ref == walletReferencePayout = "Withdrawal"
  | ref == walletReferenceAirportCashRecharge = "Airport cash recharge (booth)"
  | otherwise = ref

--------------------------------------------------------------------------------
-- getWalletPayoutHistory
--------------------------------------------------------------------------------

getWalletPayoutHistory ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe [PR.PayoutRequestStatus] ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow DriverWallet.PayoutHistoryResponse
  )
getWalletPayoutHistory (mbPersonId, _merchantId, _mocId) mbFrom mbTo mbStatuses mbLimit mbOffset = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  let statuses = fromMaybe [] mbStatuses
      limit = mbLimit
      offset = mbOffset

  items <- PayoutItems.getPayoutItems driverId.getId mbFrom mbTo statuses limit offset

  let apiItems = map toHistoryItem items
      totalPaidOut = sum [fromMaybe 0 i.amount | i <- items, isPaidOut i.status]
      totalPending = sum [fromMaybe 0 i.amount | i <- items, isPending i.status]
      lastPayout = Kernel.Prelude.listToMaybe [toHistoryItem i | i <- items, isPaidOut i.status]

  pure $
    DriverWallet.PayoutHistoryResponse
      { totalPaidOut,
        lastPayout,
        totalPending,
        items = apiItems
      }
  where
    isPaidOut s = s == PR.CREDITED || s == PR.CASH_PAID
    isPending s = s == PR.INITIATED || s == PR.PROCESSING

    toHistoryItem :: PayoutItems.PayoutItem -> DriverWallet.PayoutHistoryItem
    toHistoryItem i =
      DriverWallet.PayoutHistoryItem
        { amount = i.amount,
          payoutFee = i.payoutFee,
          entityName = i.entityName,
          status = i.status,
          timestamp = i.timestamp,
          payoutMethod = i.payoutMethod,
          payoutVpa = i.payoutVpa
        }

--------------------------------------------------------------------------------
-- postWalletPayout
--------------------------------------------------------------------------------

-- | All context needed for payout operations, loaded once at the start.
data PayoutContext = PayoutContext
  { driverId :: Kernel.Types.Id.Id DP.Person,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    mocId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    person :: DP.Person,
    payoutVpa :: Maybe Text,
    transporterConfig :: DTConf.TransporterConfig
  }

loadPayoutContext ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person) ->
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m PayoutContext
loadPayoutContext mbPersonId merchantId mocId = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  transporterConfig <- findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
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

-- | Validate that wallet payouts are enabled for this merchant.
ensurePayoutsEnabled :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => PayoutContext -> m ()
ensurePayoutsEnabled ctx = do
  merchant <- CQM.findById ctx.merchantId >>= fromMaybeM (MerchantNotFound ctx.merchantId.getId)
  let isPrepaidAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
  unless (isPrepaidAndWalletEnabled && ctx.transporterConfig.driverWalletConfig.enableWalletPayout) $
    throwError $ InvalidRequest "Payouts are disabled"

-- | Check that the driver hasn't exceeded the maximum daily payout count.
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
      Just accountId -> findByAccountWithFilters accountId (Just utcStartOfDay) (Just utcEndOfDay) Nothing Nothing Nothing (Just [walletReferencePayout])
    when (length payoutsToday >= maxPayoutsPerDay) $
      throwError $ InvalidRequest "Maximum payouts per day reached"

-- | Validate that the payoutable balance meets the minimum threshold.
ensureMinimumPayoutAmount :: (MonadFlow m) => PayoutContext -> HighPrecMoney -> m ()
ensureMinimumPayoutAmount ctx payoutableBalance = do
  let minAmount = ctx.transporterConfig.driverWalletConfig.minimumWalletPayoutAmount
  when (payoutableBalance < minAmount) $
    throwError $ InvalidRequest ("Minimum payout amount is " <> show minAmount)

-- | Extract and validate the payout VPA (works for both drivers and fleet owners).
resolvePayoutVpa :: (MonadFlow m) => PayoutContext -> m Text
resolvePayoutVpa ctx =
  fromMaybeM (InternalError $ "Payout vpa not present for " <> ctx.driverId.getId) ctx.payoutVpa

postWalletPayout ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow APISuccess.APISuccess
  )
postWalletPayout (mbPersonId, merchantId, mocId) = do
  ctx <- loadPayoutContext mbPersonId merchantId mocId
  ensurePayoutsEnabled ctx
  let counterparty = counterpartyFromRole ctx.person.role
  Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey ctx.driverId.getId) 10 10 $ do
    now <- getCurrentTime
    mbAccount <- getWalletAccountByOwner counterparty ctx.driverId.getId
    let mbAccountId = (.id) <$> mbAccount
    walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty ctx.driverId.getId
    ensurePayoutLimitNotReached ctx mbAccountId now
    -- Single query: get both non-redeemable balance and redeemable entry IDs
    let timeDiff = secondsToNominalDiffTime ctx.transporterConfig.timeDiffFromUtc
        cutOffDays = ctx.transporterConfig.driverWalletConfig.payoutCutOffDays
        cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
    (nonRedeemable, redeemableIds) <- case mbAccountId of
      Nothing -> pure (0, [])
      Just accountId -> getPayoutEligibilityData accountId cutoff now
    let payoutableBalance = walletBalance - nonRedeemable
    ensureMinimumPayoutAmount ctx payoutableBalance
    vpa <- resolvePayoutVpa ctx
    initiateWalletPayout ctx vpa payoutableBalance PR.INSTANT Nothing (Just cutoff) (map (.getId) redeemableIds)
  pure APISuccess.Success

-- | Compute the payout fee based on the PayoutFeeConfig.
--   Returns 0 if no config is set.
computePayoutFee :: Maybe DTConf.PayoutFeeConfig -> HighPrecMoney -> HighPrecMoney
computePayoutFee Nothing _ = 0
computePayoutFee (Just feeConfig) amount =
  case feeConfig.feeType of
    DTConf.PERCENTAGE -> SPayment.roundToTwoDecimalPlaces $ amount * feeConfig.feeValue / 100
    DTConf.FIXED -> min feeConfig.feeValue amount -- fee cannot exceed the amount

-- | Create a PayoutRequest (INITIATED), then call Juspay createPayoutService (→ PROCESSING).
--   No ledger entry here — that happens in the webhook handler on SUCCESS.
initiateWalletPayout ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    BeamFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl]
  ) =>
  PayoutContext ->
  Text -> -- VPA
  HighPrecMoney -> -- payoutable balance
  PR.PayoutType -> -- INSTANT or SCHEDULED
  Maybe UTCTime -> -- coverageFrom
  Maybe UTCTime -> -- coverageTo
  [Text] -> -- redeemable entry IDs for settlement
  m ()
initiateWalletPayout ctx vpa payoutableBalance payoutType coverageFrom coverageTo redeemableEntryIds = do
  phoneNo <- mapM decrypt ctx.person.mobileNumber
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName ctx.person.merchantOperatingCityId Nothing PREPAID_SUBSCRIPTION
      >>= fromMaybeM (NoSubscriptionConfigForService ctx.person.merchantOperatingCityId.getId "PREPAID_SUBSCRIPTION")
  payoutServiceName <- Payout.decidePayoutService (fromMaybe (DEMSC.PayoutService TPayout.Juspay) subscriptionConfig.payoutServiceName) ctx.person.clientSdkVersion ctx.person.merchantOperatingCityId
  merchantOperatingCity <- CQMOC.findById (Kernel.Types.Id.cast ctx.person.merchantOperatingCityId) >>= fromMaybeM (MerchantOperatingCityNotFound ctx.person.merchantOperatingCityId.getId)

  let fee = computePayoutFee ctx.transporterConfig.driverWalletConfig.payoutFee payoutableBalance
      netAmount = SPayment.roundToTwoDecimalPlaces (payoutableBalance - fee)
      submission =
        PayoutRequest.PayoutSubmission
          { beneficiaryId = ctx.driverId.getId,
            entityName = DPayment.DRIVER_WALLET_TRANSACTION,
            entityId = ctx.driverId.getId,
            entityRefId = Nothing,
            amount = netAmount,
            payoutFee = if fee > 0 then Just fee else Nothing,
            merchantId = ctx.merchantId.getId,
            merchantOpCityId = ctx.mocId.getId,
            city = show merchantOperatingCity.city,
            vpa = vpa,
            customerName = Just ctx.person.firstName,
            customerPhone = phoneNo,
            customerEmail = ctx.person.email,
            remark = "Settlement for wallet",
            orderType = "FULFILL_ONLY",
            scheduledAt = Nothing,
            payoutType = Just payoutType,
            coverageFrom = coverageFrom,
            coverageTo = coverageTo
          }
      payoutCall = Payout.createPayoutOrder ctx.person.merchantId ctx.person.merchantOperatingCityId payoutServiceName (Just ctx.person.id.getId)

  when (netAmount > 0.0) $ do
    result <- PayoutRequest.submitPayoutRequest submission payoutCall
    case result of
      PayoutRequest.PayoutInitiated pr _ -> do
        -- Stash redeemable entry IDs in Redis for the webhook handler to settle
        unless (null redeemableEntryIds) $
          Redis.setExp (makePayoutEntryIdsKey pr.id.getId) redeemableEntryIds 86400 -- 24h TTL
        Notify.sendNotificationToDriver ctx.person.merchantOperatingCityId FCM.SHOW Nothing FCM.PAYOUT_INITIATED "Payout Initiated" ("Your payout of " <> show netAmount <> " has been initiated." <> if fee > 0 then " (Fee: " <> show fee <> ")" else "") ctx.person ctx.person.deviceToken
      PayoutRequest.PayoutFailed _ err ->
        logError $ "Wallet payout failed for driver " <> ctx.driverId.getId <> ": " <> err

-- | Redis key for stashing redeemable entry IDs during payout.
makePayoutEntryIdsKey :: Text -> Text
makePayoutEntryIdsKey payoutRequestId = "payout-entry-ids:" <> payoutRequestId

--------------------------------------------------------------------------------
-- postWalletTopup (finance ledger: platform Asset -> driver RideCredit, reference WalletTopup)
--------------------------------------------------------------------------------

postWalletTopup ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    DriverWallet.TopUpRequest ->
    Environment.Flow PlanSubscribeRes
  )
postWalletTopup (mbPersonId, merchantId, mocId) = doWalletTopup mbPersonId merchantId mocId
  where
    doWalletTopup mbP mId mocId0 r =
      do
        driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbP
        when (r.amount <= 0) $ throwError $ InvalidRequest "Top-up amount must be greater than zero"
        Redis.whenWithLockRedisAndReturnValue (makeWalletTopupLockKey driverId.getId) 10 $ do
          (createOrderResp, orderId) <- SPayment.createWalletTopupOrder (driverId, mId, mocId0) r.amount
          pure $
            PlanSubscribeRes
              { orderId = orderId,
                orderResp = createOrderResp
              }
        >>= \case
          Right res -> pure res
          Left _ -> throwError WalletTopupLockFailed

    makeWalletTopupLockKey :: Text -> Text
    makeWalletTopupLockKey dId = "wallet-topup-lock:" <> dId

--------------------------------------------------------------------------------
-- recordAirportCashRecharge (booth operator took cash; credit driver wallet, idempotent by referenceId)
--------------------------------------------------------------------------------

-- | Shared helper to build a FinanceCtx for driver wallet recharges (topup/cash).
--   Populates merchant/supplier metadata so invoices and analytics have rich context.
-- TODO: Add supplier name and GSTIN
mkDriverWalletFinanceCtx ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DP.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Currency ->
  Text -> -- referenceId (order id or booth receipt id)
  m FinanceCtx
mkDriverWalletFinanceCtx driverId merchantId mocId currency referenceId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantOpCity <- CQMOC.findById mocId >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
  let mName = Just merchant.name
      mShortId = Just merchant.shortId.getShortId
      address =
        Just $
          show merchantOpCity.city
            <> ", "
            <> show merchantOpCity.state
            <> ", "
            <> show merchantOpCity.country
      supplierName = Nothing
      supplierGSTIN = Nothing
      supplierId = Nothing
  pure
    FinanceCtx
      { merchantId = merchantId.getId,
        merchantOpCityId = mocId.getId,
        currency = currency,
        counterpartyType = FAccount.DRIVER,
        counterpartyId = driverId.getId,
        referenceId = referenceId,
        merchantName = mName,
        merchantShortId = mShortId,
        issuedByAddress = address,
        supplierName = supplierName,
        supplierGSTIN = supplierGSTIN,
        supplierId = supplierId,
        panOfParty = Nothing,
        panType = Nothing,
        tdsRateReason = Nothing
      }

-- | Record airport booth cash recharge: credit driver wallet (PlatformAsset → OwnerLiability)
--   with referenceType = walletReferenceAirportCashRecharge. Idempotent by referenceId (e.g. booth receipt id).
recordAirportCashRecharge ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  (Id DP.Person, Id Domain.Types.Merchant.Merchant, Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) ->
  HighPrecMoney ->
  Text -> -- referenceId for idempotency (e.g. booth receipt id)
  m ()
recordAirportCashRecharge (driverId, merchantId, mocId) amount referenceId = do
  when (amount <= 0) $ throwError $ InvalidRequest "Cash recharge amount must be greater than zero"
  existing <- getEntriesByReference walletReferenceAirportCashRecharge referenceId
  when (null existing) $ do
    merchantOpCity <- CQMOC.findById mocId >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
    let currency = merchantOpCity.currency
    ctx <- mkDriverWalletFinanceCtx driverId merchantId mocId currency referenceId
    let cashRechargeInvoiceConfig =
          InvoiceConfig
            { invoiceType = FinanceInvoice.SubscriptionPurchase,
              issuedToType = "DRIVER",
              issuedToId = driverId.getId,
              issuedToName = Nothing,
              issuedToAddress = Nothing,
              lineItems = [InvoiceLineItem {description = "Airport Cash Recharge", quantity = 1, unitPrice = amount, lineTotal = amount, isExternalCharge = False}],
              gstBreakdown = Nothing
            }
    result <- runFinance ctx $ do
      _ <- transfer PlatformAsset OwnerLiability amount walletReferenceAirportCashRecharge
      invoice cashRechargeInvoiceConfig
    void $ fromEitherM (\e -> WalletLedgerEntryFailed ("airport cash recharge: " <> show e)) result
