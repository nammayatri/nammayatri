{-
  Lib.Finance.Balance

  The single source of truth for "how much money does this account really have".
  Composes the ledger balance with every claim against it — PENDING ledger holds,
  Redis offer holds, in-flight payout liability — and the account-class config
  (redeemable cutoff, minimum retained balance) into one BalanceBreakdown, so no
  domain ever re-derives spendable/redeemable amounts by hand.

  Invariant this module relies on: a PENDING ledger entry whose FROM side is a
  balance-bearing account is a hold on that account — EXCEPT pending tax accruals
  (counterpart = a GOVERNMENT account), which deduct from a pending inflow by
  construction, never from the balance (e.g. TDS on a not-yet-collected cancellation
  fee). sumPendingByFromAccount implements the exclusion.

  Design: docs/backend/design/finance-kernel-unification-plan.md §4.
-}
module Lib.Finance.Balance
  ( BalanceBreakdown (..),
    TransferAmount (..),
    ResolvedTransferAmount (..),
    getBalanceBreakdown,
    getBalanceBreakdownWith,
    getBalanceAmountsWith,
    getBalanceBreakdownById,
    resolveTransferAmount,
    withAccountBalanceLock,
    accountBalanceLockKey,
    offerHoldKeyForAccount,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.AccountConfig (ResolvedAccountConfig (..), cutoffTimeFor, getAccountConfigForAccount)
import Lib.Finance.Domain.Types.Account (Account (..), AccountType (PayoutLiability))
import Lib.Finance.Domain.Types.FinanceAccountConfig (FeeBearer (..), FeeSpec)
import Lib.Finance.Domain.Types.LedgerEntry (LedgerEntry)
import Lib.Finance.Error.Types
import Lib.Finance.Ledger.Service (getPayoutEligibilityAmountsWith, getPayoutEligibilityDataWith)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra
import Lib.Finance.TempBalanceHold (getOfferHoldTotalAtKey)

data BalanceBreakdown = BalanceBreakdown
  { -- | account.balance, verbatim
    ledgerBalance :: HighPrecMoney,
    -- | sum of PENDING ledger entries debiting this account (statutory / deposit / prepaid holds)
    pendingHolds :: HighPrecMoney,
    -- | sum of live Redis offer holds (standard key + any legacy keys passed by the caller)
    offerHolds :: HighPrecMoney,
    -- | balance parked on the sibling PayoutLiability account (transfers in flight)
    processingOut :: HighPrecMoney,
    -- | post-cutoff unsettled net — accrued too recently to redeem (0 when no cutoff configured)
    nonRedeemable :: HighPrecMoney,
    -- | ledgerBalance − nonRedeemable, floored at 0
    redeemable :: HighPrecMoney,
    -- | redeemable − pendingHolds − offerHolds − minRetainedBalance, floored at 0
    available :: HighPrecMoney,
    -- | the unsettled entries a payout of 'redeemable' would cover
    redeemableEntries :: [LedgerEntry],
    -- | the cutoff instant this breakdown was computed under: the configured
    --   redeemable cutoff, or Nothing when the class has none (everything before
    --   "now" counted as redeemable). Domains needing a cutoff for display or
    --   bucketing take it from HERE — never recompute one from config.
    cutoffAt :: Maybe UTCTime,
    -- | the class config this breakdown was computed under
    config :: ResolvedAccountConfig
  }
  deriving (Generic)

-- | How much to move. AllAvailable is resolved by the kernel under the account's
--   balance lock — never pre-computed by a domain.
data TransferAmount = AllAvailable | Exact HighPrecMoney
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Outcome of resolving a TransferAmount against a breakdown + config.
data ResolvedTransferAmount = ResolvedTransferAmount
  { -- | what leaves the account
    grossDebit :: HighPrecMoney,
    -- | what the counterparty receives (gross − fee when the owner bears it)
    netToCounterparty :: HighPrecMoney,
    fee :: HighPrecMoney,
    feeBearer :: Maybe FeeBearer
  }
  deriving (Eq, Show, Generic)

-- | Compute the full breakdown for an account. 'legacyOfferHoldKeys' lets callers
--   whose offer holds still live at domain-named Redis keys (e.g.
--   "WalletOfferHolds:<driverId>") include them until the write path migrates to
--   'offerHoldKeyForAccount'.
getBalanceBreakdown ::
  (BeamFlow.BeamFlow m r) =>
  Account ->
  [Text] ->
  m BalanceBreakdown
getBalanceBreakdown = getBalanceBreakdownWith []

-- | 'getBalanceBreakdown' where entries with one of 'alwaysRedeemableRefs' are never held
--   back by the redeemable cutoff: posted after it, they are redeemable straight away
--   (see 'getPayoutEligibilityDataWith'). The driver wallet passes its TDS-reimbursement
--   reference type here.
getBalanceBreakdownWith ::
  (BeamFlow.BeamFlow m r) =>
  [Text] ->
  Account ->
  [Text] ->
  m BalanceBreakdown
getBalanceBreakdownWith = breakdownWith True

-- | 'getBalanceBreakdownWith' for callers that read only the numbers (available,
--   redeemable, holds, ...): 'redeemableEntries' is always empty, and the unbounded
--   read of the wallet's unsettled history is skipped. Every figure equals the one
--   'getBalanceBreakdownWith' returns.
getBalanceAmountsWith ::
  (BeamFlow.BeamFlow m r) =>
  [Text] ->
  Account ->
  [Text] ->
  m BalanceBreakdown
getBalanceAmountsWith = breakdownWith False

breakdownWith ::
  (BeamFlow.BeamFlow m r) =>
  Bool -> -- also load the entries a payout of 'redeemable' would cover
  [Text] ->
  Account ->
  [Text] ->
  m BalanceBreakdown
breakdownWith withEntries alwaysRedeemableRefs account legacyOfferHoldKeys = do
  cfg <- getAccountConfigForAccount account
  pendingHolds <- QLedgerExtra.sumPendingByFromAccount account.id
  offerHolds <- sum <$> mapM getOfferHoldTotalAtKey (offerHoldKeyForAccount account.id : legacyOfferHoldKeys)
  processingOut <- getProcessingOut account
  now <- getCurrentTime
  -- No configured cutoff means "everything accrued so far is redeemable": eligibility
  -- runs against 'now', so nonRedeemable is 0 by construction and redeemableEntries is
  -- still the full unsettled set (callers like cashback need the entry list even when
  -- the class has no cutoff policy).
  let cutoffAt = (`cutoffTimeFor` now) <$> cfg.redeemableCutoff
  let eligibilityOf = if withEntries then getPayoutEligibilityDataWith else getPayoutEligibilityAmountsWith
  elig <- eligibilityOf alwaysRedeemableRefs account.id account.balance (fromMaybe now cutoffAt) now
  let available = max 0 (elig.redeemableBalance - pendingHolds - offerHolds - cfg.minRetainedBalance)
  pure
    BalanceBreakdown
      { ledgerBalance = account.balance,
        pendingHolds,
        offerHolds,
        processingOut,
        nonRedeemable = elig.nonRedeemableBalance,
        redeemable = elig.redeemableBalance,
        available,
        redeemableEntries = elig.redeemableEntries,
        cutoffAt,
        config = cfg
      }

getBalanceBreakdownById ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  [Text] ->
  m (Either FinanceError BalanceBreakdown)
getBalanceBreakdownById accountId legacyOfferHoldKeys =
  QAccount.findById accountId >>= \case
    Nothing -> pure $ Left $ AccountError AccountNotFound (show accountId)
    Just account -> Right <$> getBalanceBreakdown account legacyOfferHoldKeys

-- | Balance parked on the same counterparty's PayoutLiability account (in-flight
--   outbound transfers). 0 for accounts without a counterparty.
getProcessingOut :: (BeamFlow.BeamFlow m r) => Account -> m HighPrecMoney
getProcessingOut account
  | account.accountType == PayoutLiability = pure 0
  | isNothing account.counterpartyType || isNothing account.counterpartyId = pure 0
  | otherwise = do
    mbPayoutAcc <-
      QAccount.findByMerchantCounterpartyAndTypeAndSubLedger
        account.merchantId
        account.counterpartyType
        account.counterpartyId
        PayoutLiability
        account.subLedger
    pure $ maybe 0 (max 0 . (.balance)) mbPayoutAcc

-- | Resolve a requested amount against the breakdown. Pure so it can only be called
--   with a breakdown read under the balance lock.
--
--   Fee policy applies ONLY when the kernel resolves the amount itself (AllAvailable).
--   'Exact' means the caller fixed the gross debit — typically having already deducted
--   its own fee — so re-applying the class fee here would double-charge the gates
--   (falsely tripping TRANSFER_BELOW_MINIMUM near the threshold).
resolveTransferAmount ::
  ResolvedAccountConfig ->
  BalanceBreakdown ->
  TransferAmount ->
  Either FinanceError ResolvedTransferAmount
resolveTransferAmount cfg breakdown requested = do
  (gross, feeApplies) <- case requested of
    AllAvailable -> Right (breakdown.available, True)
    Exact amt
      | amt <= 0 -> Left $ TransferError INVALID_TRANSFER_AMOUNT ("non-positive amount: " <> show amt)
      | amt > breakdown.available -> Left $ AccountError InsufficientBalance ("requested " <> show amt <> ", available " <> show breakdown.available)
      | otherwise -> Right (amt, False)
  let (feeAmt, mbBearer) = if feeApplies then computeFee cfg.transferOutFee gross else (0, Nothing)
      net = case mbBearer of
        Just OWNER_BEARER -> floorToTwoDecimals (gross - feeAmt)
        _ -> floorToTwoDecimals gross
  when (net <= 0) $
    Left $ TransferError TRANSFER_CONSUMED_BY_FEE ("gross " <> show gross <> ", fee " <> show feeAmt)
  -- The class minimum, like the fee, applies only to kernel-RESOLVED amounts: an
  -- Exact caller already enforced its own floor against the GROSS (the historical
  -- check), and re-checking the fee-netted value here created a silent dead band
  -- of [minimum, minimum + fee) where nothing was ever paid.
  when (feeApplies && net < cfg.minTransferOutAmount) $
    Left $ TransferError TRANSFER_BELOW_MINIMUM ("net " <> show net <> ", minimum " <> show cfg.minTransferOutAmount)
  pure
    ResolvedTransferAmount
      { grossDebit = gross,
        netToCounterparty = net,
        fee = feeAmt,
        feeBearer = mbBearer
      }

computeFee :: Maybe FeeSpec -> HighPrecMoney -> (HighPrecMoney, Maybe FeeBearer)
computeFee Nothing _ = (0, Nothing)
computeFee (Just spec) gross =
  let fixed = fromMaybe 0 spec.fixedFee
      pct = maybe 0 (\rate -> gross * realToFrac (rate / 100)) spec.percentageRate
   in -- capped at gross, as the pre-kernel fee computation always was
      (min gross (floorToTwoDecimals (fixed + pct)), Just spec.bearer)

floorToTwoDecimals :: HighPrecMoney -> HighPrecMoney
floorToTwoDecimals x = fromIntegral (floor (x * 100) :: Integer) / 100

-- | Kernel-owned balance lock, keyed by account (replaces domain-named running-balance
--   locks as flows migrate onto the kernel verbs).
accountBalanceLockKey :: Id Account -> Text
accountBalanceLockKey accountId = "Finance:BalanceLock:" <> accountId.getId

-- | Runs the action under the account's balance lock and returns its result. Waiters
--   retry every 10ms until the lock is released or its TTL expires (a crashed holder
--   cannot wedge the account for longer than 'ttlSeconds').
withAccountBalanceLock :: (CacheFlow m r, MonadFlow m) => Id Account -> Int -> m a -> m a
withAccountBalanceLock accountId ttlSeconds =
  Redis.withWaitAndLockRedis (accountBalanceLockKey accountId) ttlSeconds 10000

-- | Standard offer-hold key for an account. New write paths must use this; legacy
--   domain-named keys are accepted read-side via getBalanceBreakdown's second argument.
offerHoldKeyForAccount :: Id Account -> Text
offerHoldKeyForAccount accountId = "Finance:OfferHolds:" <> accountId.getId
