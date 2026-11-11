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
    withOwnerLock,
    withOwnerLockOrSkip,
    ownerLockKey,
    offerHoldKeyForAccount,
  )
where

import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Account.Service (releaseOwnedLock, withMasterReads)
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

-- | Kernel-owned lock over one owner's postings and the balance checks made before them (a
--   driver, a fleet owner, ...), keyed by the owner's id. Every flow that checks a balance and then
--   posts, or posts several legs that belong together, takes it. Balance writes themselves are also
--   serialised per account ('Lib.Finance.Account.Service.withAccountWriteLock'). Not
--   'accountBalanceLockKey': the transfer verbs hold that lock while they call the caller's runner,
--   and neither lock is re-entrant.
--
--   The key keeps the driver app's previous name, so during a rolling deploy old pods (which lock
--   "WalletRunningBalanceLockKey:<id>") and new pods exclude each other. Renaming it later needs every
--   pod on the old name drained first.
--   TODO: once no pod runs a release older than this one, rename to "Finance:OwnerLock:<id>" in a release
--   of its own (no other lock change in it), so the old and new names are never in use at the same time.
ownerLockKey :: Text -> Text
ownerLockKey ownerId = "WalletRunningBalanceLockKey:" <> ownerId

-- | Runs the action under the owner's lock, waiting as long as it is held. The lease is renewed while
--   the action runs (up to 'ownerLockMaxHoldSeconds'), so a slow holder doesn't lose the lock half way,
--   and only the holder releases it. A crashed holder can't wedge the owner for longer than one lease.
withOwnerLock :: (CacheFlow m r, MonadFlow m) => Text -> m a -> m a
withOwnerLock ownerId action = do
  token <- generateGUIDText
  let resource = ownerLockResource ownerId
      acquire = do
        acquired <- Redis.setNxExpire resource ownerLockSeconds token
        unless acquired $ threadDelay ownerLockPollMicros >> acquire
  acquire
  holdOwnerLock resource token action

-- | Like 'withOwnerLock', but gives up after 'ownerLockSeconds' of waiting and skips the action (logged).
--   Only for work that something retries anyway.
withOwnerLockOrSkip :: (CacheFlow m r, MonadFlow m) => Text -> m () -> m ()
withOwnerLockOrSkip ownerId action = do
  token <- generateGUIDText
  startedAt <- getCurrentTime
  let resource = ownerLockResource ownerId
      acquire = do
        acquired <- Redis.setNxExpire resource ownerLockSeconds token
        if acquired
          then pure True
          else do
            now <- getCurrentTime
            if diffUTCTime now startedAt >= fromIntegral ownerLockSeconds
              then pure False
              else threadDelay 50000 >> acquire
  acquired <- acquire
  if acquired
    then holdOwnerLock resource token action
    else logWarning $ "Owner lock for " <> ownerId <> " still busy after " <> show ownerLockSeconds <> " s; skipped"

-- | Runs the action holding the lock: renews the lease every few seconds while it runs, then releases the
--   lock if this holder still owns it (an atomic token check-and-delete, so a holder whose lease ran out
--   never deletes the next holder's lock). The renewal reads the holder from the master, never a replica
--   that may not have the SETNX yet, and without the decode-failure delete of 'Redis.get', since an old
--   pod's lock holds a different value.
holdOwnerLock :: (CacheFlow m r, MonadFlow m) => Text -> Text -> m a -> m a
holdOwnerLock resource token action = do
  done <- liftIO (newIORef False)
  -- DB and KV reads under the lock go to the master: a "posted already?" check must see the previous
  -- holder's postings, which a replica may not have yet. (A write resets that setting, so the kernel's
  -- by-reference reads also ask for the master themselves; plain Redis reads need 'Redis.withMasterRedis'
  -- at the read site.) The renewer is started inside the protected region, so
  -- an exception can't leave it renewing for a holder that is gone; a failed release is logged rather
  -- than replacing the action's own result.
  (fork ("OwnerLockRenew:" <> resource) (renew done ownerLockMaxHoldSeconds) >> withMasterReads action)
    `finally` (liftIO (atomicWriteIORef done True) >> releaseLogged)
  where
    currentHolder = Redis.withMasterRedis (Redis.get' resource (pure ()))
    renew done secondsLeft = do
      threadDelay (ownerLockRenewSeconds * 1000000)
      finished <- liftIO (readIORef done)
      unless finished $
        if secondsLeft <= 0
          then logWarning $ "Owner lock " <> resource <> " held past " <> show ownerLockMaxHoldSeconds <> " s; no longer renewed"
          else -- A Redis error on one tick is logged and the next tick tries again, so a long holder keeps its lease.

            try @_ @SomeException renewStep >>= \case
              Right True -> continue
              Right False -> pure ()
              Left err -> logWarning ("Owner lock " <> resource <> " renewal step failed: " <> show err) >> continue
      where
        continue = renew done (secondsLeft - ownerLockRenewSeconds)
        -- True to keep renewing.
        renewStep =
          currentHolder >>= \case
            Just holder
              | holder == token -> Redis.expire resource ownerLockSeconds >> pure True
              | otherwise -> logWarning ("Owner lock " <> resource <> " was taken over; no longer renewed") >> pure False
            -- Gone (lease ran out) or unreadable: take it back if it is free, and keep renewing. If the
            -- action finished meanwhile (its release ran first), the lock just taken back is dropped again.
            Nothing -> do
              retaken <- Redis.setNxExpire resource ownerLockSeconds token
              finishedNow <- liftIO (readIORef done)
              if retaken && finishedNow then release >> pure False else pure True
    release = void (releaseOwnedLock resource token)
    releaseLogged =
      try @_ @SomeException release >>= \case
        Left err -> logError $ "Owner lock " <> resource <> " not released (expires with its lease): " <> show err
        Right () -> pure ()

-- | The Redis key, named as 'Redis.withWaitAndLockRedis' names its locks, so this lock and the previous
--   release's lock on the same key exclude each other.
ownerLockResource :: Text -> Text
ownerLockResource ownerId = Redis.buildLockResourceName (ownerLockKey ownerId)

ownerLockSeconds :: Int
ownerLockSeconds = 10

ownerLockRenewSeconds :: Int
ownerLockRenewSeconds = 3

ownerLockMaxHoldSeconds :: Int
ownerLockMaxHoldSeconds = 120

-- | Retry interval while another holder has the lock: postings hold it for milliseconds to seconds, so a
--   10 ms poll costs no noticeable latency and keeps contended Redis traffic low.
ownerLockPollMicros :: Int
ownerLockPollMicros = 10000

-- | Standard offer-hold key for an account. New write paths must use this; legacy
--   domain-named keys are accepted read-side via getBalanceBreakdown's second argument.
offerHoldKeyForAccount :: Id Account -> Text
offerHoldKeyForAccount accountId = "Finance:OfferHolds:" <> accountId.getId
