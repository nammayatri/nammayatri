{-
  Finance.Account.Service

  Concrete account operations for domain use.
  Uses generated Beam queries internally.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Account.Service
  ( -- * Account operations
    createAccount,
    getAccount,
    getOrCreateAccount,
    getBalance,
    updateBalanceByDelta,
    withAccountWriteLock,
    releaseOwnedLock,
    withMasterReads,
    findAccountsByCounterparty,
    findAccountByCounterpartyAndType,

    -- * Input types (re-export from Interface)
    module Lib.Finance.Account.Interface,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.List (nub, sort)
import qualified Database.Redis as Hedis
import EulerHS.Extra.Monitoring.Types (UseMasterRedis (..))
import qualified EulerHS.Language as L
import Kernel.Beam.Types (MasterReadEnabled (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common ()
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Account.Interface
import Lib.Finance.Domain.Types.Account
import Lib.Finance.Error.Types
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Account as QAccount

-- | Create a new account
createAccount ::
  (BeamFlow.BeamFlow m r) =>
  AccountInput ->
  m (Either FinanceError Account)
createAccount input = do
  now <- getCurrentTime
  accountId <- generateGUID
  let account =
        Account
          { id = Id accountId,
            counterpartyType = input.counterpartyType,
            counterpartyId = input.counterpartyId,
            subLedger = input.subLedger,
            accountType = input.accountType,
            currency = input.currency,
            balance = 0, -- Accounts always start at 0
            status = Active,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            description = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QAccount.create account
  pure $ Right account

-- | Get account by ID
getAccount ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  m (Maybe Account)
getAccount = QAccount.findById

-- | Get or create account (idempotent operation)
-- Returns existing account if found, creates new one if not.
-- The common case (account exists) is a lock-free read; the Redis lock is
-- taken only on the one-time creation path to prevent duplicate accounts
-- under concurrent requests. Never lock on the read path: shared accounts
-- (e.g. merchant-level) would serialize every posting on one key.
getOrCreateAccount ::
  (BeamFlow.BeamFlow m r) =>
  AccountInput ->
  m (Either FinanceError Account)
getOrCreateAccount input = do
  mbExisting <- findExisting
  case mbExisting of
    Just existing -> pure $ Right existing
    Nothing ->
      Redis.withWaitAndLockRedis lockKey 10 10000 $ do
        -- re-check inside the lock: a concurrent caller may have created it
        mbExisting' <- findExisting
        case mbExisting' of
          Just existing -> pure $ Right existing
          Nothing -> createAccount input
  where
    findExisting =
      QAccount.findByMerchantCounterpartyAndTypeAndSubLedger
        input.merchantId
        input.counterpartyType
        input.counterpartyId
        input.accountType
        input.subLedger
    lockKey =
      "finance:getOrCreateAccount:"
        <> input.merchantId
        <> ":"
        <> show input.counterpartyType
        <> ":"
        <> fromMaybe "" input.counterpartyId
        <> ":"
        <> show input.accountType
        <> ":"
        <> show input.currency
        <> maybe "" (":" <>) input.subLedger

-- | Get current balance for an account
getBalance ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  m (Maybe HighPrecMoney)
getBalance accountId = do
  mbAccount <- QAccount.findById accountId
  pure $ mbAccount <&> (.balance)

-- | Update balance by a delta amount
-- Positive delta = credit, Negative delta = debit
updateBalanceByDelta ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  HighPrecMoney -> -- Delta (can be positive or negative)
  m (Either FinanceError HighPrecMoney) -- New balance
updateBalanceByDelta accountId delta = withAccountWriteLock [accountId] $ do
  mbAccount <- QAccount.findById accountId
  case mbAccount of
    Nothing -> pure $ Left $ AccountError AccountNotFound (show accountId)
    Just account -> do
      let newBalance = account.balance + delta
      QAccount.updateBalance newBalance accountId
      pure $ Right newBalance

-- | Serialises balance writes to these accounts. A balance is stored as an absolute value, so every
--   read-modify-write of one (a posting, a settle, a reversal) runs under this lock and reads the balance
--   inside it; two postings touching the same account can then never lose an update. That covers shared
--   accounts (merchant-level, which no owner lock serialises) and an owner lock that ran out mid-flow.
--   Held only for the reads and writes themselves (milliseconds), locks are taken in id order so two
--   writers on the same pair can't deadlock, and nothing else is locked inside it. The wait is bounded:
--   a posting that can't get the lock in 'accountWriteLockWaitSeconds' fails with an error rather than
--   queueing without limit (a retried job or webhook posts it again; a caller that isn't retried must
--   handle the error), and a slow acquire is logged so a hot shared account shows up.
withAccountWriteLock :: (CacheFlow m r, MonadFlow m) => [Id Account] -> m a -> m a
withAccountWriteLock accountIds action =
  foldr lockOne (withMasterReads action) (sort (nub ((.getId) <$> accountIds)))
  where
    lockOne accountId inner = do
      let resource = Redis.buildLockResourceName ("Finance:AccountWrite:" <> accountId)
      token <- generateGUIDText
      startedAt <- getCurrentTime
      -- Backs off from 2 ms to 20 ms while the account is busy, so waiters on a hot account don't flood it.
      let acquire pollMicros = do
            acquired <- Redis.setNxExpire resource accountWriteLockSeconds token
            unless acquired $ do
              now <- getCurrentTime
              when (diffUTCTime now startedAt >= fromIntegral accountWriteLockWaitSeconds) $
                throwError (InternalError ("Account " <> accountId <> " is busy; balance write not applied"))
              threadDelay pollMicros
              acquire (min 20000 (pollMicros * 2))
      acquire 2000
      lockedAt <- getCurrentTime
      when (diffUTCTime lockedAt startedAt >= 1) $
        logWarning $ "Account write lock for " <> accountId <> " took " <> show (diffUTCTime lockedAt startedAt) <> " to acquire"
      -- Released only by its holder (an atomic check-and-delete), so a holder that ran past the lease never
      -- deletes the next holder's lock. A section held long enough to near the lease is logged.
      inner `finally` do
        heldFor <- (`diffUTCTime` lockedAt) <$> getCurrentTime
        when (heldFor >= 3) $
          logWarning $ "Account write lock for " <> accountId <> " held " <> show heldFor <> " (lease " <> show accountWriteLockSeconds <> " s)"
        release resource token
    -- A failed release is logged, never thrown: the posting under the lock may already have committed (or
    -- failed with its own error), and the lease frees the lock anyway.
    release resource token =
      try @_ @SomeException (releaseOwnedLock resource token) >>= \case
        Left err -> logError $ "Account write lock " <> resource <> " not released (expires with its lease): " <> show err
        Right True -> pure ()
        Right False -> logWarning $ "Account write lock " <> resource <> " ran past its lease before release"

-- | Deletes a lock only while it still holds this holder's token, in one Redis step: a GET then DEL could
--   delete the next holder's lock if the lease ran out in between. True when it was deleted.
releaseOwnedLock :: (CacheFlow m r, MonadFlow m) => Text -> Text -> m Bool
releaseOwnedLock resource token =
  (== (1 :: Integer))
    <$> Redis.withMasterRedis (Redis.runWithPrefix resource $ \key -> Hedis.eval releaseScript [key] [BSL.toStrict (A.encode token)])
  where
    -- The token is stored JSON-encoded, as 'Redis.setNxExpire' writes it.
    releaseScript = "if redis.call('GET', KEYS[1]) == ARGV[1] then return redis.call('DEL', KEYS[1]) else return 0 end"

-- | Runs the action with DB and KV reads on the master, restoring the previous setting afterwards (unset
--   stays unset, so per-table read settings apply again): a read under a lock must see the previous
--   holder's write, which a replica may not have yet.
withMasterReads :: (MonadFlow m, MonadMask m) => m a -> m a
withMasterReads action = do
  previousDb <- L.getOptionLocal MasterReadEnabled
  previousRedis <- L.getOptionLocal UseMasterRedis
  L.setOptionLocal MasterReadEnabled True
  L.setOptionLocal UseMasterRedis True
  action `finally` do
    maybe (L.delOptionLocal MasterReadEnabled) (L.setOptionLocal MasterReadEnabled) previousDb
    maybe (L.delOptionLocal UseMasterRedis) (L.setOptionLocal UseMasterRedis) previousRedis

accountWriteLockSeconds :: Int
accountWriteLockSeconds = 10

accountWriteLockWaitSeconds :: Int
accountWriteLockWaitSeconds = 5

-- | Find the pooled (no sub-ledger) account for a counterparty by type.
findAccountsByCounterparty ::
  (BeamFlow.BeamFlow m r) =>
  Maybe CounterpartyType -> -- Counterparty type (e.g., DRIVER, FLEET_OWNER)
  Maybe Text -> -- Counterparty ID
  AccountType ->
  m (Maybe Account)
findAccountsByCounterparty counterpartyType counterpartyId accountType =
  findAccountByCounterpartyAndType counterpartyType counterpartyId accountType Nothing

-- | Find specific account by counterparty, type, and optional sub-ledger.
findAccountByCounterpartyAndType ::
  (BeamFlow.BeamFlow m r) =>
  Maybe CounterpartyType -> -- Counterparty type
  Maybe Text -> -- Counterparty ID
  AccountType ->
  Maybe Text -> -- Sub-ledger
  m (Maybe Account)
findAccountByCounterpartyAndType = QAccount.findByCounterpartyAndTypeAndSubLedger
