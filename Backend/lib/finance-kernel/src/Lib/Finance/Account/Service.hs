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
    findAccountsByCounterparty,
    findAccountByCounterpartyAndType,

    -- * Input types (re-export from Interface)
    module Lib.Finance.Account.Interface,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common ()
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
    -- Scoped by merchant: counterpartyId holds the merchant itself for most roles, but
    -- for BUYER it holds the counterparty, which two merchants can share.
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
updateBalanceByDelta accountId delta = do
  mbAccount <- QAccount.findById accountId
  case mbAccount of
    Nothing -> pure $ Left $ AccountError AccountNotFound (show accountId)
    Just account -> do
      let newBalance = account.balance + delta
      -- Check for negative balance on liability accounts
      when (account.accountType == Liability && newBalance < 0) $
        pure () -- Could enforce business rules here
      QAccount.updateBalance newBalance accountId
      pure $ Right newBalance

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
