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
            accountType = input.accountType,
            accountCategory = input.accountCategory,
            currency = input.currency,
            balance = 0, -- Accounts always start at 0
            status = Active,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
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
-- Returns existing account if found, creates new one if not
getOrCreateAccount ::
  (BeamFlow.BeamFlow m r) =>
  AccountInput ->
  m (Either FinanceError Account)
getOrCreateAccount input = do
  mbExisting <-
    QAccount.findByCounterpartyAndType
      input.counterpartyType
      input.counterpartyId
      input.accountType
      input.currency

  case mbExisting of
    Just existing -> pure $ Right existing
    Nothing -> createAccount input

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

-- | Find all accounts for a counterparty
findAccountsByCounterparty ::
  (BeamFlow.BeamFlow m r) =>
  Maybe CounterpartyType -> -- Counterparty type (e.g., DRIVER, FLEET_OWNER)
  Maybe Text -> -- Counterparty ID
  m [Account]
findAccountsByCounterparty = QAccount.findByCounterparty

-- | Find specific account by counterparty and type
findAccountByCounterpartyAndType ::
  (BeamFlow.BeamFlow m r) =>
  Maybe CounterpartyType -> -- Counterparty type
  Maybe Text -> -- Counterparty ID
  AccountType ->
  Currency ->
  m (Maybe Account)
findAccountByCounterpartyAndType = QAccount.findByCounterpartyAndType
