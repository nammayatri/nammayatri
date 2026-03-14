{-
  Finance.Error.Types

  Common error types. Domain provides recovery actions.
-}
module Lib.Finance.Error.Types
  ( FinanceError (..),
    LedgerErrorCode (..),
    AccountErrorCode (..),
    InvoiceErrorCode (..),
    StateErrorCode (..),
    TaxErrorCode (..),
    ReconErrorCode (..),
    RecoveryAction (..),
  )
where

import Kernel.Prelude

-- | Common finance error types
data FinanceError
  = LedgerError LedgerErrorCode Text
  | AccountError AccountErrorCode Text
  | InvoiceError InvoiceErrorCode Text
  | StateError StateErrorCode Text
  | TaxError TaxErrorCode Text
  | ReconError ReconErrorCode Text
  | DomainError Text Text -- Domain extends
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Ledger-specific errors
data LedgerErrorCode
  = ConservationViolation -- Credits ≠ Debits
  | InvalidReversal -- Reversal of non-existent entry
  | DuplicateEntry -- Idempotency violation
  | AccountMismatch -- Accounts in different currencies
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

-- | Account-specific errors
data AccountErrorCode
  = AccountNotFound
  | InsufficientBalance
  | AccountSuspended
  | AccountClosed
  | CurrencyMismatch
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

-- | Invoice-specific errors
data InvoiceErrorCode
  = InvalidLedgerEntries
  | AmountMismatch
  | InvoiceAlreadyIssued
  | InvoiceCancelled
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

-- | State machine errors
data StateErrorCode
  = InvalidTransition
  | EntityNotFound
  | StateAlreadySet
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

-- | Tax calculation errors
data TaxErrorCode
  = RateNotFound
  | CalculationError
  | JurisdictionNotSupported
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

-- | Reconciliation-specific errors
data ReconErrorCode
  = FareMismatch -- Reported amount differs from actual
  | OrderNotFound -- Order/ride not found for reconciliation
  | DuplicateRecon -- Recon entry already exists
  | InvalidDomain -- Unrecognized PPF domain
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

-- | Recovery actions (domain provides specific handlers)
data RecoveryAction
  = Retry
  | Escalate
  | DeadLetter
  | Manual
  | Custom Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
