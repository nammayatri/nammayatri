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
    TransferErrorCode (..),
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
  | TransferError TransferErrorCode Text
  | DomainError Text Text -- Domain extends
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Every rejection the transfer verbs can hand back, as a closed set — callers match
--   on constructors, not string codes. SCREAMING_SNAKE so 'show' reproduces the
--   pre-enum string codes verbatim (logs, lastError, dashboards stay stable).
data TransferErrorCode
  = TRANSFER_OUT_DISABLED
  | TRANSFER_IN_DISABLED
  | INSTANT_TRANSFER_NOT_ALLOWED
  | TRANSFER_DAILY_LIMIT_REACHED
  | TRANSFER_BELOW_MINIMUM
  | TRANSFER_CONSUMED_BY_FEE
  | INVALID_TRANSFER_AMOUNT
  | TRANSFER_NOT_FOUND
  | CTX_REFERENCE_MISMATCH
  | INBOUND_RAIL_WITHOUT_COLLECT
  | NOT_AN_INBOUND_TRANSFER
  | INBOUND_REQUIRES_RECIPE
  | INVALID_CREDIT_RECIPE
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

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

-- | Recovery actions (domain provides specific handlers)
data RecoveryAction
  = Retry
  | Escalate
  | DeadLetter
  | Manual
  | Custom Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
