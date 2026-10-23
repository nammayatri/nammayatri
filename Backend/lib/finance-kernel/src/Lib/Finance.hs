{-
  Lib.Finance - Finance Kernel Module Exports

  Domain-agnostic financial primitives for any payment system.

  The Three Laws:
  - LAW 1: Conservation of Money (Credits = Debits) - Ledger
  - LAW 2: Immutability of History (append-only) - Ledger + Audit
  - LAW 3: Deterministic State Transitions - StateMachine

  Usage:
  - Import Lib.Finance for all types and services
  - Call Service functions directly (createEntry, getBalance, etc.)
  - Interface modules only contain Input DTOs and helpers
-}
module Lib.Finance
  ( -- * Core
    module Lib.Finance.Core.Money,
    module Lib.Finance.Core.Types,

    -- * Account
    module Lib.Finance.Domain.Types.Account,
    module Lib.Finance.Account.Interface,
    module Lib.Finance.Account.Service,

    -- * Account-class config & balance
    module Lib.Finance.Domain.Types.FinanceAccountConfig,
    module Lib.Finance.AccountConfig,
    module Lib.Finance.Balance,

    -- * Transfers (the money-movement process primitive)
    module Lib.Finance.Domain.Types.Transfer,
    module Lib.Finance.Domain.Types.ScheduledTransferRule,
    module Lib.Finance.Transfer.Rail,
    module Lib.Finance.Transfer.Backfill,
    module Lib.Finance.Transfer.Internal,
    module Lib.Finance.Transfer.Outbound,
    module Lib.Finance.Transfer.Inbound,
    module Lib.Finance.Transfer.Outcome,
    module Lib.Finance.Transfer.Scheduled,
    module Lib.Finance.Transfer.History,

    -- * Ledger
    module Lib.Finance.Domain.Types.LedgerEntry,
    module Lib.Finance.Ledger.Interface,
    module Lib.Finance.Ledger.Service,

    -- * Invoice
    module Lib.Finance.Domain.Types.Invoice,
    module Lib.Finance.Invoice.Interface,
    module Lib.Finance.Invoice.Service,
    module Lib.Finance.Invoice.PdfService,

    -- * State Machine
    module Lib.Finance.Domain.Types.CurrentState,
    module Lib.Finance.StateMachine.Interface,
    module Lib.Finance.StateMachine.Service,

    -- * SAP Journal Entry
    module Lib.Finance.Domain.Types.SapJournalEntry,
    module Lib.Finance.SapJournalEntry.Interface,
    module Lib.Finance.SapJournalEntry.Service,

    -- * Audit
    module Lib.Finance.Domain.Types.AuditEntry,
    module Lib.Finance.Audit.Interface,
    module Lib.Finance.Audit.Service,

    -- * Tax
    module Lib.Finance.Tax.Types,
    module Lib.Finance.Tax.Interface,

    -- * Error
    module Lib.Finance.Error.Types,

    -- * FinanceM Monad
    module Lib.Finance.FinanceM,
  )
where

-- Input DTOs and helpers
import Lib.Finance.Account.Interface
-- Services (concrete implementations)
import Lib.Finance.Account.Service
import Lib.Finance.AccountConfig
import Lib.Finance.Audit.Interface
import Lib.Finance.Audit.Service
import Lib.Finance.Balance
-- Core modules
import Lib.Finance.Core.Money
import Lib.Finance.Core.Types
-- Domain types (generated from YAML)
import Lib.Finance.Domain.Types.Account
import Lib.Finance.Domain.Types.AuditEntry hiding (AuditEntityType (..)) -- avoid conflicts
import Lib.Finance.Domain.Types.CurrentState
import Lib.Finance.Domain.Types.FinanceAccountConfig
import Lib.Finance.Domain.Types.Invoice
import Lib.Finance.Domain.Types.LedgerEntry hiding (Adjustment, Expense, Revenue)
import Lib.Finance.Domain.Types.SapJournalEntry
import Lib.Finance.Domain.Types.ScheduledTransferRule
import Lib.Finance.Domain.Types.Transfer
import Lib.Finance.Error.Types
import Lib.Finance.FinanceM
import Lib.Finance.Invoice.Interface
import Lib.Finance.Invoice.PdfService
import Lib.Finance.Invoice.Service
import Lib.Finance.Ledger.Interface
import Lib.Finance.Ledger.Service
import Lib.Finance.SapJournalEntry.Interface
import Lib.Finance.SapJournalEntry.Service
import Lib.Finance.StateMachine.Interface
import Lib.Finance.StateMachine.Service
import Lib.Finance.Tax.Interface
import Lib.Finance.Tax.Types
import Lib.Finance.Transfer.Backfill
import Lib.Finance.Transfer.History
import Lib.Finance.Transfer.Inbound
import Lib.Finance.Transfer.Internal
import Lib.Finance.Transfer.Outbound
import Lib.Finance.Transfer.Outcome
import Lib.Finance.Transfer.Rail
import Lib.Finance.Transfer.Scheduled
