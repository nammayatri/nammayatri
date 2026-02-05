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

    -- * Ledger
    module Lib.Finance.Domain.Types.LedgerEntry,
    module Lib.Finance.Ledger.Interface,
    module Lib.Finance.Ledger.Service,

    -- * Invoice
    module Lib.Finance.Domain.Types.Invoice,
    module Lib.Finance.Invoice.Interface,
    module Lib.Finance.Invoice.Service,

    -- * State Machine
    module Lib.Finance.Domain.Types.CurrentState,
    module Lib.Finance.StateMachine.Interface,
    module Lib.Finance.StateMachine.Service,

    -- * Audit
    module Lib.Finance.Domain.Types.AuditEntry,
    module Lib.Finance.Audit.Interface,
    module Lib.Finance.Audit.Service,

    -- * Tax
    module Lib.Finance.Tax.Types,
    module Lib.Finance.Tax.Interface,

    -- * Error
    module Lib.Finance.Error.Types,
  )
where

-- Input DTOs and helpers
import Lib.Finance.Account.Interface
-- Services (concrete implementations)
import Lib.Finance.Account.Service
import Lib.Finance.Audit.Interface
import Lib.Finance.Audit.Service
-- Core modules
import Lib.Finance.Core.Money
import Lib.Finance.Core.Types
-- Domain types (generated from YAML)
import Lib.Finance.Domain.Types.Account
import Lib.Finance.Domain.Types.AuditEntry
import Lib.Finance.Domain.Types.CurrentState
import Lib.Finance.Domain.Types.Invoice
import Lib.Finance.Domain.Types.LedgerEntry
import Lib.Finance.Error.Types
import Lib.Finance.Invoice.Interface
import Lib.Finance.Invoice.Service
import Lib.Finance.Ledger.Interface
import Lib.Finance.Ledger.Service
import Lib.Finance.StateMachine.Interface
import Lib.Finance.StateMachine.Service
import Lib.Finance.Tax.Interface
import Lib.Finance.Tax.Types
