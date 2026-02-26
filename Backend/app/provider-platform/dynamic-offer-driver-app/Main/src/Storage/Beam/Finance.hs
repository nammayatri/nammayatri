{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Finance where

import qualified Data.Text as T
import qualified Lib.Finance.Storage.Beam.Account as BeamAccount
import qualified Lib.Finance.Storage.Beam.AuditEntry as BeamAudit
import qualified Lib.Finance.Storage.Beam.CurrentState as BeamCurrentState
import qualified Lib.Finance.Storage.Beam.IndirectTaxTransaction as BeamIndirectTax
import qualified Lib.Finance.Storage.Beam.DirectTaxTransaction as BeamDirectTax
import qualified Lib.Finance.Storage.Beam.Invoice as BeamInvoice
import qualified Lib.Finance.Storage.Beam.InvoiceLedgerLink as BeamInvoiceLedger
import qualified Lib.Finance.Storage.Beam.LedgerEntry as BeamLedger
import qualified Lib.Finance.Storage.Beam.StateTransition as BeamStateTransition
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName BeamAccount.AccountT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamAudit.AuditEntryT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamCurrentState.CurrentStateT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamIndirectTax.IndirectTaxTransactionT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamDirectTax.DirectTaxTransactionT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamInvoice.InvoiceT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamInvoiceLedger.InvoiceLedgerLinkT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamLedger.LedgerEntryT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamStateTransition.StateTransitionT where
  schemaName _ = T.pack currentSchemaName
