{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Payment where

import qualified Data.Text as T
-- Finance-kernel beam imports

import qualified Lib.Finance.Storage.Beam.Account as BeamAccount
import qualified Lib.Finance.Storage.Beam.AuditEntry as BeamAudit
import qualified Lib.Finance.Storage.Beam.CurrentState as BeamCurrentState
import qualified Lib.Finance.Storage.Beam.DirectTaxTransaction as BeamDirectTax
import qualified Lib.Finance.Storage.Beam.IndirectTaxTransaction as BeamIndirectTax
import qualified Lib.Finance.Storage.Beam.Invoice as BeamInvoice
import qualified Lib.Finance.Storage.Beam.InvoiceLedgerLink as BeamInvoiceLedger
import qualified Lib.Finance.Storage.Beam.LedgerEntry as BeamLedger
import qualified Lib.Finance.Storage.Beam.PgPaymentSettlementReport as BeamPgPayment
import qualified Lib.Finance.Storage.Beam.PgPayoutSettlementReport as BeamPgPayout
import qualified Lib.Finance.Storage.Beam.ReconciliationEntry as BeamReconciliationEntry
import qualified Lib.Finance.Storage.Beam.ReconciliationSummary as BeamReconciliationSummary
import qualified Lib.Finance.Storage.Beam.StateTransition as BeamStateTransition
import qualified Lib.Payment.Storage.Beam.Offer as BeamOF
import qualified Lib.Payment.Storage.Beam.PaymentOrder as BeamPO
import qualified Lib.Payment.Storage.Beam.PaymentOrderOffer as BeamOffer
import qualified Lib.Payment.Storage.Beam.PaymentOrderSplit as BeamSplit
import qualified Lib.Payment.Storage.Beam.PaymentTransaction as BeamPT
import qualified Lib.Payment.Storage.Beam.PayoutOrder as BeamP
import qualified Lib.Payment.Storage.Beam.PayoutRequest as BeamPR
import qualified Lib.Payment.Storage.Beam.PayoutTransaction as BeamT
import qualified Lib.Payment.Storage.Beam.PersonDailyOfferStats as BeamPDOS
import qualified Lib.Payment.Storage.Beam.PersonOfferStats as BeamPOS
import qualified Lib.Payment.Storage.Beam.PersonWallet as BeamPW
import qualified Lib.Payment.Storage.Beam.Refunds as BeamRF
import qualified Lib.Payment.Storage.Beam.WalletRewardPosting as BeamWRP
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName BeamPO.PaymentOrderT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPT.PaymentTransactionT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamRF.RefundsT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamP.PayoutOrderT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPR.PayoutRequestT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamT.PayoutTransactionT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamSplit.PaymentOrderSplitT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamOffer.PaymentOrderOfferT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamWRP.WalletRewardPostingT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPW.PersonWalletT where
  schemaName _ = T.pack currentSchemaName

-- Finance-kernel beam orphan instances
instance HasSchemaName BeamAccount.AccountT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamAudit.AuditEntryT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamCurrentState.CurrentStateT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamDirectTax.DirectTaxTransactionT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamIndirectTax.IndirectTaxTransactionT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamInvoice.InvoiceT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamInvoiceLedger.InvoiceLedgerLinkT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamLedger.LedgerEntryT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPgPayment.PgPaymentSettlementReportT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPgPayout.PgPayoutSettlementReportT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamReconciliationEntry.ReconciliationEntryT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamReconciliationSummary.ReconciliationSummaryT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamStateTransition.StateTransitionT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamOF.OfferT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPOS.PersonOfferStatsT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPDOS.PersonDailyOfferStatsT where
  schemaName _ = T.pack currentSchemaName
