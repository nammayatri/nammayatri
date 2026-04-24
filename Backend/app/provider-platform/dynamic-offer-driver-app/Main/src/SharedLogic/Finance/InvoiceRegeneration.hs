-- | Helpers for voiding a prior ride invoice and re-issuing a new invoice
--   that references existing ledger entries plus optional new entries (e.g. tip).
--
--   Design: ledger entries are immutable historical facts. When a tip is added
--   post-ride, the original BaseRide/Tax/Toll/etc. entries remain valid — they
--   represent what really happened at ride end. Only the invoice document needs
--   replacing because its customer-facing total is now different.
--
--   This module:
--   * Voids the prior invoice document
--   * Returns the existing entries so the caller can re-link them to a new invoice
--     (along with the new Tips entry) via finance-kernel's createInvoice.
module SharedLogic.Finance.InvoiceRegeneration
  ( voidPriorRideInvoice,
    PriorInvoiceContext (..),
  )
where

import Kernel.Prelude
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Invoice.Service as InvoiceSvc
import qualified Lib.Finance.Ledger.Service as LedgerSvc
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified SharedLogic.Finance.Wallet as Wallet

data PriorInvoiceContext = PriorInvoiceContext
  { priorInvoice :: FInvoice.Invoice,
    priorEntries :: [LE.LedgerEntry]
  }

-- | Void the prior Ride invoice for a booking (looked up via the BaseRide ledger entry's invoice link).
--   Marks ONLY the invoice document as Voided; the underlying ledger entries are NOT reversed.
--   Returns the voided invoice and its linked entries so the caller can build a new invoice
--   that re-links the same entries (plus any new ones).
voidPriorRideInvoice ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- referenceId (booking.id.getId)
  m (Maybe PriorInvoiceContext)
voidPriorRideInvoice refId = do
  -- Ride base portion is posted as a single BaseRide entry that links to
  -- the invoice.
  mbEntry <- listToMaybe <$> LedgerSvc.getEntriesByReference Wallet.walletReferenceBaseRide refId
  case mbEntry of
    Nothing -> do
      logInfo $ "No prior ride base entry for booking " <> refId <> "; nothing to void"
      pure Nothing
    Just entry -> do
      mbInv <- InvoiceSvc.getInvoiceForEntry entry.id
      case mbInv of
        Nothing -> do
          logInfo $ "Ride base entry " <> entry.id.getId <> " has no linked invoice"
          pure Nothing
        Just inv -> do
          allEntries <- InvoiceSvc.getEntriesForInvoice inv.id
          InvoiceSvc.updateInvoiceStatus inv.id FInvoice.Voided
          logInfo $ "Voided invoice " <> inv.id.getId <> " for booking " <> refId
          pure $ Just PriorInvoiceContext {priorInvoice = inv, priorEntries = allEntries}
