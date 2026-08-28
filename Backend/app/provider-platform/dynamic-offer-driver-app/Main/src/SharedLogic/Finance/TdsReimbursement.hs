module SharedLogic.Finance.TdsReimbursement
  ( findInvoiceMappings,
    assertInvoicesNotAlreadyClaimedForTdsReimbursement,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DirectTax
import qualified Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping as DTdsMap
import qualified Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest as DTdsReq
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.DirectTaxTransaction as QDirectTax
import qualified Lib.Finance.Storage.Queries.FinanceTdsReimbursementInvoiceMapping as QTdsMap
import qualified Lib.Finance.Storage.Queries.FinanceTdsReimbursementRequest as QTdsReq
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra

findInvoiceMappings ::
  BeamFlow m r =>
  Id DTdsReq.FinanceTdsReimbursementRequest ->
  [DTdsMap.FinanceTdsReimbursementInvoiceMapping] ->
  m [(DTdsMap.FinanceTdsReimbursementInvoiceMapping, FinanceInvoice.Invoice)]
findInvoiceMappings requestId mappings = do
  when (null mappings) $
    throwError (InvalidRequest $ "No invoice mappings for TDS reimbursement request: " <> requestId.getId)
  let invoiceIds = map (.invoiceId) mappings
  invoices <- QFinanceInvoiceExtra.findByIds invoiceIds
  forM mappings $ \mapping -> do
    invoice <-
      find (\invoice -> invoice.id == mapping.invoiceId) invoices
        & fromMaybeM (InvalidRequest $ "Invoice not found: " <> mapping.invoiceId.getId)
    pure (mapping, invoice)

-- | Fail if any invoice is already claimed for TDS reimbursement:
--     * mapping on a PENDING/APPROVED request (other than 'mbExcludeRequestId'), or
--     * DirectTax with tdsTreatment=Reimbursed (safety net for crooked/partial flows).
--   Used on FO submit (step 1, mbExcludeRequestId=Nothing) and on ledger adjustment
--   validate/post (steps 2–3, exclude the request being processed).
assertInvoicesNotAlreadyClaimedForTdsReimbursement ::
  BeamFlow m r =>
  Maybe (Id DTdsReq.FinanceTdsReimbursementRequest) ->
  [FinanceInvoice.Invoice] ->
  m ()
assertInvoicesNotAlreadyClaimedForTdsReimbursement _ [] = pure ()
assertInvoicesNotAlreadyClaimedForTdsReimbursement mbExcludeRequestId invoices = do
  let invoiceById = M.fromList [(inv.id, inv) | inv <- invoices]
      invoiceByNumber = M.fromList [(inv.invoiceNumber, inv) | inv <- invoices]
      invoiceIds = M.keys invoiceById

  mappings <- QTdsMap.findAllByInvoiceIds invoiceIds
  requests <- QTdsReq.findAllByIds (nub $ map (.requestId) mappings)
  directTaxes <- QDirectTax.findAllByInvoiceNumbers (map (Just . (.invoiceNumber)) invoices)

  let requestById = M.fromList [(req.id, req) | req <- requests]
      mappingConflicts =
        catMaybes
          [ activeMappingConflict mbExcludeRequestId invoice mapping request
            | mapping <- mappings,
              Just invoice <- [M.lookup mapping.invoiceId invoiceById],
              Just request <- [M.lookup mapping.requestId requestById]
          ]
      directTaxConflicts =
        catMaybes
          [ reimbursedDirectTaxConflict invoice txn
            | txn <- directTaxes,
              Just invNumber <- [txn.invoiceNumber],
              Just invoice <- [M.lookup invNumber invoiceByNumber]
          ]
      conflicts = mappingConflicts <> directTaxConflicts

  unless (null conflicts) $
    throwError $
      InvalidRequest $
        "Invoice(s) already claimed for TDS reimbursement: " <> T.intercalate "; " conflicts

activeMappingConflict ::
  Maybe (Id DTdsReq.FinanceTdsReimbursementRequest) ->
  FinanceInvoice.Invoice ->
  DTdsMap.FinanceTdsReimbursementInvoiceMapping ->
  DTdsReq.FinanceTdsReimbursementRequest ->
  Maybe Text
activeMappingConflict mbExcludeRequestId invoice mapping request
  | mbExcludeRequestId == Just mapping.requestId = Nothing
  | request.status `elem` [DTdsReq.PENDING, DTdsReq.APPROVED] =
    Just $
      formatInvoice invoice
        <> " active mapping on request "
        <> mapping.requestId.getId
        <> " (status="
        <> show request.status
        <> ")"
  | otherwise = Nothing

reimbursedDirectTaxConflict ::
  FinanceInvoice.Invoice ->
  DirectTax.DirectTaxTransaction ->
  Maybe Text
reimbursedDirectTaxConflict invoice txn
  | txn.tdsTreatment == DirectTax.Reimbursed =
    Just $
      formatInvoice invoice
        <> " DirectTax Reimbursed (referenceId="
        <> txn.referenceId
        <> ", tdsAmount="
        <> show txn.tdsAmount
        <> ")"
  | otherwise = Nothing

formatInvoice :: FinanceInvoice.Invoice -> Text
formatInvoice invoice = "invoiceId=" <> invoice.id.getId <> " invoiceNumber=" <> invoice.invoiceNumber
