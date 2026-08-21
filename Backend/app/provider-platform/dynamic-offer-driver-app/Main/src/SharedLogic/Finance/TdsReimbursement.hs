module SharedLogic.Finance.TdsReimbursement
  ( findInvoiceMappings,
    assertInvoicesNotAlreadyClaimedForTdsReimbursement,
  )
where

import Control.Monad.Extra (concatMapM)
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
  m [(DTdsMap.FinanceTdsReimbursementInvoiceMapping, FinanceInvoice.Invoice)]
findInvoiceMappings requestId = do
  mappings <- QTdsMap.findAllByRequestId requestId
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
assertInvoicesNotAlreadyClaimedForTdsReimbursement mbExcludeRequestId invoices = do
  conflicts <- concatMapM (findClaimConflicts mbExcludeRequestId) invoices
  unless (null conflicts) $
    throwError $
      InvalidRequest $
        "Invoice(s) already claimed for TDS reimbursement: " <> T.intercalate "; " conflicts

findClaimConflicts ::
  BeamFlow m r =>
  Maybe (Id DTdsReq.FinanceTdsReimbursementRequest) ->
  FinanceInvoice.Invoice ->
  m [Text]
findClaimConflicts mbExcludeRequestId invoice = do
  mappingConflicts <- findActiveMappingConflicts mbExcludeRequestId invoice
  directTaxConflicts <- findReimbursedDirectTaxConflicts invoice
  pure $ mappingConflicts <> directTaxConflicts

findActiveMappingConflicts ::
  BeamFlow m r =>
  Maybe (Id DTdsReq.FinanceTdsReimbursementRequest) ->
  FinanceInvoice.Invoice ->
  m [Text]
findActiveMappingConflicts mbExcludeRequestId invoice = do
  mappings <- QTdsMap.findAllByInvoiceId invoice.id
  fmap catMaybes $
    forM mappings $ \mapping -> do
      if mbExcludeRequestId == Just mapping.requestId
        then pure Nothing
        else do
          mbRequest <- QTdsReq.findByPrimaryKey mapping.requestId
          pure $ case mbRequest of
            Just request
              | request.status `elem` [DTdsReq.PENDING, DTdsReq.APPROVED] ->
                Just $
                  formatInvoice invoice
                    <> " active mapping on request "
                    <> mapping.requestId.getId
                    <> " (status="
                    <> show request.status
                    <> ")"
            _ -> Nothing

findReimbursedDirectTaxConflicts :: BeamFlow m r => FinanceInvoice.Invoice -> m [Text]
findReimbursedDirectTaxConflicts invoice = do
  directTaxes <- QDirectTax.findByInvoiceNumber (Just invoice.invoiceNumber)
  pure
    [ formatInvoice invoice
        <> " DirectTax Reimbursed (referenceId="
        <> txn.referenceId
        <> ", tdsAmount="
        <> show txn.tdsAmount
        <> ")"
      | txn <- directTaxes,
        txn.tdsTreatment == DirectTax.Reimbursed
    ]

formatInvoice :: FinanceInvoice.Invoice -> Text
formatInvoice invoice = "invoiceId=" <> invoice.id.getId <> " invoiceNumber=" <> invoice.invoiceNumber
