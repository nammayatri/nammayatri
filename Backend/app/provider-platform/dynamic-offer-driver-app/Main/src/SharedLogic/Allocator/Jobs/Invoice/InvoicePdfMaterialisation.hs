{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Invoice.InvoicePdfMaterialisation
  ( runGenerateInvoicePdfJob,
  )
where

import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Finance.InvoiceDocument (InvoicePdfFlow, materialiseInvoicePdf)

-- | Render + store an invoice PDF in S3 (stamping 'pdfS3Path'). Scheduled right
--   after the invoice is created and fully stamped (place of supply, QR), so the
--   stored PDF is final. Idempotent — an already-stored invoice is skipped.
runGenerateInvoicePdfJob ::
  InvoicePdfFlow m r =>
  Job 'GenerateInvoicePdf ->
  m ExecutionResult
runGenerateInvoicePdfJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let invoiceIdText = jobInfo.jobData.invoiceId
  eRes <- withTryCatch "runGenerateInvoicePdfJob" $ materialiseInvoicePdf (Id invoiceIdText)
  case eRes of
    Right () -> pure Complete
    Left err -> do
      logError $ "GenerateInvoicePdf failed for invoice " <> invoiceIdText <> ", retrying: " <> show err
      pure Retry
