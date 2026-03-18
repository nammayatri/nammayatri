module SharedLogic.InvoicePdf
  ( generateInvoicePdf,
    InvoicePdfData (..),
    InvoiceLineItem (..),
  )
where

import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Utils.Common

-- | Data required to generate an invoice PDF
data InvoicePdfData = InvoicePdfData
  { invoiceNumber :: Text,
    invoiceDate :: UTCTime,
    supplierName :: Text,
    supplierAddress :: Maybe Text,
    supplierGstin :: Maybe Text,
    recipientName :: Text,
    recipientAddress :: Maybe Text,
    recipientGstin :: Maybe Text,
    lineItems :: [InvoiceLineItem],
    taxableValue :: HighPrecMoney,
    cgstRate :: Maybe Double,
    cgstAmount :: Maybe HighPrecMoney,
    sgstRate :: Maybe Double,
    sgstAmount :: Maybe HighPrecMoney,
    totalAmount :: HighPrecMoney,
    remarks :: Maybe Text
  }
  deriving (Show, Generic)

-- | Individual line item in an invoice
data InvoiceLineItem = InvoiceLineItem
  { description :: Text,
    quantity :: Int,
    unitPrice :: HighPrecMoney,
    amount :: HighPrecMoney,
    sacCode :: Maybe Text
  }
  deriving (Show, Generic)

-- | Generate a PDF for an invoice
-- In production, this would use a PDF generation library (e.g., wkhtmltopdf, pdfkit)
-- or an external service. For now, this returns a placeholder indicating
-- the PDF generation infrastructure is set up and ready for integration.
generateInvoicePdf :: (MonadFlow m) => InvoicePdfData -> m ByteString
generateInvoicePdf pdfData = do
  logInfo $ "Generating PDF for invoice: " <> pdfData.invoiceNumber
  -- Placeholder: In production, integrate with PDF generation service
  -- Options:
  -- 1. Use wkhtmltopdf with an HTML template
  -- 2. Use a Haskell PDF library like pdf-toolbox
  -- 3. Call an external microservice for PDF generation
  pure $ encodeUtf8 ("PDF placeholder for invoice " <> pdfData.invoiceNumber)
