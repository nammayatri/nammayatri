module Lib.Finance.Invoice.PdfService
  ( -- * Re-exported from beckn-spec
    DateOrTime (..),
    toUTCTimeFrom,
    toUTCTimeTo,

    -- * Invoice PDF data prep (consumed by SharedLogic.RenderInvoiceFromTemplate)
    InvoicePdfData (..),
    buildInvoicePdfData,
    parseLineItems,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Domain.Types.Invoice (DateOrTime (..), toUTCTimeFrom, toUTCTimeTo)
import Kernel.Prelude
import Lib.Finance.Domain.Types.IndirectTaxTransaction (IndirectTaxTransaction)
import Lib.Finance.Domain.Types.Invoice (Invoice)
import Lib.Finance.Invoice.Interface (InvoiceLineItem)

data InvoicePdfData = InvoicePdfData
  { financeInvoice :: Invoice,
    parsedLineItems :: [InvoiceLineItem],
    mbTaxTxn :: Maybe IndirectTaxTransaction,
    mbPaymentMethodType :: Maybe Text,
    mbCardBrand :: Maybe Text,
    mbCardLastFour :: Maybe Text,
    mbRecipientBusinessId :: Maybe Text,
    mbSellerBusinessId :: Maybe Text,
    mbSellerVatNumber :: Maybe Text
  }

-- | Positional args (rather than record syntax) for the trailing fields so
-- consuming packages aren't blocked by DuplicateRecordFields collisions with
-- InvoiceContext.
buildInvoicePdfData ::
  Invoice ->
  [InvoiceLineItem] ->
  Maybe IndirectTaxTransaction ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  InvoicePdfData
buildInvoicePdfData inv items mbTax mbPayType mbBrand mbLast4 mbRecipientBid mbSellerBid mbSellerVat =
  InvoicePdfData
    { financeInvoice = inv,
      parsedLineItems = items,
      mbTaxTxn = mbTax,
      mbPaymentMethodType = mbPayType,
      mbCardBrand = mbBrand,
      mbCardLastFour = mbLast4,
      mbRecipientBusinessId = mbRecipientBid,
      mbSellerBusinessId = mbSellerBid,
      mbSellerVatNumber = mbSellerVat
    }

-- | Throws on decode failure or when any item lacks the typed 'itemType'.
-- List endpoints wrap this in 'withTryCatch' to skip bad rows; single-invoice
-- endpoints propagate the 500.
parseLineItems :: Aeson.Value -> [InvoiceLineItem]
parseLineItems val =
  case Aeson.fromJSON val of
    Aeson.Success items
      | all (isJust . (.itemType)) items -> items
      | otherwise -> error "parseLineItems: encountered line item without typed itemType"
    Aeson.Error err -> error $ "parseLineItems: invoice lineItems JSON failed to decode: " <> T.pack err
