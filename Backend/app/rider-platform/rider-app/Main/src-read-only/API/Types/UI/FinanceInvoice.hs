{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FinanceInvoice where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Types.Invoice
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Invoice
import Servant
import Tools.Auth

data FinanceInvoiceListItem = FinanceInvoiceListItem
  { cgstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    counterpartyId :: Kernel.Prelude.Text,
    counterpartyType :: Kernel.Prelude.Text,
    generatedAt :: Data.Time.UTCTime,
    gstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    gstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    gstinOfParty :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    invoiceDate :: Data.Time.UTCTime,
    invoiceId :: Kernel.Prelude.Text,
    invoiceNumber :: Kernel.Prelude.Text,
    invoiceStatus :: Lib.Finance.Domain.Types.Invoice.InvoiceStatus,
    invoiceType :: Domain.Types.Invoice.InvoiceType,
    irn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedByAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedByTaxNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedToAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedToName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedToTaxNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lineItems :: Data.Aeson.Value,
    paymentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sacCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sgstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    subscriptionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    supplierAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    supplierGstin :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    supplierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    supplierTaxNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    taxRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    taxableValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    taxableValueOfServiceSupplied :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tdsReference :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    totalInvoiceValue :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FinanceInvoiceListResp = FinanceInvoiceListResp {invoices :: [FinanceInvoiceListItem], totalItems :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FinanceInvoicePdfResp = FinanceInvoicePdfResp {invoiceNumber :: Kernel.Prelude.Text, pdfBase64 :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
