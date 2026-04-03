{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.FinanceInvoice where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Data.Time
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Data.Aeson



data FinanceInvoiceItem
    = FinanceInvoiceItem {cgstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                          cgstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
                          gstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
                          gstinOfParty :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          igstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                          igstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
                          invoiceDate :: Data.Time.UTCTime,
                          invoiceNumber :: Kernel.Prelude.Text,
                          invoiceType :: Lib.Finance.Domain.Types.Invoice.InvoiceType,
                          issuedByAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          issuedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          issuedByTaxNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          issuedToAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          issuedToName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          issuedToTaxNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          lineItems :: Kernel.Prelude.Maybe Data.Aeson.Value,
                          paymentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          sacCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          sgstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                          sgstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
                          supplierAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          supplierGSTIN :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          supplierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          supplierTaxNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          taxAmount :: Kernel.Types.Common.HighPrecMoney,
                          taxRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
                          taxableValue :: Kernel.Types.Common.HighPrecMoney,
                          totalAmountPayable :: Kernel.Types.Common.HighPrecMoney,
                          totalCredit :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                          totalGstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data FinanceInvoiceListRes
    = FinanceInvoiceListRes {invoices :: [FinanceInvoiceItem], totalItems :: Kernel.Prelude.Int}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



