{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Invoice where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified "beckn-spec" Domain.Types.Invoice
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Invoice
import Servant
import Servant.Client

data FareBreakup = FareBreakup {price :: Data.Text.Text, title :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FinanceInvoiceListItem = FinanceInvoiceListItem
  { cgstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    counterpartyId :: Data.Text.Text,
    counterpartyType :: Data.Text.Text,
    generatedAt :: Kernel.Prelude.UTCTime,
    gstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    gstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    gstinOfParty :: Kernel.Prelude.Maybe Data.Text.Text,
    igstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    invoiceDate :: Kernel.Prelude.UTCTime,
    invoiceId :: Data.Text.Text,
    invoiceNumber :: Data.Text.Text,
    invoiceStatus :: Lib.Finance.Domain.Types.Invoice.InvoiceStatus,
    invoiceType :: Domain.Types.Invoice.InvoiceType,
    irn :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedByAddress :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedByName :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedByTaxNo :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedToAddress :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedToName :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedToTaxNo :: Kernel.Prelude.Maybe Data.Text.Text,
    lineItems :: Data.Aeson.Value,
    merchantGstin :: Kernel.Prelude.Maybe Data.Text.Text,
    paymentMethod :: Kernel.Prelude.Maybe Data.Text.Text,
    qrCode :: Kernel.Prelude.Maybe Data.Text.Text,
    rideId :: Kernel.Prelude.Maybe Data.Text.Text,
    sacCode :: Kernel.Prelude.Maybe Data.Text.Text,
    sgstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    supplierAddress :: Kernel.Prelude.Maybe Data.Text.Text,
    supplierGstin :: Kernel.Prelude.Maybe Data.Text.Text,
    supplierId :: Kernel.Prelude.Maybe Data.Text.Text,
    supplierName :: Kernel.Prelude.Maybe Data.Text.Text,
    supplierTaxNo :: Kernel.Prelude.Maybe Data.Text.Text,
    taxRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    taxableValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    taxableValueOfServiceSupplied :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalInvoiceValue :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FinanceInvoiceListRes = FinanceInvoiceListRes {invoices :: [FinanceInvoiceListItem], summary :: Dashboard.Common.Summary, totalItems :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FinanceInvoicePdfRes = FinanceInvoicePdfRes {invoiceNumber :: Data.Text.Text, pdfBase64 :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data InvoiceRes = InvoiceRes
  { chargeableDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    chargeableDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    date :: Kernel.Prelude.UTCTime,
    destination :: Data.Text.Text,
    driverName :: Data.Text.Text,
    faresList :: [FareBreakup],
    rideEndTime :: Kernel.Prelude.UTCTime,
    rideStartTime :: Kernel.Prelude.UTCTime,
    shortRideId :: Data.Text.Text,
    source :: Data.Text.Text,
    totalAmount :: Data.Text.Text,
    vehicleNumber :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("invoice" :> (GetInvoiceInvoice :<|> GetInvoiceFinanceList :<|> GetInvoiceFinancePdf))

type GetInvoiceInvoice =
  ( "invoice" :> MandatoryQueryParam "from" Kernel.Prelude.UTCTime :> MandatoryQueryParam "phoneNumber" Data.Text.Text
      :> MandatoryQueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get '[JSON] [InvoiceRes]
  )

type GetInvoiceFinanceList =
  ( "finance" :> "list" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "invoiceId" Data.Text.Text
      :> QueryParam
           "invoiceNumber"
           Data.Text.Text
      :> QueryParam "invoiceType" Domain.Types.Invoice.InvoiceType
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           Lib.Finance.Domain.Types.Invoice.InvoiceStatus
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           FinanceInvoiceListRes
  )

type GetInvoiceFinancePdf = ("finance" :> "pdf" :> MandatoryQueryParam "invoiceId" Data.Text.Text :> Get '[JSON] FinanceInvoicePdfRes)

data InvoiceAPIs = InvoiceAPIs
  { getInvoiceInvoice :: Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient [InvoiceRes],
    getInvoiceFinanceList :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient FinanceInvoiceListRes,
    getInvoiceFinancePdf :: Data.Text.Text -> EulerHS.Types.EulerClient FinanceInvoicePdfRes
  }

mkInvoiceAPIs :: (Client EulerHS.Types.EulerClient API -> InvoiceAPIs)
mkInvoiceAPIs invoiceClient = (InvoiceAPIs {..})
  where
    getInvoiceInvoice :<|> getInvoiceFinanceList :<|> getInvoiceFinancePdf = invoiceClient

data InvoiceUserActionType
  = GET_INVOICE_INVOICE
  | GET_INVOICE_FINANCE_LIST
  | GET_INVOICE_FINANCE_PDF
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''InvoiceUserActionType])
