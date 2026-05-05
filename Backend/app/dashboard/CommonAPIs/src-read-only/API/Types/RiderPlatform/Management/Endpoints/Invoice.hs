{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Invoice where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Invoice.PdfService
import Servant
import Servant.Client

data FareBreakup = FareBreakup {price :: Data.Text.Text, title :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FinanceInvoicePdfResp = FinanceInvoicePdfResp {invoiceNumber :: Data.Text.Text, pdfBase64 :: Data.Text.Text}
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

type API = ("invoice" :> (GetInvoiceFinanceInvoicePdf :<|> GetInvoiceInvoice))

type GetInvoiceFinanceInvoicePdf =
  ( "finance" :> "invoice" :> "pdf" :> QueryParam "from" Lib.Finance.Invoice.PdfService.DateOrTime
      :> QueryParam
           "invoiceType"
           Lib.Finance.Domain.Types.Invoice.InvoiceType
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "referenceId"
           Data.Text.Text
      :> QueryParam
           "to"
           Lib.Finance.Invoice.PdfService.DateOrTime
      :> Get
           '[JSON]
           FinanceInvoicePdfResp
  )

type GetInvoiceInvoice =
  ( "invoice" :> MandatoryQueryParam "from" Kernel.Prelude.UTCTime :> MandatoryQueryParam "phoneNumber" Data.Text.Text
      :> MandatoryQueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get '[JSON] [InvoiceRes]
  )

data InvoiceAPIs = InvoiceAPIs
  { getInvoiceFinanceInvoicePdf :: Kernel.Prelude.Maybe Lib.Finance.Invoice.PdfService.DateOrTime -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Lib.Finance.Invoice.PdfService.DateOrTime -> EulerHS.Types.EulerClient FinanceInvoicePdfResp,
    getInvoiceInvoice :: Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient [InvoiceRes]
  }

mkInvoiceAPIs :: (Client EulerHS.Types.EulerClient API -> InvoiceAPIs)
mkInvoiceAPIs invoiceClient = (InvoiceAPIs {..})
  where
    getInvoiceFinanceInvoicePdf :<|> getInvoiceInvoice = invoiceClient

data InvoiceUserActionType
  = GET_INVOICE_FINANCE_INVOICE_PDF
  | GET_INVOICE_INVOICE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''InvoiceUserActionType])
