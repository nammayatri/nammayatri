{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Invoice
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Invoice
import qualified Data.Text
import qualified Domain.Action.RiderPlatform.Management.Invoice
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Invoice.PdfService
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("invoice" :> (GetInvoiceFinanceInvoicePdf :<|> GetInvoiceInvoice))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getInvoiceFinanceInvoicePdf merchantId city :<|> getInvoiceInvoice merchantId city

type GetInvoiceFinanceInvoicePdf =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INVOICE / 'API.Types.RiderPlatform.Management.Invoice.GET_INVOICE_FINANCE_INVOICE_PDF)
      :> API.Types.RiderPlatform.Management.Invoice.GetInvoiceFinanceInvoicePdf
  )

type GetInvoiceInvoice =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INVOICE / 'API.Types.RiderPlatform.Management.Invoice.GET_INVOICE_INVOICE)
      :> API.Types.RiderPlatform.Management.Invoice.GetInvoiceInvoice
  )

getInvoiceFinanceInvoicePdf :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Lib.Finance.Invoice.PdfService.DateOrTime -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Lib.Finance.Invoice.PdfService.DateOrTime -> Environment.FlowHandler API.Types.RiderPlatform.Management.Invoice.FinanceInvoicePdfResp)
getInvoiceFinanceInvoicePdf merchantShortId opCity apiTokenInfo from invoiceType limit offset referenceId to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Invoice.getInvoiceFinanceInvoicePdf merchantShortId opCity apiTokenInfo from invoiceType limit offset referenceId to

getInvoiceInvoice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Invoice.InvoiceRes])
getInvoiceInvoice merchantShortId opCity apiTokenInfo from phoneNumber to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Invoice.getInvoiceInvoice merchantShortId opCity apiTokenInfo from phoneNumber to
