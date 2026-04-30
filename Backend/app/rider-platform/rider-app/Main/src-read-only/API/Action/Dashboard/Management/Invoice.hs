{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Invoice
  ( API.Types.RiderPlatform.Management.Invoice.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Invoice
import qualified Data.Text
import qualified Domain.Action.Dashboard.Invoice
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Invoice.PdfService
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Invoice.API)
handler merchantId city = getInvoiceFinanceInvoicePdf merchantId city :<|> getInvoiceInvoice merchantId city

getInvoiceFinanceInvoicePdf :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Lib.Finance.Invoice.PdfService.DateOrTime -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Lib.Finance.Invoice.PdfService.DateOrTime -> Environment.FlowHandler API.Types.RiderPlatform.Management.Invoice.FinanceInvoicePdfResp)
getInvoiceFinanceInvoicePdf a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Invoice.getInvoiceFinanceInvoicePdf a8 a7 a6 a5 a4 a3 a2 a1

getInvoiceInvoice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Invoice.InvoiceRes])
getInvoiceInvoice a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Invoice.getInvoiceInvoice a5 a4 a3 a2 a1
