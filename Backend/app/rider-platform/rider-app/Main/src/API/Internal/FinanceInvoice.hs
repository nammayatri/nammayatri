module API.Internal.FinanceInvoice
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Domain.Action.Internal.FinanceInvoice as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.Invoice (InvoiceType)
import Lib.Finance.Invoice.PdfService (DateOrTime)
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()

type API =
  "finance"
    :> "invoice"
    :> "pdf"
    :> "bpp"
    :> Header "token" Text
    :> QueryParam "bppBookingId" Text
    :> QueryParam "from" DateOrTime
    :> QueryParam "to" DateOrTime
    :> QueryParam "invoiceType" InvoiceType
    :> Get '[JSON] API.FinanceInvoicePdfResp

handler :: FlowServer API
handler = getFinanceInvoicePdfByBppBookingId

getFinanceInvoicePdfByBppBookingId ::
  Maybe Text ->
  Maybe Text ->
  Maybe DateOrTime ->
  Maybe DateOrTime ->
  Maybe InvoiceType ->
  FlowHandler API.FinanceInvoicePdfResp
getFinanceInvoicePdfByBppBookingId token mbBppBookingId mbFrom mbTo mbInvoiceType =
  withFlowHandlerAPI $ Domain.getFinanceInvoicePdfByBppBookingId token mbBppBookingId mbFrom mbTo mbInvoiceType
