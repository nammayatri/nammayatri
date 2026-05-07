module API.Internal.FinanceInvoice
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Domain.Action.Internal.FinanceInvoice as Domain
import Domain.Types.Invoice (InvoiceType)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Lib.Finance.Invoice.PdfService (DateOrTime)
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()

type API =
  PdfAPI :<|> ListAPI

type PdfAPI =
  "finance"
    :> "invoice"
    :> "pdf"
    :> "bpp"
    :> Header "token" Text
    :> QueryParam "bppBookingId" Text
    :> QueryParam "invoiceId" Text
    :> QueryParam "from" DateOrTime
    :> QueryParam "to" DateOrTime
    :> QueryParam "invoiceType" InvoiceType
    :> Get '[JSON] API.FinanceInvoicePdfResp

type ListAPI =
  "finance"
    :> "invoice"
    :> "list"
    :> "bpp"
    :> Header "token" Text
    :> QueryParam "bppBookingId" Text
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> QueryParam "invoiceType" InvoiceType
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] API.FinanceInvoiceListResp

handler :: FlowServer API
handler = getFinanceInvoicePdfByBppBookingId :<|> getFinanceInvoiceListBySupplier

getFinanceInvoicePdfByBppBookingId ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe DateOrTime ->
  Maybe DateOrTime ->
  Maybe InvoiceType ->
  FlowHandler API.FinanceInvoicePdfResp
getFinanceInvoicePdfByBppBookingId token mbBppBookingId mbInvoiceId mbFrom mbTo mbInvoiceType =
  withFlowHandlerAPI $ Domain.getFinanceInvoicePdfByBppBookingId token mbBppBookingId mbInvoiceId mbFrom mbTo mbInvoiceType

getFinanceInvoiceListBySupplier ::
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler API.FinanceInvoiceListResp
getFinanceInvoiceListBySupplier token mbBppBookingId mbFrom mbTo mbInvoiceType mbLimit mbOffset =
  withFlowHandlerAPI $ Domain.getFinanceInvoiceListBySupplier token mbBppBookingId mbFrom mbTo mbInvoiceType mbLimit mbOffset
