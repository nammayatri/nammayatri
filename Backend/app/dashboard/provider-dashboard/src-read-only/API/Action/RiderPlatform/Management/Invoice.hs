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
import "rider-app" Domain.Types.AccessMatrix
import qualified "beckn-spec" Domain.Types.Invoice
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice
import Servant
import Storage.Beam.CommonInstances ()

type API = ("invoice" :> (GetInvoiceInvoice :<|> GetInvoiceFinanceList :<|> GetInvoiceFinancePdf))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getInvoiceInvoice merchantId city :<|> getInvoiceFinanceList merchantId city :<|> getInvoiceFinancePdf merchantId city

type GetInvoiceInvoice =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.INVOICE) / ('API.Types.RiderPlatform.Management.Invoice.GET_INVOICE_INVOICE))
      :> API.Types.RiderPlatform.Management.Invoice.GetInvoiceInvoice
  )

type GetInvoiceFinanceList =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.INVOICE) / ('API.Types.RiderPlatform.Management.Invoice.GET_INVOICE_FINANCE_LIST))
      :> API.Types.RiderPlatform.Management.Invoice.GetInvoiceFinanceList
  )

type GetInvoiceFinancePdf =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.INVOICE) / ('API.Types.RiderPlatform.Management.Invoice.GET_INVOICE_FINANCE_PDF))
      :> API.Types.RiderPlatform.Management.Invoice.GetInvoiceFinancePdf
  )

getInvoiceInvoice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Invoice.InvoiceRes])
getInvoiceInvoice merchantShortId opCity apiTokenInfo from phoneNumber to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Invoice.getInvoiceInvoice merchantShortId opCity apiTokenInfo from phoneNumber to

getInvoiceFinanceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Domain.Types.Invoice.InvoiceType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.Invoice.InvoiceStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.RiderPlatform.Management.Invoice.FinanceInvoiceListRes)
getInvoiceFinanceList merchantShortId opCity apiTokenInfo from invoiceId invoiceNumber invoiceType limit offset status to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Invoice.getInvoiceFinanceList merchantShortId opCity apiTokenInfo from invoiceId invoiceNumber invoiceType limit offset status to

getInvoiceFinancePdf :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Data.Text.Text -> Environment.FlowHandler API.Types.RiderPlatform.Management.Invoice.FinanceInvoicePdfRes)
getInvoiceFinancePdf merchantShortId opCity apiTokenInfo invoiceId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Invoice.getInvoiceFinancePdf merchantShortId opCity apiTokenInfo invoiceId
