{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Invoice
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Invoice
import qualified Data.Text
import qualified Domain.Action.Dashboard.Invoice
import qualified "beckn-spec" Domain.Types.Invoice
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("invoice" :> (GetInvoiceInvoice :<|> GetInvoiceFinanceList :<|> GetInvoiceFinancePdf))

type GetInvoiceInvoice = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/INVOICE/GET_INVOICE_INVOICE" :> API.Types.RiderPlatform.Management.Invoice.GetInvoiceInvoice)

type GetInvoiceFinanceList = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/INVOICE/GET_INVOICE_FINANCE_LIST" :> API.Types.RiderPlatform.Management.Invoice.GetInvoiceFinanceList)

type GetInvoiceFinancePdf = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/INVOICE/GET_INVOICE_FINANCE_PDF" :> API.Types.RiderPlatform.Management.Invoice.GetInvoiceFinancePdf)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getInvoiceInvoice merchantId city :<|> getInvoiceFinanceList merchantId city :<|> getInvoiceFinancePdf merchantId city

getInvoiceInvoice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Invoice.InvoiceRes])
getInvoiceInvoice a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.Invoice.getInvoiceInvoice a6 a5 a3 a2 a1

getInvoiceFinanceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.RiderPlatform.Management.Invoice.FinanceInvoiceListRes)
getInvoiceFinanceList a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a9 $ Domain.Action.Dashboard.Invoice.getInvoiceFinanceList a11 a10 a8 a7 a6 a5 a4 a3 a2 a1

getInvoiceFinancePdf :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Text.Text -> Environment.FlowHandler API.Types.RiderPlatform.Management.Invoice.FinanceInvoicePdfRes)
getInvoiceFinancePdf a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Invoice.getInvoiceFinancePdf a4 a3 a1
