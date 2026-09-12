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
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("invoice" :> GetInvoiceInvoice)

type GetInvoiceInvoice = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/INVOICE/GET_INVOICE_INVOICE" :> API.Types.RiderPlatform.Management.Invoice.GetInvoiceInvoice)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getInvoiceInvoice merchantId city

getInvoiceInvoice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Invoice.InvoiceRes])
getInvoiceInvoice a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Invoice.getInvoiceInvoice a6 a5 a3 a2 a1
