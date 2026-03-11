{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FinanceInvoice
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FinanceInvoice
import qualified Control.Lens
import qualified Data.Time
import qualified Domain.Action.UI.FinanceInvoice
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "subscription" :> "invoices" :> QueryParam "fromDate" Data.Time.UTCTime :> QueryParam "invoiceType" Lib.Finance.Domain.Types.Invoice.InvoiceType
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "toDate"
           Data.Time.UTCTime
      :> Get
           ('[JSON])
           API.Types.UI.FinanceInvoice.FinanceInvoiceListRes
  )

handler :: Environment.FlowServer API
handler = getSubscriptionInvoices

getSubscriptionInvoices ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Data.Time.UTCTime) ->
    Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.Invoice.InvoiceType) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Data.Time.UTCTime) ->
    Environment.FlowHandler API.Types.UI.FinanceInvoice.FinanceInvoiceListRes
  )
getSubscriptionInvoices a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FinanceInvoice.getSubscriptionInvoices (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a6) a5 a4 a3 a2 a1
