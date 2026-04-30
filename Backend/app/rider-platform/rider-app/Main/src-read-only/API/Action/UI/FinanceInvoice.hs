{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FinanceInvoice
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FinanceInvoice
import qualified Control.Lens
import qualified Domain.Action.UI.FinanceInvoice
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Invoice.PdfService
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "finance" :> "invoice" :> "pdf" :> QueryParam "from" Lib.Finance.Invoice.PdfService.DateOrTime
      :> QueryParam
           "invoiceType"
           Lib.Finance.Domain.Types.Invoice.InvoiceType
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "referenceId"
           Kernel.Prelude.Text
      :> QueryParam
           "to"
           Lib.Finance.Invoice.PdfService.DateOrTime
      :> Get
           ('[JSON])
           API.Types.UI.FinanceInvoice.FinanceInvoicePdfResp
  )

handler :: Environment.FlowServer API
handler = getFinanceInvoicePdf

getFinanceInvoicePdf ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Lib.Finance.Invoice.PdfService.DateOrTime) ->
    Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.Invoice.InvoiceType) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Text) ->
    Kernel.Prelude.Maybe (Lib.Finance.Invoice.PdfService.DateOrTime) ->
    Environment.FlowHandler API.Types.UI.FinanceInvoice.FinanceInvoicePdfResp
  )
getFinanceInvoicePdf a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FinanceInvoice.getFinanceInvoicePdf (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a7) a6 a5 a4 a3 a2 a1
