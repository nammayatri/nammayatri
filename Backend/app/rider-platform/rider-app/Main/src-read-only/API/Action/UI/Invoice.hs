{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Invoice
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Invoice
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.Invoice
import qualified Domain.Types.Merchant
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
  ( TokenAuth :> "invoice" :> MandatoryQueryParam "from" Kernel.Prelude.UTCTime :> MandatoryQueryParam "to" Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           [API.Types.UI.Invoice.InvoiceRes]
      :<|> TokenAuth
      :> "invoice"
      :> "list"
      :> QueryParam
           "invoiceType"
           Lib.Finance.Domain.Types.Invoice.InvoiceType
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "referenceId"
           Data.Text.Text
      :> Get
           ('[JSON])
           API.Types.UI.Invoice.FinanceInvoiceListRes
  )

handler :: Environment.FlowServer API
handler = getInvoice :<|> getInvoiceList

getInvoice ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.UTCTime ->
    Kernel.Prelude.UTCTime ->
    Environment.FlowHandler [API.Types.UI.Invoice.InvoiceRes]
  )
getInvoice a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Invoice.getInvoice (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getInvoiceList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.Invoice.InvoiceType) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    Environment.FlowHandler API.Types.UI.Invoice.FinanceInvoiceListRes
  )
getInvoiceList a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Invoice.getInvoiceList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1
