{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Invoice
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Invoice
import qualified Control.Lens
import qualified Domain.Action.UI.Invoice
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "invoice" :> MandatoryQueryParam "from" Kernel.Prelude.UTCTime :> MandatoryQueryParam "to" Kernel.Prelude.UTCTime :> Get '[JSON] [API.Types.UI.Invoice.InvoiceRes])

handler :: Environment.FlowServer API
handler = getInvoice

getInvoice ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.UTCTime ->
    Kernel.Prelude.UTCTime ->
    Environment.FlowHandler [API.Types.UI.Invoice.InvoiceRes]
  )
getInvoice a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Invoice.getInvoice (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
