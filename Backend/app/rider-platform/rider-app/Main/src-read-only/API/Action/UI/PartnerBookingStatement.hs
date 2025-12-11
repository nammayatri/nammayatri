{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PartnerBookingStatement
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PartnerBookingStatement
import qualified Domain.Action.UI.PartnerBookingStatement
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "corporate" :> "bookingStatement" :> Header "X-Partner-API-Key" Kernel.Prelude.Text :> ReqBody ('[JSON]) API.Types.UI.PartnerBookingStatement.BookingStatementReq
      :> Post
           ('[JSON])
           API.Types.UI.PartnerBookingStatement.BookingStatementRes
      :<|> "corporate"
      :> "invoiceData"
      :> Header
           "X-Partner-API-Key"
           Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.PartnerBookingStatement.InvoiceDataReq
      :> Post
           ('[JSON])
           API.Types.UI.PartnerBookingStatement.InvoiceDataRes
  )

handler :: Environment.FlowServer API
handler = postCorporateBookingStatement :<|> postCorporateInvoiceData

postCorporateBookingStatement :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.PartnerBookingStatement.BookingStatementReq -> Environment.FlowHandler API.Types.UI.PartnerBookingStatement.BookingStatementRes)
postCorporateBookingStatement a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PartnerBookingStatement.postCorporateBookingStatement a2 a1

postCorporateInvoiceData :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.PartnerBookingStatement.InvoiceDataReq -> Environment.FlowHandler API.Types.UI.PartnerBookingStatement.InvoiceDataRes)
postCorporateInvoiceData a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PartnerBookingStatement.postCorporateInvoiceData a2 a1
