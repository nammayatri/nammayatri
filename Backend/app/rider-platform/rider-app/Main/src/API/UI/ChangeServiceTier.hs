module API.UI.ChangeServiceTier
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.ChangeServiceTier as Domain
import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API =
  "booking"
    :> Capture "bookingId" (Id SRB.Booking)
    :> "changeServiceTier"
    :> ( "quotes"
           :> TokenAuth
           :> Get '[JSON] DQuote.GetQuotesRes
           :<|> "confirm"
             :> TokenAuth
             :> ReqBody '[JSON] Domain.ChangeServiceTierConfirmReq
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler bookingId =
  getQuotes bookingId
    :<|> confirmChange bookingId

getQuotes :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> FlowHandler DQuote.GetQuotesRes
getQuotes bookingId (personId, merchantId) = withFlowHandlerAPIPersonId personId $ Domain.getChangeServiceTierQuotes (personId, merchantId) bookingId

confirmChange :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> Domain.ChangeServiceTierConfirmReq -> FlowHandler APISuccess
confirmChange bookingId (personId, merchantId) req = withFlowHandlerAPIPersonId personId $ Domain.postChangeServiceTierConfirm (personId, merchantId) bookingId req
