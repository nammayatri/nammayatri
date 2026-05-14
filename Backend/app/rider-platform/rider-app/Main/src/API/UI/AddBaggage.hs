module API.UI.AddBaggage
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.AddBaggage as Domain
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
    :> "addBaggage"
    :> "confirm"
    :> TokenAuth
    :> ReqBody '[JSON] Domain.AddBaggageConfirmReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler bookingId = confirmAddBaggage bookingId

confirmAddBaggage :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> Domain.AddBaggageConfirmReq -> FlowHandler APISuccess
confirmAddBaggage bookingId (personId, merchantId) req =
  withFlowHandlerAPIPersonId personId $ Domain.postAddBaggageConfirm (personId, merchantId) bookingId req
