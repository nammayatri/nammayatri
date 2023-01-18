module API.UI.Cancel
  ( API,
    handler,
  )
where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Core.ACL.Cancel as ACL
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import Environment
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import Tools.Auth

type API =
  "rideBooking"
    :> Capture "rideBookingId" (Id SRB.Booking)
    :> "cancel"
    :> TokenAuth
    :> ReqBody '[JSON] DCancel.CancelReq
    :> Post '[JSON] APISuccess

-------- Cancel Flow----------

handler :: FlowServer API
handler =
  cancel

cancel ::
  Id SRB.Booking ->
  Id Person.Person ->
  DCancel.CancelReq ->
  FlowHandler APISuccess
cancel bookingId personId req =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dCancelRes <- DCancel.cancel bookingId personId req
    void $ withShortRetry $ CallBPP.cancel dCancelRes.bppUrl =<< ACL.buildCancelReq dCancelRes
    return Success
