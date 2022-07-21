module Product.Cancel
  ( CancelAPI,
    cancel,
  )
where

import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Id
import qualified Core.ACL.Cancel as ACL
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import qualified ExternalAPI.Flow as ExternalAPI
import Servant
import Utils.Auth
import Utils.Common

type CancelAPI =
  "rideBooking"
    :> Capture "rideBookingId" (Id SRB.Booking)
    :> "cancel"
    :> TokenAuth
    :> ReqBody '[JSON] DCancel.CancelReq
    :> Post '[JSON] APISuccess

cancel ::
  Id SRB.Booking ->
  Id Person.Person ->
  DCancel.CancelReq ->
  FlowHandler APISuccess
cancel bookingId personId req =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dCancelRes <- DCancel.cancel bookingId personId req
    void . ExternalAPI.cancel dCancelRes.bppUrl =<< ACL.buildCancelReq dCancelRes
    return Success
