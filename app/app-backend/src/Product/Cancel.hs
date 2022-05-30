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
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RideBooking as SRB
import qualified ExternalAPI.Flow as ExternalAPI
import Servant
import Utils.Auth
import Utils.Common

type CancelAPI =
  "rideBooking"
    :> Capture "bookingId" (Id SRB.RideBooking)
    :> "cancel"
    :> TokenAuth
    :> ReqBody '[JSON] DCancel.CancelReq
    :> Post '[JSON] APISuccess

cancel ::
  Id SRB.RideBooking ->
  Id Person.Person ->
  DCancel.CancelReq ->
  FlowHandler APISuccess
cancel bookingId personId req =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dCancelRes <- DCancel.cancel bookingId personId req
    void . ExternalAPI.cancel dCancelRes.bppUrl =<< ACL.buildCancelReq dCancelRes
    return Success
