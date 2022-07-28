module API.Beckn.Cancel (API, handler) where

import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Cancel as API
import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Cancel as ACL
import qualified Domain.Action.Beckn.Cancel as DCancel
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Organization as Organization
import Environment
import EulerHS.Prelude
import Servant

type API =
  Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.CancelAPI

handler :: FlowServer API
handler = cancel

cancel ::
  Id Organization.Organization ->
  SignatureAuthResult ->
  Cancel.CancelReq ->
  FlowHandler AckResponse
cancel transporterId subscriber req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Cancel API Flow" "Reached"
    dConfirmReq <- ACL.buildCancelReq req
    DCancel.cancel transporterId subscriber dConfirmReq
    return Ack
