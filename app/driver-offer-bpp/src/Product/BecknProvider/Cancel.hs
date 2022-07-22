module Product.BecknProvider.Cancel where

import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Cancel as ACL
import qualified Domain.Action.Beckn.Cancel as DCancel
import qualified Domain.Types.Organization as Organization
import Environment
import EulerHS.Prelude
import Utils.Common

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
