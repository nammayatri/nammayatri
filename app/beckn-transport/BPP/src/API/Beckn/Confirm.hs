module API.Beckn.Confirm (API, handler) where

import App.Types
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Confirm as API
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Confirm as ACL
import qualified Core.ACL.OnConfirm as ACL
import qualified Domain.Action.Beckn.Confirm as DConfirm
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Organization as Org
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import Servant
import Utils.Common

type API =
  Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.ConfirmAPI

handler :: FlowServer API
handler = confirm

confirm ::
  Id Org.Organization ->
  SignatureAuthResult ->
  Confirm.ConfirmReq ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "confirm API Flow" "Reached"
    dConfirmReq <- ACL.buildConfirmReq subscriber req
    let context = req.context
    dConfirmRes <- DConfirm.confirm transporterId subscriber dConfirmReq
    ExternalAPI.withCallback dConfirmRes.transporter Context.CONFIRM OnConfirm.onConfirmAPI context context.bap_uri $
      -- there should be DOnConfirm.onConfirm, but it is empty anyway
      pure $ ACL.mkOnConfirmMessage dConfirmRes
