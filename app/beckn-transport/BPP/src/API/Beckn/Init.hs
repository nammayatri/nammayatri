module API.Beckn.Init (API, handler) where

import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Init as API
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Init as ACL
import qualified Core.ACL.OnInit as ACL
import Core.Beckn (withCallback)
import qualified Domain.Action.Beckn.Init as DInit
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Organization as Org
import Environment
import EulerHS.Prelude
import Servant

type API =
  Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.InitAPI

handler :: FlowServer API
handler = initImpl

initImpl ::
  Id Org.Organization ->
  SignatureAuthResult ->
  Init.InitReq ->
  FlowHandler AckResponse
initImpl transporterId (SignatureAuthResult _ subscriber _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    dInitReq <- ACL.buildInitReq subscriber req
    let context = req.context
    dInitRes <- DInit.init transporterId dInitReq
    withCallback dInitRes.transporter Context.INIT OnInit.onInitAPI context context.bap_uri $
      -- there should be DOnInit.onInit, but it is empty anyway
      pure $ ACL.mkOnInitMessage dInitRes
