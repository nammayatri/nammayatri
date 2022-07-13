module API.Beckn.Init where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Init as API
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Init as ACL
import qualified Core.ACL.OnInit as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.Init as DInit
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Organization as Org
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import Servant
import Utils.Common

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
initImpl transporterId (SignatureAuthResult signPayload subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    Esq.runTransaction $
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    dInitReq <- ACL.buildInitReq subscriber req
    let context = req.context
    dInitRes <- DInit.init transporterId dInitReq
    ExternalAPI.withCallback dInitRes.transporter Context.INIT OnInit.onInitAPI context context.bap_uri $
      -- there should be DOnInit.onInit, but it is empty anyway
      pure $ ACL.mkOnInitMessage dInitRes
