module Product.BecknProvider.Init (init) where

import Beckn.Prelude hiding (init)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Init as ACL
import qualified Core.ACL.OnInit as ACL
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Organization as Org
import Environment
import ExternalAPI.Flow as ExternalAPI
import Utils.Common

init ::
  Id Org.Organization ->
  SignatureAuthResult ->
  Init.InitReq ->
  FlowHandler AckResponse
init transporterId (SignatureAuthResult _ subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Init API Flow" "Reached"
    dInitReq <- ACL.buildInitReq subscriber req
    let context = req.context
    dInitRes <- DInit.handler transporterId dInitReq
    ExternalAPI.withCallback dInitRes.transporter Context.INIT OnInit.onInitAPI context context.bap_uri $
      pure $ ACL.mkOnInitMessage dInitRes
