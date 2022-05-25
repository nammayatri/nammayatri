module Product.BecknProvider.Init where

import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Init as ACL
import qualified Core.ACL.OnInit as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Organization as Org
import qualified ExternalAPI.Flow as ExternalAPI
import Utils.Common

init ::
  Id Org.Organization ->
  SignatureAuthResult ->
  Init.InitReq ->
  FlowHandler AckResponse
init transporterId (SignatureAuthResult signPayload subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    Esq.runTransaction $
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    dInitReq <- ACL.buildInitReq subscriber req
    let context = req.context
    dInitRes <- DInit.init transporterId dInitReq
    ExternalAPI.withCallback dInitRes.transporter Context.INIT OnInit.onInitAPI context context.bap_uri $
      -- there should be DOnInit.onInit, but it is empty anyway
      pure $ ACL.mkOnInitMessage dInitRes
