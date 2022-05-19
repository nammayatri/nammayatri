module Product.Init where

import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess
import Beckn.Types.Id
import qualified Core.ACL.Init as ACL
import qualified Domain.Action.UI.Init as DInit
import qualified Domain.Types.Person as SP
import qualified ExternalAPI.Flow as ExternalAPI
import Servant
import Utils.Auth
import Utils.Common

type InitAPI =
  "rideInit"
    :> TokenAuth
    :> ReqBody '[JSON] DInit.InitReq
    :> Post '[JSON] InitRes

type InitRes = APISuccess

initFlow ::
  Id SP.Person ->
  DInit.InitReq ->
  FlowHandler APISuccess
initFlow _ req =
  withFlowHandlerAPI $ do
    dInitRes <- DInit.init req
    void . ExternalAPI.init dInitRes.providerUrl =<< ACL.buildInitReq dInitRes
    return Success
