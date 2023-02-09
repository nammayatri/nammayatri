module API.UI.Frontend
  ( DFrontend.GetPersonFlowStatusRes,
    DFrontend.FrontendEvent (..),
    DFrontend.NotifyEventReq (..),
    DFrontend.NotifyEventResp,
    API,
    handler,
  )
where

import qualified Domain.Action.UI.Frontend as DFrontend
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "frontend"
    :> ( "flowStatus"
           :> TokenAuth
           :> Get '[JSON] DFrontend.GetPersonFlowStatusRes
           :<|> "notifyEvent"
             :> TokenAuth
             :> ReqBody '[JSON] DFrontend.NotifyEventReq
             :> Post '[JSON] DFrontend.NotifyEventResp
       )

handler :: FlowServer API
handler =
  getPersonFlowStatus
    :<|> notifyEvent

getPersonFlowStatus :: Id Person.Person -> FlowHandler DFrontend.GetPersonFlowStatusRes
getPersonFlowStatus = withFlowHandlerAPI . DFrontend.getPersonFlowStatus

notifyEvent :: Id Person.Person -> DFrontend.NotifyEventReq -> FlowHandler DFrontend.NotifyEventResp
notifyEvent personId = withFlowHandlerAPI . DFrontend.notifyEvent personId
