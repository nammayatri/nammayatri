module API.UI.LiveActivity
  ( API,
    handler,
  )
where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Kernel.Types.APISuccess as APISuccess
import Tools.Auth
import qualified Domain.Action.UI.LiveActivity as LAA


type API =
  "liveActivityToken"
    :> TokenAuth
    :> ReqBody '[JSON] LAA.LiveActivityTokenReq
    :> Post '[JSON] APISuccess.APISuccess

handler :: FlowServer API
handler = liveActivityToken

liveActivityToken ::
  (Id Person.Person, Id Merchant.Merchant) ->
  LAA.LiveActivityTokenReq ->
  FlowHandler APISuccess.APISuccess
liveActivityToken (_, _) = withFlowHandlerAPI . LAA.liveActivityTokenResp
