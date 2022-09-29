module API.Beckn.OnUpdate (API, handler) where

import App.Types
import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as OnUpdate
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnUpdate as ACL
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate

type API = OnUpdate.OnUpdateAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onUpdate

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReq ->
  FlowHandler AckResponse
onUpdate (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnUpdateReq <- ACL.buildOnUpdateReq req
  Beckn.Prelude.whenJust mbDOnUpdateReq (DOnUpdate.onUpdate registryUrl)
  pure Ack
