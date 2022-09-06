module API.Beckn.OnSelect (API, handler) where

import App.Types
import Beckn.Prelude
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnSelect as ACL
import qualified Domain.Action.Beckn.OnSelect as DOnSelect

type API = OnSelect.OnSelectAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onSelect

onSelect ::
  SignatureAuthResult ->
  OnSelect.OnSelectReq ->
  FlowHandler AckResponse
onSelect (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnSelectReq <- ACL.buildOnSelectReq req
  whenJust mbDOnSelectReq (DOnSelect.onSelect registryUrl)
  pure Ack
