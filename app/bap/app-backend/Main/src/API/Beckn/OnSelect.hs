module API.Beckn.OnSelect (API, handler) where

import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import qualified Core.ACL.OnSelect as ACL
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = OnSelect.OnSelectAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onSelect

onSelect ::
  SignatureAuthResult ->
  OnSelect.OnSelectReq ->
  FlowHandler AckResponse
onSelect (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnSelectReq <- ACL.buildOnSelectReq req
  whenJust mbDOnSelectReq $ \onSelectReq ->
    Redis.whenWithLockRedis (onSelectLockKey req.context.message_id) 60 $
      DOnSelect.onSelect registryUrl onSelectReq
  pure Ack

onSelectLockKey :: Text -> Text
onSelectLockKey id = "Customer:OnSelect:MessageId-" <> id
