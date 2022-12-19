module API.Beckn.OnSelect (API, handler) where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnSelect as ACL
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import Environment

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
