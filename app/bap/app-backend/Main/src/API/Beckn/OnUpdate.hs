module API.Beckn.OnUpdate (API, handler) where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as OnUpdate
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnUpdate as ACL
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import Environment

type API = OnUpdate.OnUpdateAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onUpdate

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReq ->
  FlowHandler AckResponse
onUpdate (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnUpdateReq <- ACL.buildOnUpdateReq req
  Beckn.Prelude.whenJust mbDOnUpdateReq $ \onUpdateReq ->
    Redis.whenWithLockRedis (onUpdateLockKey req.context.message_id) 60 $
      DOnUpdate.onUpdate registryUrl onUpdateReq
  pure Ack

onUpdateLockKey :: Text -> Text
onUpdateLockKey id = "Customer:OnUpdate:MessageId-" <> id
