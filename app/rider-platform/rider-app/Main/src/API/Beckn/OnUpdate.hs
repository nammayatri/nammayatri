module API.Beckn.OnUpdate (API, handler) where

import qualified Beckn.ACL.OnUpdate as ACL
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as OnUpdate
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = OnUpdate.OnUpdateAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onUpdate

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReq ->
  FlowHandler AckResponse
onUpdate (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnUpdateReq <- ACL.buildOnUpdateReq req
  whenJust mbDOnUpdateReq $ \onUpdateReq ->
    Redis.whenWithLockRedis (onUpdateLockKey req.context.message_id) 60 $
      DOnUpdate.onUpdate registryUrl onUpdateReq
  pure Ack

onUpdateLockKey :: Text -> Text
onUpdateLockKey id = "Customer:OnUpdate:MessageId-" <> id
