module API.Beckn.OnConfirm (API, handler) where

import qualified Beckn.ACL.OnConfirm as ACL
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = OnConfirm.OnConfirmAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onConfirm

onConfirm ::
  SignatureAuthResult ->
  OnConfirm.OnConfirmReq ->
  FlowHandler AckResponse
onConfirm (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnConfirmReq <- ACL.buildOnConfirmReq req
  whenJust mbDOnConfirmReq $ \onConfirmReq ->
    Redis.whenWithLockRedis (onConfirmLockKey onConfirmReq.bppBookingId.getId) 60 $
      DOnConfirm.onConfirm registryUrl onConfirmReq
  pure Ack

onConfirmLockKey :: Text -> Text
onConfirmLockKey id = "Customer:OnConfirm:BppBookingId-" <> id
