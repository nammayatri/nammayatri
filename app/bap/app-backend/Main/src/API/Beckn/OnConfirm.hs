module API.Beckn.OnConfirm (API, handler) where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnConfirm as ACL
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Environment

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
