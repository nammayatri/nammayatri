{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.FRFS.OnUpdate where

import qualified Beckn.ACL.FRFS.OnUpdate as ACL
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.Beckn.FRFS.OnUpdate as DOnUpdate
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import TransactionLogs.PushLogs

type API = Spec.OnUpdateAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onUpdate

onUpdate ::
  SignatureAuthResult ->
  Spec.OnUpdateReq ->
  FlowHandler Spec.AckResponse
onUpdate _ req = withFlowHandlerAPI $ do
  transaction_id <- req.onUpdateReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  withTransactionIdLogTag' transaction_id $ do
    logDebug $ "Received OnUpdate request" <> encodeToText req
    dOnUpdateReq <- ACL.buildOnUpdateReq req
    void $
      runMaybeT $ do
        onUpdateReq <- MaybeT $ pure dOnUpdateReq
        lift $
          Redis.whenWithLockRedis (onUpdateLockKey onUpdateReq.bppOrderId) 60 $ do
            (merchant, booking) <- DOnUpdate.validateRequest onUpdateReq
            fork "onUpdate request processing" $
              Redis.whenWithLockRedis (onUpdateProcessingLockKey onUpdateReq.bppOrderId) 60 $
                DOnUpdate.onUpdate merchant booking onUpdateReq
            fork "FRFS onUpdate received pushing ondc logs" do
              void $ pushLogs "on_update" (toJSON req) merchant.id.getId "PUBLIC_TRANSPORT"
    pure Utils.ack

onUpdateLockKey :: Text -> Text
onUpdateLockKey id = "FRFS:OnUpdate:bppOrderId-" <> id

onUpdateProcessingLockKey :: Text -> Text
onUpdateProcessingLockKey id = "FRFS:OnUpdate:Processing:bppOrderId-" <> id
