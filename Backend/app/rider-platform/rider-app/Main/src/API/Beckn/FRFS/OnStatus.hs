{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.FRFS.OnStatus where

import qualified Beckn.ACL.FRFS.OnStatus as ACL
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.Beckn.FRFS.OnStatus as DOnStatus
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import TransactionLogs.PushLogs

type API = Spec.OnStatusAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onStatus

onStatus ::
  SignatureAuthResult ->
  Spec.OnStatusReq ->
  FlowHandler Spec.AckResponse
onStatus _ req = withFlowHandlerAPI $ do
  transaction_id <- req.onStatusReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received OnStatus request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    dOnStatusReq <- ACL.buildOnStatusReq req
    Redis.whenWithLockRedis (onConfirmLockKey dOnStatusReq.bppOrderId) 60 $ do
      (merchant, booking) <- DOnStatus.validateRequest (DOnStatus.Booking dOnStatusReq)
      fork "onStatus request processing" $
        Redis.whenWithLockRedis (onConfirmProcessingLockKey dOnStatusReq.bppOrderId) 60 $
          void $ DOnStatus.onStatus merchant booking (DOnStatus.Booking dOnStatusReq)
      fork "FRFS onStatus received pushing ondc logs" do
        void $ pushLogs "on_status" (toJSON req) merchant.id.getId "PUBLIC_TRANSPORT"
  pure Utils.ack

onConfirmLockKey :: Text -> Text
onConfirmLockKey id = "FRFS:OnStatus:bppOrderId-" <> id

onConfirmProcessingLockKey :: Text -> Text
onConfirmProcessingLockKey id = "FRFS:OnStatus:Processing:bppOrderId-" <> id
