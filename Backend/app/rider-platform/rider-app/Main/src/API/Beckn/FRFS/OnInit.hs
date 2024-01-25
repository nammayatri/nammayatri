{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.FRFS.OnInit where

import qualified Beckn.ACL.FRFS.OnInit as ACL
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.Beckn.FRFS.OnInit as DOnInit
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()

type API = Spec.OnInitAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onInit

onInit ::
  SignatureAuthResult ->
  Spec.OnInitReq ->
  FlowHandler Spec.AckResponse
onInit _ req = withFlowHandlerAPI $ do
  transaction_id <- req.onInitReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received OnInit request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    onInitReq <- ACL.buildOnInitReq req
    Redis.whenWithLockRedis (onInitLockKey onInitReq.messageId) 60 $ do
      (merchant, booking) <- DOnInit.validateRequest onInitReq
      fork "FRFS on_init processing" $ do
        Redis.whenWithLockRedis (onInitProcessingLockKey onInitReq.messageId) 60 $
          DOnInit.onInit onInitReq merchant booking
  pure Utils.ack

onInitLockKey :: Text -> Text
onInitLockKey id = "FRFS:OnInit:MessageId-" <> id

onInitProcessingLockKey :: Text -> Text
onInitProcessingLockKey id = "FRFS:OnInit:Processing:MessageId-" <> id
