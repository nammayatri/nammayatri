{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnSelect (API, handler, onSelectWebhook) where

import qualified Beckn.ACL.OnSelect as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Data.Text as T
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.InboundGate as InboundGate
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Tools.ActorInfo as ActorInfo
import TransactionLogs.PushLogs

type API = OnSelect.OnSelectAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onSelect

onSelectWebhook :: OnSelect.OnSelectReqV2 -> FlowHandler AckResponse
onSelectWebhook = onSelect (error "OnSelect webhook: SignatureAuthResult not present (verified upstream by onix)")

onSelect ::
  SignatureAuthResult ->
  OnSelect.OnSelectReqV2 ->
  FlowHandler AckResponse
onSelect _ reqV2 = withFlowHandlerBecknAPI . ActorInfo.withRequestIdActorInfo $ do
  drop' <- do
    mbMerchant <- case reqV2.onSelectReqContext.contextBapId of
      Nothing -> pure Nothing
      Just bapId -> CQMerchant.findBySubscriberId (ShortId bapId)
    case mbMerchant of
      Nothing -> pure False
      Just merchant -> InboundGate.shouldDropSigned "on_select" merchant.id (reqV2.onSelectReqContext.contextBppId)
  if drop'
    then pure Ack
    else do
      transactionId <- Utils.getTransactionId reqV2.onSelectReqContext
      L.setOptionLocal TxnIdKey transactionId
      Utils.withTransactionIdLogTag transactionId $ do
        mbDOnSelectReq <- ACL.buildOnSelectReqV2 reqV2
        messageId <- Utils.getMessageIdText reqV2.onSelectReqContext
        whenJust mbDOnSelectReq $ \onSelectReq ->
          Redis.whenWithLockRedis (onSelectLockKey messageId) 60 $ do
            validatedOnSelectReq <- DOnSelect.validateRequest onSelectReq
            fork "on select received pushing ondc logs" do
              void $ pushLogs "on_select" (toJSON reqV2) validatedOnSelectReq.searchRequest.merchantId.getId "MOBILITY"
            fork "on select processing" $ do
              Redis.whenWithLockRedis (onSelectProcessingLockKey messageId) 60 $
                DOnSelect.onSelect validatedOnSelectReq
        pure Ack

onSelectLockKey :: Text -> Text
onSelectLockKey id = "Customer:OnSelect:MessageId-" <> id

onSelectProcessingLockKey :: Text -> Text
onSelectProcessingLockKey id = "Customer:OnSelect:Processing:MessageId-" <> id
