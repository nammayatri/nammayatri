{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnSelect (API, handler) where

import qualified Beckn.ACL.OnSelect as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Data.Text as T
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import TransactionLogs.PushLogs

type API = OnSelect.OnSelectAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onSelect

onSelect ::
  SignatureAuthResult ->
  OnSelect.OnSelectReqV2 ->
  FlowHandler AckResponse
onSelect _ reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.onSelectReqContext
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
