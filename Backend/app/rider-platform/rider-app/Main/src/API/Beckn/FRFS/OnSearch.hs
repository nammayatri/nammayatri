{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.FRFS.OnSearch where

import qualified Beckn.ACL.FRFS.OnSearch as ACL
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.Beckn.FRFS.OnSearch as DOnSearch
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
-- import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()

type API = Spec.OnSearchAPI

-- handler :: SignatureAuthResult -> FlowServer API
-- handler = onSearch

handler :: FlowServer API
handler = onSearch

onSearch ::
  -- SignatureAuthResult ->
  Spec.OnSearchReq ->
  FlowHandler Spec.AckResponse
onSearch req = withFlowHandlerAPI $ do
  transaction_id <- req.onSearchReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  withTransactionIdLogTag' transaction_id $ do
    message_id <- req.onSearchReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
    onSearchReq <- ACL.buildOnSearchReq req
    Redis.whenWithLockRedis (onSearchLockKey message_id) 60 $ do
      (merchant, search) <- DOnSearch.validateRequest onSearchReq
      fork "FRFS on_search processing" $ do
        Redis.whenWithLockRedis (onSearchProcessingLockKey message_id) 60 $
          DOnSearch.onSearch onSearchReq merchant search
  pure Utils.ack

onSearchLockKey :: Text -> Text
onSearchLockKey id = "FRFS:OnSearch:MessageId-" <> id

onSearchProcessingLockKey :: Text -> Text
onSearchProcessingLockKey id = "FRFS:OnSearch:Processing:MessageId-" <> id
