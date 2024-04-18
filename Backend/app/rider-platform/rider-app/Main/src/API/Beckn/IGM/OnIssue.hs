{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.IGM.OnIssue where

import qualified Beckn.ACL.IGM.OnIssue as ACL
import qualified BecknV2.IGM.APIs as Spec
import qualified Domain.Action.Beckn.IGM.OnIssue as DOnIssue
import Environment
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.OnIssueAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onIssue

onIssue ::
  SignatureAuthResult ->
  Spec.OnIssueReq ->
  FlowHandler Spec.AckResponse
onIssue _ req = withFlowHandlerAPI $ do
  transaction_id <- req.onIssueReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received OnIssue request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    message_id <- req.onIssueReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
    onIssueReq <- ACL.buildOnIssueReq req
    Redis.whenWithLockRedis (onIssueLockKey message_id) 60 $ do
      issue <- DOnIssue.validateRequest onIssueReq
      fork "IGM on_issue processing" $ do
        Redis.whenWithLockRedis (onIssueProcessingLockKey message_id) 60 $
          DOnIssue.onIssue onIssueReq issue
  pure Utils.ack

onIssueLockKey :: Text -> Text
onIssueLockKey id = "IGM:OnIssue:MessageId-" <> id

onIssueProcessingLockKey :: Text -> Text
onIssueProcessingLockKey id = "IGM:OnIssue:Processing:MessageId-" <> id
