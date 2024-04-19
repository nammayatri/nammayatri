{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.IGM.Issue where

import qualified Beckn.ACL.IGM.Issue as ACL
import qualified BecknV2.IGM.APIs as Spec
import qualified Domain.Action.Beckn.IGM.Issue as DIssue
import Environment
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.IssueAPI

handler :: SignatureAuthResult -> FlowServer API
handler = issue

issue ::
  SignatureAuthResult ->
  Spec.IssueReq ->
  FlowHandler Spec.AckResponse
issue _ req = withFlowHandlerAPI $ do
  transaction_id <- req.context.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received Issue request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    message_id <- req.context.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
    issueReq <- ACL.buildIssueReq req
    Redis.whenWithLockRedis (issueLockKey message_id) 60 $ do
      validatedIssueReq <- DIssue.validateRequest issueReq
      fork "IGM issue processing" $ do
        Redis.whenWithLockRedis (issueProcessingLockKey message_id) 60 $
          DIssue.issue validatedIssueReq
  pure Utils.ack

issueLockKey :: Text -> Text
issueLockKey message_id = "IGM:Issue:MessageId" <> message_id

issueProcessingLockKey :: Text -> Text
issueProcessingLockKey id = "IGM:OnIssue:Processing:MessageId-" <> id
