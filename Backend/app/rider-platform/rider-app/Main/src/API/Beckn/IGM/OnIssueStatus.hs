{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.IGM.OnIssueStatus where

import qualified Beckn.ACL.IGM.OnIssueStatus as ACL
import qualified BecknV2.IGM.APIs as Spec
import qualified Domain.Action.Beckn.IGM.OnIssueStatus as DOnIssueStatus
import Environment
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.OnIssueStatusAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onIssueStatus

onIssueStatus ::
  SignatureAuthResult ->
  Spec.OnIssueStatusReq ->
  FlowHandler Spec.AckResponse
onIssueStatus _ req = withFlowHandlerAPI $ do
  transaction_id <- req.onIssueStatusReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received OnIssueStatus request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    message_id <- req.onIssueStatusReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
    onIssueStatusReq <- ACL.buildOnIssueStatusReq req
    Redis.whenWithLockRedis (onIssueStatusLockKey message_id) 60 $ do
      issue <- DOnIssueStatus.validateRequest onIssueStatusReq
      fork "IGM on_issue processing" $ do
        Redis.whenWithLockRedis (onIssueStatusProcessingLockKey message_id) 60 $
          DOnIssueStatus.onIssueStatus onIssueStatusReq issue -- shrey00 : implement this function
  pure Utils.ack

onIssueStatusLockKey :: Text -> Text
onIssueStatusLockKey id = "IGM:OnIssueStatus:MessageId-" <> id

onIssueStatusProcessingLockKey :: Text -> Text
onIssueStatusProcessingLockKey id = "IGM:OnIssueStatus:Processing:MessageId-" <> id
