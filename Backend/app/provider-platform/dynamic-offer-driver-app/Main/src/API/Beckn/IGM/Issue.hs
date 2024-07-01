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
import qualified Beckn.ACL.IGM.OnIssue as ACL
import qualified BecknV2.IGM.APIs as Spec
import qualified Domain.Action.Beckn.IGM.Issue as DIssue
import qualified Domain.Types.Merchant as DM
import Environment
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified SharedLogic.CallIGMBAP as CallBAP

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Spec.IssueAPI

handler :: FlowServer API
handler = issue

issue ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Spec.IssueReq ->
  FlowHandler Spec.AckResponse
issue merchantId _ req = withFlowHandlerAPI $ do
  transaction_id <- req.context.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  message_id <- req.context.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bap_id <- req.context.contextBapId & fromMaybeM (InvalidRequest "BapId not found")
  bap_uri <- req.context.contextBapUri & fromMaybeM (InvalidRequest "BapUri not found")
  logDebug $ "Received Issue request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    issueReq <- ACL.buildIssueReq req
    Redis.whenWithLockRedis (issueLockKey message_id) 60 $ do
      validatedIssueReq <- DIssue.validateRequest merchantId issueReq
      fork "IGM issue processing" $ do
        Redis.whenWithLockRedis (issueProcessingLockKey message_id) 60 $ do
          dIssueRes <- DIssue.handler validatedIssueReq
          becknOnIssueReq <- ACL.buildOnIssueReq transaction_id message_id bap_id bap_uri dIssueRes
          bapUrl <- parseBaseUrl bap_uri
          void $ CallBAP.callOnIssue becknOnIssueReq bapUrl dIssueRes.merchant
  pure Utils.ack

issueLockKey :: Text -> Text
issueLockKey message_id = "IGM:Issue:MessageId" <> message_id

issueProcessingLockKey :: Text -> Text
issueProcessingLockKey id = "IGM:OnIssue:Processing:MessageId-" <> id
