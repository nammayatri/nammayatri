{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.IGM.IssueStatus where

import qualified Beckn.ACL.IGM.IssueStatus as ACL
import qualified Beckn.ACL.IGM.OnIssueStatus as ACL
import qualified BecknV2.IGM.APIs as Spec
import qualified Domain.Action.Beckn.IGM.IssueStatus as DIssueStatus
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
    :> Spec.IssueStatusAPI

handler :: FlowServer API
handler = issueStatus

issueStatus ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Spec.IssueStatusReq ->
  FlowHandler Spec.AckResponse
issueStatus _ _ req = withFlowHandlerAPI $ do
  transaction_id <- req.issueStatusReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  message_id <- req.issueStatusReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bap_id <- req.issueStatusReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId not found")
  bap_uri <- req.issueStatusReqContext.contextBapUri & fromMaybeM (InvalidRequest "BapUri not found")
  logDebug $ "Received IssueStatus request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    issueStatusReq <- ACL.buildIssueStatusReq req
    Redis.whenWithLockRedis (issueStatusLockKey message_id) 60 $ do
      validatedIssueStatusReq <- DIssueStatus.validateRequest issueStatusReq
      fork "IGM issueStatus processing" $ do
        Redis.whenWithLockRedis (issueStatusProcessingLockKey message_id) 60 $ do
          dIssueStatusRes <- DIssueStatus.handler validatedIssueStatusReq
          becknOnIssueReq <- ACL.buildOnIssueStatusReq transaction_id message_id bap_id bap_uri dIssueStatusRes
          bapUrl <- parseBaseUrl bap_uri
          void $ CallBAP.callOnIssueStatus becknOnIssueReq bapUrl dIssueStatusRes.merchant
  pure Utils.ack

issueStatusLockKey :: Text -> Text
issueStatusLockKey message_id = "IGM:IssueStatus:MessageId" <> message_id

issueStatusProcessingLockKey :: Text -> Text
issueStatusProcessingLockKey id = "IGM:OnIssueStatus:Processing:MessageId-" <> id
