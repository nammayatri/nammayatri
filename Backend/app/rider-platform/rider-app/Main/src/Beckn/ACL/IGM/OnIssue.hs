{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.IGM.OnIssue where

import qualified Domain.Action.Beckn.IGM.OnIssue as DOnIssue
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildOnIssueReq ::
  MonadFlow m =>
  Spec.OnIssueReq ->
  m (DOnIssue.DOnIssue)
buildOnIssueReq req = do
  let context = req.onIssueReqContext
  Utils.validateContext Spec.ON_ISSUE context
  _transactionId <- context.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  _messageId <- context.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bppSubscriberId <- context.contextBppId & fromMaybeM (InvalidRequest "BppSubscriberId not found")
  bppSubscriberUrl <- context.contextBppUri & fromMaybeM (InvalidRequest "BppSubscriberUrl not found")

  message <- req.onIssueReqMessage & fromMaybeM (InvalidRequest "Message not found") -- shrey00 : errorHandler
  let issue = message.onIssueReqMessageIssue
  respondentAction <- issue.issueIssueActions >>= (.issueActionsRespondentActions) >>= listToMaybe & fromMaybeM (InvalidRequest "RespondentActions Missing")
  respondentInfo <- respondentAction.respondentActionUpdatedBy & fromMaybeM (InvalidRequest "RespondentActionUpdatedBy Missing")
  let respondentContact = respondentInfo.organizationContact
      respondentPerson = respondentInfo.organizationPerson
  pure $
    DOnIssue.DOnIssue
      { id = issue.issueId,
        providerId = bppSubscriberId,
        providerUrl = bppSubscriberUrl,
        respondentName = respondentPerson >>= (.complainantPersonName),
        respondentEmail = respondentContact >>= (.gROContactEmail),
        respondentPhone = respondentContact >>= (.gROContactPhone),
        respondentAction = respondentAction.respondentActionRespondentAction,
        updatedAt = respondentAction.respondentActionUpdatedAt
      }
