{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.IGM.OnIssueStatus where

import qualified Domain.Action.Beckn.IGM.OnIssueStatus as DOnIssueStatus
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildOnIssueStatusReq ::
  MonadFlow m =>
  Spec.OnIssueStatusReq ->
  m DOnIssueStatus.DOnIssueStatus
buildOnIssueStatusReq req = do
  let context = req.onIssueStatusReqContext
  Utils.validateContext Spec.ON_ISSUE_STATUS context
  _transactionId <- context.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  _messageId <- context.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bppSubscriberId <- context.contextBppId & fromMaybeM (InvalidRequest "BppSubscriberId not found")
  _bppSubscriberUrl <- context.contextBppUri & fromMaybeM (InvalidRequest "BppSubscriberUrl not found")
  message <- req.onIssueStatusReqMessage & fromMaybeM (InvalidRequest "Message not found")
  let issue = message.issueReqMessageIssue
      mbResolution = issue.issueResolution
      gro = issue.issueResolutionProvider >>= (.resolutionProviderRespondentInfo.resolutionProviderRespondentInfoResolutionSupport) >>= (.resolutionSupportGros) >>= listToMaybe
      groContact = gro >>= (.gROContact)
      groPerson = gro >>= (.gROPerson)
  pure $
    DOnIssueStatus.DOnIssueStatus
      { id = issue.issueId,
        providerId = bppSubscriberId,
        respondentName = groPerson >>= (.complainantPersonName),
        respondentEmail = groContact >>= (.gROContactEmail),
        respondentPhone = groContact >>= (.gROContactPhone),
        respondentAction = mbResolution >>= (.issueResolutionAction),
        resolutionActionTriggered = mbResolution <&> (.issueResolutionActionTriggered),
        updatedAt = issue.issueUpdatedAt
      }
