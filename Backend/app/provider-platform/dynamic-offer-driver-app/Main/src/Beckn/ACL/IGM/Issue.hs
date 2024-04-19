{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.IGM.Issue (buildIssueReq) where

import qualified Domain.Action.Beckn.IGM.Issue as DIssue
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildIssueReq ::
  (MonadFlow m) =>
  Spec.IssueReq ->
  m DIssue.DIssue
buildIssueReq req = do
  Utils.validateContext Spec.ISSUE req.context
  issueCategory <- req.issueReqMessage.issueReqMessageIssue.issueCategory & fromMaybeM (InvalidRequest "IssueCategory not found")
  issueType <- req.issueReqMessage.issueReqMessageIssue.issueIssueType & fromMaybeM (InvalidRequest "IssueType not found")
  issueStatus <- req.issueReqMessage.issueReqMessageIssue.issueStatus & fromMaybeM (InvalidRequest "IssueStatus not found")
  bookingId <- req.issueReqMessage.issueReqMessageIssue.issueOrderDetails >>= (.orderDetailsFulfillments) >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "BookingId not found")
  bapId <- req.context.contextBapId & fromMaybeM (InvalidRequest "BapId not found")
  let issueRaisedBy = req.issueReqMessage.issueReqMessageIssue.issueIssueActions >>= (.issueActionsComplainantActions) >>= listToMaybe >>= (.complainantActionUpdatedBy) >>= (.organizationOrg) >>= (.organizationOrgName)
      issueId = req.issueReqMessage.issueReqMessageIssue.issueId
      customerContact = req.issueReqMessage.issueReqMessageIssue.issueIssueActions >>= (.issueActionsComplainantActions) >>= listToMaybe >>= (.complainantActionUpdatedBy) >>= (.organizationContact)
      customerName = req.issueReqMessage.issueReqMessageIssue.issueIssueActions >>= (.issueActionsComplainantActions) >>= listToMaybe >>= (.complainantActionUpdatedBy) >>= (.organizationPerson) >>= (.complainantPersonName)
  pure $
    DIssue.DIssue
      { customerEmail = customerContact >>= (.gROContactEmail),
        customerPhone = customerContact >>= (.gROContactPhone),
        ..
      }
