{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.IGM.OnIssue (buildOnIssueReq) where

import qualified Beckn.ACL.IGM.Utils as Utils
import qualified Domain.Action.Beckn.IGM.Issue as DIssue
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import Kernel.Prelude
import Kernel.Utils.Common

buildOnIssueReq ::
  ( MonadFlow m,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Text ->
  Text ->
  Text ->
  Text ->
  DIssue.IssueRes ->
  m Spec.OnIssueReq
buildOnIssueReq txnId msgId bapId bapUri issueRes = do
  context <- Utils.buildContext Spec.ON_ISSUE Spec.ON_DEMAND bapId issueRes.merchant' txnId msgId issueRes.merchantOperatingCity.city (Just $ Utils.BapData bapId bapUri) (Utils.buildTTL 30 issueRes.updatedAt)
  let message = tfOnIssueMessage issueRes
  pure $
    Spec.OnIssueReq
      { onIssueReqContext = context,
        onIssueReqMessage = message,
        onIssueReqError = Nothing
      }

tfOnIssueMessage :: DIssue.IssueRes -> Maybe Spec.OnIssueReqMessage
tfOnIssueMessage issueRes =
  Just
    Spec.OnIssueReqMessage
      { onIssueReqMessageIssue = tfIssue issueRes
      }

tfIssue :: DIssue.IssueRes -> Spec.Issue
tfIssue issueRes =
  Spec.Issue
    { issueCategory = Nothing,
      issueComplainantInfo = Nothing,
      issueCreatedAt = issueRes.createdAt,
      issueDescription = Nothing,
      issueExpectedResolutionTime = Nothing,
      issueExpectedResponseTime = Nothing,
      issueId = issueRes.issueId,
      issueIssueActions = tfIssueActions issueRes,
      issueIssueType = Nothing,
      issueOrderDetails = Nothing,
      issueResolution = Nothing,
      issueResolutionProvider = Nothing,
      issueSource = Nothing,
      issueStatus = DIssue.mapDomainStatusToSpecStatus issueRes.issueStatus,
      issueSubCategory = Nothing,
      issueUpdatedAt = issueRes.updatedAt,
      issueRating = Nothing
    }

tfIssueActions :: DIssue.IssueRes -> Maybe Spec.IssueActions
tfIssueActions issueRes =
  Just $
    Spec.IssueActions
      { issueActionsComplainantActions = Nothing,
        issueActionsRespondentActions = tfRespondentActions issueRes
      }

tfRespondentActions :: DIssue.IssueRes -> Maybe [Spec.RespondentAction]
tfRespondentActions issueRes =
  Just
    [ Spec.RespondentAction
        { respondentActionRespondentAction = Just $ show Spec.PROCESSING,
          respondentActionShortDesc = Nothing,
          respondentActionUpdatedAt = Just issueRes.updatedAt,
          respondentActionUpdatedBy = tfUpdatedBy issueRes
        }
    ]

tfUpdatedBy :: DIssue.IssueRes -> Maybe Spec.Organization
tfUpdatedBy issueRes =
  Just $
    Spec.Organization
      { organizationContact = tfOrganizationContact issueRes,
        organizationOrg = tfOrganzationOrg issueRes,
        organizationPerson = tfOrganizationPerson issueRes
      }

tfOrganizationContact :: DIssue.IssueRes -> Maybe Spec.GROContact
tfOrganizationContact issueRes =
  Just $
    Spec.GROContact
      { gROContactEmail = Just issueRes.groEmail,
        gROContactPhone = Just issueRes.groPhone
      }

tfOrganzationOrg :: DIssue.IssueRes -> Maybe Spec.OrganizationOrg
tfOrganzationOrg issueRes =
  Just $
    Spec.OrganizationOrg
      { organizationOrgName = Just issueRes.merchant'.name
      }

tfOrganizationPerson :: DIssue.IssueRes -> Maybe Spec.ComplainantPerson
tfOrganizationPerson issueRes =
  Just $
    Spec.ComplainantPerson
      { complainantPersonName = Just issueRes.groName
      }
