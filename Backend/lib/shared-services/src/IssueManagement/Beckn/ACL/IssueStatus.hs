module IssueManagement.Beckn.ACL.IssueStatus where

import Data.Text as T
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import IGM.Utils (mkOrgName)
import qualified IGM.Utils as Utils
import qualified IssueManagement.Beckn.ACL.IGM.Utils as Utils
import qualified IssueManagement.Domain.Action.Beckn.IssueStatus as DIssueStatus
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

buildIssueStatusReq ::
  (MonadFlow m) =>
  Spec.IssueStatusReq ->
  m DIssueStatus.DIssueStatus
buildIssueStatusReq req = do
  Utils.validateContext Spec.ISSUE_STATUS req.issueStatusReqContext
  let issueId = req.issueStatusReqMessage.issueStatusReqMessageIssueId
  bapId <- req.issueStatusReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId not found")
  pure $
    DIssueStatus.DIssueStatus
      { issueId = issueId,
        ..
      }

buildOnIssueStatusReq ::
  ( MonadFlow m,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Text ->
  Text ->
  Text ->
  Text ->
  DIssueStatus.IssueStatusRes ->
  m Spec.OnIssueStatusReq
buildOnIssueStatusReq txnId msgId bapId bapUri res = do
  context <- Utils.buildContext Spec.ON_ISSUE_STATUS Spec.PUBLIC_TRANSPORT res.merchant.subscriberId.getShortId res.merchant txnId msgId res.merchantOperatingCity.city (Just $ Utils.BapData bapId bapUri) (Utils.buildTTL 30 (convertRFC3339ToUTC res.updatedAt))
  let message = tfOnIssueStatusMessage res
  pure $
    Spec.OnIssueStatusReq
      { onIssueStatusReqContext = context,
        onIssueStatusReqMessage = message,
        onIssueStatusReqError = Nothing
      }

tfOnIssueStatusMessage :: DIssueStatus.IssueStatusRes -> Maybe Spec.IssueReqMessage
tfOnIssueStatusMessage res =
  Just
    Spec.IssueReqMessage
      { issueReqMessageIssue = tfIssue res
      }

tfIssue :: DIssueStatus.IssueStatusRes -> Spec.Issue
tfIssue res =
  Spec.Issue
    { issueCategory = Nothing,
      issueComplainantInfo = Nothing,
      issueCreatedAt = res.createdAt,
      issueDescription = Nothing,
      issueExpectedResolutionTime = Nothing,
      issueExpectedResponseTime = Nothing,
      issueId = res.issueId.getId,
      issueIssueActions = tfIssueActions res,
      issueIssueType = Nothing,
      issueOrderDetails = Nothing,
      issueResolution = tfIssueResolution res,
      issueResolutionProvider = tfResolutionProvider res,
      issueSource = Nothing,
      issueStatus = Nothing,
      issueSubCategory = Nothing,
      issueUpdatedAt = res.updatedAt,
      issueRating = Nothing
    }

tfIssueActions :: DIssueStatus.IssueStatusRes -> Maybe Spec.IssueActions
tfIssueActions res =
  Just
    Spec.IssueActions
      { issueActionsComplainantActions = Nothing,
        issueActionsRespondentActions = tfRespondentActions res
      }

tfRespondentActions :: DIssueStatus.IssueStatusRes -> Maybe [Spec.RespondentAction]
tfRespondentActions res =
  Just
    [ Spec.RespondentAction
        { respondentActionRespondentAction = Just res.respondentAction,
          respondentActionShortDesc = Just $ "Issue registered and " <> toLower res.respondentAction,
          respondentActionUpdatedAt = Just res.updatedAt,
          respondentActionUpdatedBy = tfUpdatedBy res
        }
    ]

tfUpdatedBy :: DIssueStatus.IssueStatusRes -> Maybe Spec.Organization
tfUpdatedBy res =
  Just $
    Spec.Organization
      { organizationContact = tfOrganizationContact res,
        organizationOrg = tfOrganzationOrg res,
        organizationPerson = tfOrganizationPerson res
      }

tfOrganizationContact :: DIssueStatus.IssueStatusRes -> Maybe Spec.GROContact
tfOrganizationContact res =
  Just $
    Spec.GROContact
      { gROContactEmail = Just res.groEmail,
        gROContactPhone = Just res.groPhone
      }

tfOrganzationOrg :: DIssueStatus.IssueStatusRes -> Maybe Spec.OrganizationOrg
tfOrganzationOrg res =
  Just $
    Spec.OrganizationOrg
      { organizationOrgName = mkOrgName res.merchant.subscriberId.getShortId Spec.PUBLIC_TRANSPORT
      }

tfOrganizationPerson :: DIssueStatus.IssueStatusRes -> Maybe Spec.ComplainantPerson
tfOrganizationPerson res =
  Just $
    Spec.ComplainantPerson
      { complainantPersonName = Just res.groName
      }

tfIssueResolution :: DIssueStatus.IssueStatusRes -> Maybe Spec.IssueResolution
tfIssueResolution _res =
  Just $
    Spec.IssueResolution
      { issueResolutionAction = Nothing,
        issueResolutionActionTriggered = show Spec.REFUND,
        issueResolutionGroRemarks = Nothing,
        issueResolutionLongDesc = Nothing,
        issueResolutionShortDesc = show Spec.REFUND
      }

tfResolutionProvider :: DIssueStatus.IssueStatusRes -> Maybe Spec.ResolutionProvider
tfResolutionProvider res =
  Just $
    Spec.ResolutionProvider
      { resolutionProviderRespondentInfo = tfRespondentInfo res
      }

tfRespondentInfo :: DIssueStatus.IssueStatusRes -> Spec.ResolutionProviderRespondentInfo
tfRespondentInfo res =
  Spec.ResolutionProviderRespondentInfo
    { resolutionProviderRespondentInfoOrganization = tfUpdatedBy res,
      resolutionProviderRespondentInfoResolutionSupport = tfResolutionSupport res,
      resolutionProviderRespondentInfoType = Just $ show Spec.TRANSACTION_COUNTERPARTY_NP
    }

tfResolutionSupport :: DIssueStatus.IssueStatusRes -> Maybe Spec.ResolutionSupport
tfResolutionSupport res =
  Just $
    Spec.ResolutionSupport
      { resolutionSupportContact = tfOrganizationContact res,
        resolutionSupportGros = tfSupportGros res
      }

tfSupportGros :: DIssueStatus.IssueStatusRes -> Maybe [Spec.GRO]
tfSupportGros res =
  Just
    [ Spec.GRO
        { gROContact = tfOrganizationContact res,
          gROGroType = show Spec.TRANSACTION_COUNTERPARTY_NP,
          gROPerson = tfOrganizationPerson res
        }
    ]
