module IssueManagement.Beckn.ACL.IssueStatus where

import Data.Text as T
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import IGM.Utils (mkOrgName)
import qualified IGM.Utils as Utils
import qualified IssueManagement.Beckn.ACL.IGM.Utils as Utils
import qualified IssueManagement.Domain.Action.Beckn.Issue as DIssue
import qualified IssueManagement.Domain.Action.Beckn.IssueStatus as DIssueStatus
import Kernel.Prelude
import Kernel.Types.Error
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
  let issueDomain = res.domain
  context <- Utils.buildContext Spec.ON_ISSUE_STATUS issueDomain res.merchant.subscriberId.getShortId res.merchant txnId msgId res.merchantOperatingCity.city (Just $ Utils.BapData bapId bapUri) Nothing
  let message =
        if res.isValueAddNP
          then tfOnIssueStatusMessage res
          else tfOnIssueStatusMessageOffUs res
  pure $
    Spec.OnIssueStatusReq
      { onIssueStatusReqContext = context,
        onIssueStatusReqMessage = message,
        onIssueStatusReqError = Nothing
      }

-- OnUs: existing response
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
      issueStatus = DIssue.mapDomainStatusToSpecStatus res.issueStatus,
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
        { respondentActionCascadedLevel = Nothing,
          respondentActionRespondentAction = Just res.respondentAction,
          respondentActionShortDesc = Just $ "Issue registered and " <> toLower res.respondentAction,
          respondentActionUpdatedAt = Just res.updatedAt,
          respondentActionUpdatedBy = tfUpdatedBy res
        }
    ]

-- OffUs: spec-compliant response
tfOnIssueStatusMessageOffUs :: DIssueStatus.IssueStatusRes -> Maybe Spec.IssueReqMessage
tfOnIssueStatusMessageOffUs res =
  Just
    Spec.IssueReqMessage
      { issueReqMessageIssue = tfIssueOffUs res
      }

tfIssueOffUs :: DIssueStatus.IssueStatusRes -> Spec.Issue
tfIssueOffUs res =
  Spec.Issue
    { issueCategory = Nothing,
      issueComplainantInfo = Nothing,
      issueCreatedAt = res.createdAt,
      issueDescription = Nothing,
      issueExpectedResolutionTime = Nothing,
      issueExpectedResponseTime = Nothing,
      issueId = res.issueId.getId,
      issueIssueActions =
        Just
          Spec.IssueActions
            { issueActionsComplainantActions = Nothing,
              issueActionsRespondentActions =
                Just
                  [ Spec.RespondentAction
                      { respondentActionCascadedLevel = Just 1,
                        respondentActionRespondentAction = Just res.respondentAction,
                        respondentActionShortDesc = Just $ "Issue registered and " <> toLower res.respondentAction,
                        respondentActionUpdatedAt = Just res.updatedAt,
                        respondentActionUpdatedBy = tfUpdatedBy res
                      }
                  ]
            },
      issueIssueType = Nothing,
      issueOrderDetails = Nothing,
      issueResolution = tfIssueResolutionFromStored res,
      issueResolutionProvider = tfResolutionProvider res,
      issueSource = Nothing,
      issueStatus = Nothing,
      issueSubCategory = Nothing,
      issueUpdatedAt = res.updatedAt,
      issueRating = Nothing
    }

tfIssueResolution :: DIssueStatus.IssueStatusRes -> Maybe Spec.IssueResolution
tfIssueResolution _res =
  Just $
    Spec.IssueResolution
      { issueResolutionAction = Nothing,
        issueResolutionActionTriggered = show Spec.REFUND,
        issueResolutionGroRemarks = Nothing,
        issueResolutionLongDesc = Nothing,
        issueResolutionRefundAmount = Nothing,
        issueResolutionShortDesc = show Spec.REFUND
      }

tfIssueResolutionFromStored :: DIssueStatus.IssueStatusRes -> Maybe Spec.IssueResolution
tfIssueResolutionFromStored res =
  case res.resolutionActionTriggered of
    Just actionTriggered ->
      Just $
        Spec.IssueResolution
          { issueResolutionAction = Nothing,
            issueResolutionActionTriggered = actionTriggered,
            issueResolutionGroRemarks = Nothing,
            issueResolutionLongDesc = res.resolutionLongDesc,
            issueResolutionRefundAmount = res.resolutionRefundAmount,
            issueResolutionShortDesc = fromMaybe actionTriggered res.resolutionShortDesc
          }
    Nothing -> Nothing

tfResolutionProvider :: DIssueStatus.IssueStatusRes -> Maybe Spec.ResolutionProvider
tfResolutionProvider res =
  Just $
    Spec.ResolutionProvider
      { resolutionProviderRespondentInfo = tfRespondentInfo res
      }

tfRespondentInfo :: DIssueStatus.IssueStatusRes -> Spec.ResolutionProviderRespondentInfo
tfRespondentInfo res =
  Spec.ResolutionProviderRespondentInfo
    { resolutionProviderRespondentInfoOrganization = tfResolutionProviderOrg res,
      resolutionProviderRespondentInfoResolutionSupport = tfResolutionSupport res,
      resolutionProviderRespondentInfoType = Just $ show Spec.TRANSACTION_COUNTERPARTY_NP
    }

tfResolutionSupport :: DIssueStatus.IssueStatusRes -> Maybe Spec.ResolutionSupport
tfResolutionSupport res =
  Just $
    Spec.ResolutionSupport
      { resolutionSupportContact = tfGROContact res,
        resolutionSupportGros = tfSupportGros res
      }

tfSupportGros :: DIssueStatus.IssueStatusRes -> Maybe [Spec.GRO]
tfSupportGros res =
  Just
    [ Spec.GRO
        { gROContact = tfGROContact res,
          gROGroType = show Spec.TRANSACTION_COUNTERPARTY_NP_GRO,
          gROPerson = Just $ Spec.ComplainantPerson {complainantPersonName = Just res.groName}
        }
    ]

tfUpdatedBy :: DIssueStatus.IssueStatusRes -> Maybe Spec.Organization
tfUpdatedBy res =
  Just $
    Spec.Organization
      { organizationContact = Just $ Spec.GROContact {gROContactEmail = Just res.respondentEmail, gROContactPhone = Just res.respondentPhone},
        organizationOrg = tfOrganzationOrg res,
        organizationPerson = Just $ Spec.ComplainantPerson {complainantPersonName = Just res.respondentName}
      }

tfResolutionProviderOrg :: DIssueStatus.IssueStatusRes -> Maybe Spec.Organization
tfResolutionProviderOrg res =
  Just $
    Spec.Organization
      { organizationContact = Just $ Spec.GROContact {gROContactEmail = Just res.resolutionProviderEmail, gROContactPhone = Just res.resolutionProviderPhone},
        organizationOrg = tfOrganzationOrg res,
        organizationPerson = Just $ Spec.ComplainantPerson {complainantPersonName = Just res.resolutionProviderName}
      }

tfGROContact :: DIssueStatus.IssueStatusRes -> Maybe Spec.GROContact
tfGROContact res =
  Just $
    Spec.GROContact
      { gROContactEmail = Just res.groEmail,
        gROContactPhone = Just res.groPhone
      }

tfOrganzationOrg :: DIssueStatus.IssueStatusRes -> Maybe Spec.OrganizationOrg
tfOrganzationOrg res =
  Just $
    Spec.OrganizationOrg
      { organizationOrgName = mkOrgName res.merchant.subscriberId.getShortId res.domain
      }
