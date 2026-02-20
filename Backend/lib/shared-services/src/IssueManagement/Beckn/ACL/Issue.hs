module IssueManagement.Beckn.ACL.Issue where

import Control.Lens ((^?), _head)
import Data.Aeson
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec hiding (IssueSubCategory)
import IGM.Utils (mkOrgName)
import qualified IGM.Utils as Utils
import qualified IssueManagement.Beckn.ACL.IGM.Utils as Utils
import qualified IssueManagement.Domain.Action.Beckn.Issue as DIssue
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

buildIssueReq ::
  (MonadFlow m) =>
  Spec.IssueReq ->
  m DIssue.DIssue
buildIssueReq req = do
  Utils.validateContext Spec.ISSUE req.context
  issueCategory <- req.issueReqMessage.issueReqMessageIssue.issueCategory & fromMaybeM (InvalidRequest "IssueCategory not found")
  issueTypeText <- req.issueReqMessage.issueReqMessageIssue.issueIssueType & fromMaybeM (InvalidRequest "IssueType not found")
  issueStatusText <- req.issueReqMessage.issueReqMessageIssue.issueStatus & fromMaybeM (InvalidRequest "IssueStatus not found")
  bookingId <- req.issueReqMessage.issueReqMessageIssue.issueOrderDetails >>= (.orderDetailsId) & fromMaybeM (InvalidRequest "BookingId not found")
  bapId <- req.context.contextBapId & fromMaybeM (InvalidRequest "BapId not found")
  let issueRaisedBy = req.issueReqMessage.issueReqMessageIssue.issueIssueActions >>= (.issueActionsComplainantActions) >>= (^? _head) >>= (.complainantActionUpdatedBy) >>= (.organizationOrg) >>= (.organizationOrgName)
      issueId = req.issueReqMessage.issueReqMessageIssue.issueId
      customerContact = req.issueReqMessage.issueReqMessageIssue.issueIssueActions >>= (.issueActionsComplainantActions) >>= (^? _head) >>= (.complainantActionUpdatedBy) >>= (.organizationContact)
      customerName = req.issueReqMessage.issueReqMessageIssue.issueIssueActions >>= (.issueActionsComplainantActions) >>= (^? _head) >>= (.complainantActionUpdatedBy) >>= (.organizationPerson) >>= (.complainantPersonName)
      createdAt = req.issueReqMessage.issueReqMessageIssue.issueCreatedAt
  issueSubCategory <- maybe (throwError $ InvalidRequest "Invalid IssueSubCategory") pure $ decode $ encode req.issueReqMessage.issueReqMessageIssue.issueSubCategory
  pure $
    DIssue.DIssue
      { customerEmail = customerContact >>= (.gROContactEmail),
        customerPhone = customerContact >>= (.gROContactPhone),
        ..
      }

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
  context <- Utils.buildContext Spec.ON_ISSUE Spec.PUBLIC_TRANSPORT issueRes.bppId issueRes.merchant txnId msgId issueRes.merchantOperatingCity.city (Just $ Utils.BapData bapId bapUri) (Utils.buildTTL 30 (convertRFC3339ToUTC issueRes.updatedAt))
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
          respondentActionShortDesc = Just "Issue registered",
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
      { organizationOrgName = mkOrgName issueRes.bppId Spec.PUBLIC_TRANSPORT
      }

tfOrganizationPerson :: DIssue.IssueRes -> Maybe Spec.ComplainantPerson
tfOrganizationPerson issueRes =
  Just $
    Spec.ComplainantPerson
      { complainantPersonName = Just issueRes.groName
      }
