module IssueManagement.Beckn.ACL.Issue where

import Data.Aeson
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec hiding (IssueSubCategory)
import IGM.Utils (mkOrgName)
import qualified IGM.Utils as Utils
import qualified IssueManagement.Beckn.ACL.IGM.Utils as Utils
import IssueManagement.Domain.Action.Beckn.Issue (respondentActionToShortDesc)
import qualified IssueManagement.Domain.Action.Beckn.Issue as DIssue
import IssueManagement.Domain.Types.Issue.IGMIssue (mkStringToDomainType)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

buildIssueReq ::
  (MonadFlow m) =>
  Spec.IssueReq ->
  Bool ->
  m DIssue.DIssue
buildIssueReq req isValueAddNP = do
  Utils.validateContext Spec.ISSUE req.context
  let issue = req.issueReqMessage.issueReqMessageIssue
  issueCategory <- issue.issueCategory & fromMaybeM (InvalidRequest "IssueCategory not found")
  issueTypeText <- issue.issueIssueType & fromMaybeM (InvalidRequest "IssueType not found")
  issueStatusText <- issue.issueStatus & fromMaybeM (InvalidRequest "IssueStatus not found")
  bookingId <- issue.issueOrderDetails >>= (.orderDetailsId) & fromMaybeM (InvalidRequest "BookingId not found")
  let items = fromMaybe [] $ issue.issueOrderDetails >>= (.orderDetailsItems)
      fulfillments = fromMaybe [] $ issue.issueOrderDetails >>= (.orderDetailsFulfillments)
  when (length items > 1) $ throwError $ InvalidRequest "Multiple order items not supported for mobility"
  when (length fulfillments > 1) $ throwError $ InvalidRequest "Multiple order fulfillments not supported for mobility"
  whenJust (issue.issueSource >>= (.issueSourceType)) $ \srcType ->
    when (isNothing (decode @Spec.IssueSourceType (encode srcType))) $
      throwError $ InvalidRequest $ "Invalid source type: " <> srcType
  bapId <- req.context.contextBapId & fromMaybeM (InvalidRequest "BapId not found")
  let issueRaisedBy = issue.issueIssueActions >>= (.issueActionsComplainantActions) >>= listToMaybe >>= (.complainantActionUpdatedBy) >>= (.organizationOrg) >>= (.organizationOrgName)
      issueId = issue.issueId
      createdAt = issue.issueCreatedAt
  issueSubCategory <- maybe (throwError $ InvalidRequest "Invalid IssueSubCategory") pure $ decode $ encode issue.issueSubCategory

  let (customerName, customerEmail, customerPhone) =
        if isValueAddNP
          then extractCustomerFromUpdatedBy issue
          else extractCustomerFromComplainantInfo issue

  let domain = case req.context.contextDomain of
        Just d -> mkStringToDomainType d
        Nothing -> Spec.PUBLIC_TRANSPORT

  let offUsDetails =
        if isValueAddNP
          then DIssue.emptyOffUsDetails
          else buildOffUsDetails issue req.context

  pure $
    DIssue.DIssue
      { customerEmail = customerEmail,
        customerPhone = customerPhone,
        customerName = customerName,
        offUsDetails = offUsDetails,
        domain = domain,
        isValueAddNP = isValueAddNP,
        ..
      }

extractCustomerFromUpdatedBy :: Spec.Issue -> (Maybe Text, Maybe Text, Maybe Text)
extractCustomerFromUpdatedBy issue =
  let updatedByContact = issue.issueIssueActions >>= (.issueActionsComplainantActions) >>= listToMaybe >>= (.complainantActionUpdatedBy) >>= (.organizationContact)
      updatedByName = issue.issueIssueActions >>= (.issueActionsComplainantActions) >>= listToMaybe >>= (.complainantActionUpdatedBy) >>= (.organizationPerson) >>= (.complainantPersonName)
   in (updatedByName, updatedByContact >>= (.gROContactEmail), updatedByContact >>= (.gROContactPhone))

extractCustomerFromComplainantInfo :: Spec.Issue -> (Maybe Text, Maybe Text, Maybe Text)
extractCustomerFromComplainantInfo issue =
  ( issue.issueComplainantInfo >>= (.complainantPerson) >>= (.complainantPersonName),
    issue.issueComplainantInfo >>= (.complainantContact) >>= (.contactEmail),
    issue.issueComplainantInfo >>= (.complainantContact) >>= (.contactPhone)
  )

buildOffUsDetails :: Spec.Issue -> Spec.Context -> DIssue.OffUsIssueDetails
buildOffUsDetails issue ctx =
  DIssue.OffUsIssueDetails
    { descShort = issue.issueDescription >>= (.issueDescriptionShortDesc),
      descLong = issue.issueDescription >>= (.issueDescriptionLongDesc),
      orderState = issue.issueOrderDetails >>= (.orderDetailsState),
      orderProviderId = issue.issueOrderDetails >>= (.orderDetailsProviderId),
      orderMerchantOrderId = issue.issueOrderDetails >>= (.orderMerchantId),
      orderItemId = issue.issueOrderDetails >>= (.orderDetailsItems) >>= listToMaybe >>= (.itemId),
      orderItemQuantity = issue.issueOrderDetails >>= (.orderDetailsItems) >>= listToMaybe >>= (.itemQuantity),
      orderFulfillmentId = issue.issueOrderDetails >>= (.orderDetailsFulfillments) >>= listToMaybe >>= (.fulfillmentId),
      orderFulfillmentState = issue.issueOrderDetails >>= (.orderDetailsFulfillments) >>= listToMaybe >>= (.fulfillmentState),
      igmCategory = issue.issueCategory,
      igmSubCategory = issue.issueSubCategory,
      sourceType = issue.issueSource >>= (.issueSourceType),
      sourceNpId = issue.issueSource >>= (.issueSourceNetworkParticipantId),
      contextTxnId = ctx.contextTransactionId,
      contextDomain = ctx.contextDomain,
      expectedResponseTime = issue.issueExpectedResponseTime >>= (.issueExpectedResolutionTimeDuration),
      expectedResolutionTime = issue.issueExpectedResolutionTime >>= (.issueExpectedResolutionTimeDuration),
      issueUpdatedAt = Just $ convertRFC3339ToUTC issue.issueUpdatedAt,
      complainantActions = fromMaybe [] $ issue.issueIssueActions >>= (.issueActionsComplainantActions)
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
  let issueDomain = issueRes.domain
  context <- Utils.buildContext Spec.ON_ISSUE issueDomain issueRes.bppId issueRes.merchant txnId msgId issueRes.merchantOperatingCity.city (Just $ Utils.BapData bapId bapUri) Nothing
  let message =
        if issueRes.isValueAddNP
          then tfOnIssueMessage issueRes
          else tfOnIssueMessageOffUs issueRes
  pure $
    Spec.OnIssueReq
      { onIssueReqContext = context,
        onIssueReqMessage = message,
        onIssueReqError = Nothing
      }

-- OnUs: existing sparse response
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
        { respondentActionCascadedLevel = Nothing,
          respondentActionRespondentAction = Just $ show Spec.PROCESSING,
          respondentActionShortDesc = Just "Issue registered",
          respondentActionUpdatedAt = Just issueRes.updatedAt,
          respondentActionUpdatedBy = tfUpdatedBy issueRes
        }
    ]

-- OffUs: spec-compliant response with cascaded_level and resolution_provider
tfOnIssueMessageOffUs :: DIssue.IssueRes -> Maybe Spec.OnIssueReqMessage
tfOnIssueMessageOffUs issueRes =
  Just
    Spec.OnIssueReqMessage
      { onIssueReqMessageIssue = tfIssueOffUs issueRes
      }

tfIssueOffUs :: DIssue.IssueRes -> Spec.Issue
tfIssueOffUs issueRes =
  Spec.Issue
    { issueCategory = Nothing,
      issueComplainantInfo = Nothing,
      issueCreatedAt = issueRes.createdAt,
      issueDescription = Nothing,
      issueExpectedResolutionTime = Nothing,
      issueExpectedResponseTime = Nothing,
      issueId = issueRes.issueId,
      issueIssueActions = tfIssueActionsOffUs issueRes,
      issueIssueType = Nothing,
      issueOrderDetails = Nothing,
      issueResolution = Nothing,
      issueResolutionProvider = Nothing,
      issueSource = Nothing,
      issueStatus = Nothing,
      issueSubCategory = Nothing,
      issueUpdatedAt = issueRes.updatedAt,
      issueRating = Nothing
    }

tfIssueActionsOffUs :: DIssue.IssueRes -> Maybe Spec.IssueActions
tfIssueActionsOffUs issueRes =
  Just $
    Spec.IssueActions
      { issueActionsComplainantActions = Nothing,
        issueActionsRespondentActions =
          Just
            [ Spec.RespondentAction
                { respondentActionCascadedLevel = Just 1,
                  respondentActionRespondentAction = Just issueRes.respondentAction,
                  respondentActionShortDesc = Just $ respondentActionToShortDesc issueRes.respondentAction,
                  respondentActionUpdatedAt = Just issueRes.updatedAt,
                  respondentActionUpdatedBy = tfUpdatedBy issueRes
                }
            ]
      }

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
      { gROContactEmail = Just issueRes.respondentEmail,
        gROContactPhone = Just issueRes.respondentPhone
      }

tfOrganzationOrg :: DIssue.IssueRes -> Maybe Spec.OrganizationOrg
tfOrganzationOrg issueRes =
  Just $
    Spec.OrganizationOrg
      { organizationOrgName = mkOrgName issueRes.bppId issueRes.domain
      }

tfOrganizationPerson :: DIssue.IssueRes -> Maybe Spec.ComplainantPerson
tfOrganizationPerson issueRes =
  Just $
    Spec.ComplainantPerson
      { complainantPersonName = Just issueRes.respondentName
      }
