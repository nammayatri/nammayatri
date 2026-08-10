{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module IssueManagement.Domain.Action.Beckn.Issue where

import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec hiding (IssueSubCategory)
import IGM.Utils (mkOrgName)
import IssueManagement.Common
import qualified IssueManagement.Common.UI.Issue as Common
import IssueManagement.Domain.Action.UI.Issue
import qualified IssueManagement.Domain.Action.UI.Issue as Common
import IssueManagement.Domain.Types.Issue.IGMConfig
import qualified IssueManagement.Domain.Types.Issue.IGMIssue as DIGM
import qualified IssueManagement.Domain.Types.Issue.IGMIssueAction as DIGA
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IGMConfig as QIGMConfig
import qualified IssueManagement.Storage.Queries.Issue.IGMIssue as QIGM
import qualified IssueManagement.Storage.Queries.Issue.IGMIssueAction as QIGA
import qualified IssueManagement.Storage.Queries.Issue.IssueCategory as QIC
import qualified IssueManagement.Storage.Queries.Issue.IssueOption as QIO
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIR
import qualified Kernel.External.Ticket.Interface.Types as TIT
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common
import Slack.Types (SlackNotificationConfig)

data OffUsIssueDetails = OffUsIssueDetails
  { descShort :: Maybe Text,
    descLong :: Maybe Text,
    orderState :: Maybe Text,
    orderProviderId :: Maybe Text,
    orderMerchantOrderId :: Maybe Text,
    orderItemId :: Maybe Text,
    orderItemQuantity :: Maybe Double,
    orderFulfillmentId :: Maybe Text,
    orderFulfillmentState :: Maybe Text,
    igmCategory :: Maybe Text,
    igmSubCategory :: Maybe Text,
    sourceType :: Maybe Text,
    sourceNpId :: Maybe Text,
    contextTxnId :: Maybe Text,
    contextDomain :: Maybe Text,
    expectedResponseTime :: Maybe Text,
    expectedResolutionTime :: Maybe Text,
    issueUpdatedAt :: Maybe UTCTime,
    complainantActions :: [Spec.ComplainantAction]
  }
  deriving (Show, Generic)

emptyOffUsDetails :: OffUsIssueDetails
emptyOffUsDetails =
  OffUsIssueDetails Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

data DIssue = DIssue
  { issueId :: Text,
    issueCategory :: Text,
    issueSubCategory :: Maybe Spec.IssueSubCategory,
    issueTypeText :: Text,
    issueStatusText :: Text,
    bookingId :: Text,
    issueRaisedBy :: Maybe Text,
    customerName :: Maybe Text,
    customerEmail :: Maybe Text,
    customerPhone :: Maybe Text,
    createdAt :: UTCTimeRFC3339,
    bapId :: Text,
    domain :: Spec.Domain,
    offUsDetails :: OffUsIssueDetails,
    isValueAddNP :: Bool
  }
  deriving (Show, Generic)

data IssueRes = IssueRes
  { issueId :: Text,
    respondentAction :: Text,
    groName :: Text,
    groPhone :: Text,
    groEmail :: Text,
    respondentName :: Text,
    respondentPhone :: Text,
    respondentEmail :: Text,
    resolutionProviderName :: Text,
    resolutionProviderPhone :: Text,
    resolutionProviderEmail :: Text,
    createdAt :: UTCTimeRFC3339,
    updatedAt :: UTCTimeRFC3339,
    merchant :: Merchant,
    merchantOperatingCity :: MerchantOperatingCity,
    issueStatus :: DIGM.Status,
    bapId :: Text,
    bppId :: Text,
    domain :: Spec.Domain,
    isValueAddNP :: Bool
  }

data ValidatedDIssue = ValidatedDIssue
  { issueId :: Text,
    issueCategory :: Text,
    issueSubCategory :: Maybe Spec.IssueSubCategory,
    issueType :: DIGM.IssueType,
    issueStatus :: DIGM.Status,
    booking :: Booking,
    issueRaisedBy :: Maybe Text,
    customerName :: Maybe Text,
    customerEmail :: Maybe Text,
    customerPhone :: Maybe Text,
    bapId :: Text,
    igmConfig :: IGMConfig,
    createdAt :: UTCTimeRFC3339,
    merchantOperatingCity :: MerchantOperatingCity,
    merchant :: Merchant,
    bppId :: Text,
    domain :: Spec.Domain,
    offUsDetails :: OffUsIssueDetails,
    isValueAddNP :: Bool
  }

validateRequest ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id Merchant ->
  DIssue ->
  ServiceHandle m ->
  m ValidatedDIssue
validateRequest merchantId dIssue@DIssue {..} iHandle = do
  merchant <- iHandle.findByMerchantId merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  booking <- iHandle.findByBookingId (Id dIssue.bookingId) >>= fromMaybeM (BookingDoesNotExist dIssue.bookingId)
  merchantOperatingCity <- iHandle.findMOCityById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  issueStatus <- mapStatusAndTypeToStatus issueStatusText issueTypeText
  issueType <- mapType issueTypeText
  igmConfig <- QIGMConfig.findByMerchantId merchantId >>= fromMaybeM (InternalError $ "IGMConfig not found " <> show merchantId)
  let bppId = merchant.subscriberId.getShortId
  pure $ ValidatedDIssue {..}

handler ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    EncFlow m r,
    HasField "slackNotificationConfig" r SlackNotificationConfig
  ) =>
  ValidatedDIssue ->
  ServiceHandle m ->
  m IssueRes
handler ValidatedDIssue {..} iHandle = do
  now <- getCurrentTime
  case issueStatus of
    DIGM.OPEN -> openBecknIssue ValidatedDIssue {..} iHandle
    DIGM.ESCALATED -> escalateBecknIssue ValidatedDIssue {..} now iHandle
    DIGM.CLOSED -> closeBecknIssue ValidatedDIssue {..} now iHandle
    DIGM.RESOLVED -> throwError $ InvalidRequest "Issue already resolved"

openBecknIssue ::
  ( EsqDBReplicaFlow m r,
    EncFlow m r,
    BeamFlow m r,
    HasField "slackNotificationConfig" r SlackNotificationConfig
  ) =>
  ValidatedDIssue ->
  ServiceHandle m ->
  m IssueRes
openBecknIssue dIssue@ValidatedDIssue {..} iHandle = do
  let (rName, rPhone, rEmail, rpName, rpPhone, rpEmail) = mkResContactFields igmConfig
  existingIssue <- QIGM.findByPrimaryKey (Id issueId)
  case existingIssue of
    Just existing -> do
      when (existing.issueStatus /= DIGM.OPEN) $
        throwError $ InvalidRequest "Issue already exists with different status"
      pure $
        IssueRes
          { issueId = issueId,
            respondentAction = show Spec.PROCESSING,
            groName = igmConfig.groName,
            groPhone = igmConfig.groPhone,
            groEmail = igmConfig.groEmail,
            respondentName = rName,
            respondentPhone = rPhone,
            respondentEmail = rEmail,
            resolutionProviderName = rpName,
            resolutionProviderPhone = rpPhone,
            resolutionProviderEmail = rpEmail,
            createdAt = createdAt,
            updatedAt = createdAt,
            merchantOperatingCity = merchantOperatingCity,
            domain = domain,
            isValueAddNP = isValueAddNP,
            ..
          }
    Nothing -> openBecknIssueNew dIssue iHandle

openBecknIssueNew ::
  ( EsqDBReplicaFlow m r,
    EncFlow m r,
    BeamFlow m r,
    HasField "slackNotificationConfig" r SlackNotificationConfig
  ) =>
  ValidatedDIssue ->
  ServiceHandle m ->
  m IssueRes
openBecknIssueNew dIssue@ValidatedDIssue {..} iHandle = do
  let OffUsIssueDetails {..} = offUsDetails
      (rName, rPhone, rEmail, rpName, rpPhone, rpEmail) = mkResContactFields igmConfig
  ride <- iHandle.findOneByBookingId booking.id merchant.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
  driverId <- fromMaybeM (RideFieldNotPresent "Driver not found") $ ride.driverId
  category <- QIC.findByIGMIssueCategory issueCategory >>= fromMaybeM (InvalidRequest "Issue Category not found or unsupported")
  transactionId <- case (isValueAddNP, contextTxnId) of
    (False, Just txnId) -> pure txnId
    _ -> generateGUID
  let igmIssue =
        DIGM.IGMIssue
          { DIGM.createdAt = convertRFC3339ToUTC createdAt,
            DIGM.customerEmail = customerEmail,
            DIGM.customerName = customerName,
            DIGM.customerPhone = customerPhone,
            DIGM.riderId = Nothing,
            DIGM.respondingMerchantId = Nothing,
            DIGM.respondentEntityType = Nothing,
            DIGM.resolutionShortDesc = Nothing,
            DIGM.resolutionLongDesc = Nothing,
            DIGM.resolutionActionTriggered = Nothing,
            DIGM.resolutionRefundAmount = Nothing,
            DIGM.descriptionShort = descShort,
            DIGM.descriptionLong = descLong,
            DIGM.orderState = orderState,
            DIGM.orderProviderId = orderProviderId,
            DIGM.orderMerchantOrderId = orderMerchantOrderId,
            DIGM.orderItemId = orderItemId,
            DIGM.orderItemQuantity = orderItemQuantity,
            DIGM.orderFulfillmentId = orderFulfillmentId,
            DIGM.orderFulfillmentState = orderFulfillmentState,
            DIGM.igmCategory = igmCategory,
            DIGM.igmSubCategory = igmSubCategory,
            DIGM.sourceType = sourceType,
            DIGM.sourceNpId = sourceNpId,
            DIGM.contextTransactionId = contextTxnId,
            DIGM.contextDomain = contextDomain,
            DIGM.expectedResponseTime = expectedResponseTime,
            DIGM.expectedResolutionTime = expectedResolutionTime,
            DIGM.issueRating = Nothing,
            DIGM.transactionId = transactionId,
            DIGM.merchantOperatingCityId = Just (cast merchantOperatingCity.id),
            DIGM.id = Id issueId,
            DIGM.bookingId = getId booking.id,
            DIGM.issueRaisedByMerchant = Just bapId,
            DIGM.issueStatus = issueStatus,
            DIGM.domain = domain,
            DIGM.issueType = issueType,
            DIGM.respondentAction = Nothing,
            DIGM.respondentName = Nothing,
            DIGM.respondentEmail = Nothing,
            DIGM.respondentPhone = Nothing,
            DIGM.updatedAt = fromMaybe (convertRFC3339ToUTC createdAt) issueUpdatedAt,
            DIGM.merchantId = Just booking.providerId
          }
  QIGM.create igmIssue
  storeNewComplainantActions issueId complainantActions True
  mbOption <- QIO.findByIGMIssueSubCategory issueSubCategory
  let optionId = mbOption <&> (.id)
      description = fromMaybe (maybe "No description provided" (.option) mbOption) descShort
  let issueReport = Common.IssueReportReq (Just $ cast ride.id) [] optionId category.id description Nothing (Just True) Nothing
  void $ Common.createIssueReport (cast driverId, cast dIssue.merchant.id) Nothing issueReport iHandle Common.DRIVER (Just issueId)
  let issueRes =
        IssueRes
          { issueId = issueId,
            respondentAction = show Spec.PROCESSING,
            groName = igmConfig.groName,
            groPhone = igmConfig.groPhone,
            groEmail = igmConfig.groEmail,
            respondentName = rName,
            respondentPhone = rPhone,
            respondentEmail = rEmail,
            resolutionProviderName = rpName,
            resolutionProviderPhone = rpPhone,
            resolutionProviderEmail = rpEmail,
            createdAt = createdAt,
            updatedAt = createdAt,
            merchantOperatingCity = merchantOperatingCity,
            domain = domain,
            isValueAddNP = isValueAddNP,
            ..
          }
  storeRespondentAction issueRes
  pure issueRes

escalateBecknIssue ::
  ( EsqDBReplicaFlow m r,
    EncFlow m r,
    BeamFlow m r
  ) =>
  ValidatedDIssue ->
  UTCTime ->
  ServiceHandle m ->
  m IssueRes
escalateBecknIssue ValidatedDIssue {..} now iHandle = do
  let OffUsIssueDetails {..} = offUsDetails
      (rName, rPhone, rEmail, rpName, rpPhone, rpEmail) = mkResContactFields igmConfig
  igmIssue <- QIGM.findByPrimaryKey (Id issueId) >>= fromMaybeM (InvalidRequest "Issue not found")
  unless (igmIssue.issueStatus == DIGM.OPEN) $
    throwError $ InvalidRequest "Issue can only be escalated from OPEN status"
  let updatedIssue =
        igmIssue
          { DIGM.issueStatus = DIGM.ESCALATED,
            DIGM.updatedAt = fromMaybe now issueUpdatedAt
          }
  QIGM.updateByPrimaryKey updatedIssue
  storeNewComplainantActions issueId complainantActions False
  issueReport <- QIR.findByBecknIssueId issueId >>= fromMaybeM (InvalidRequest "Issue Report not found")
  QIR.updateStatusAssignee issueReport.id (Just Common.IN_PROGRESS) issueReport.assignee
  void $ Common.updateTicketStatus issueReport TIT.Pending (cast merchant.id) (cast merchantOperatingCity.id) iHandle CUSTOMER "Escalated by buyer app"
  let issueRes =
        IssueRes
          { issueId = issueId,
            respondentAction = show Spec.PROCESSING,
            groName = igmConfig.groName,
            groPhone = igmConfig.groPhone,
            groEmail = igmConfig.groEmail,
            respondentName = rName,
            respondentPhone = rPhone,
            respondentEmail = rEmail,
            resolutionProviderName = rpName,
            resolutionProviderPhone = rpPhone,
            resolutionProviderEmail = rpEmail,
            createdAt = UTCTimeRFC3339 igmIssue.createdAt,
            updatedAt = UTCTimeRFC3339 now,
            merchantOperatingCity = merchantOperatingCity,
            domain = igmIssue.domain,
            isValueAddNP = isValueAddNP,
            ..
          }
  storeRespondentAction issueRes
  pure issueRes

closeBecknIssue ::
  ( EsqDBReplicaFlow m r,
    EncFlow m r,
    BeamFlow m r
  ) =>
  ValidatedDIssue ->
  UTCTime ->
  ServiceHandle m ->
  m IssueRes
closeBecknIssue ValidatedDIssue {..} now iHandle = do
  let OffUsIssueDetails {..} = offUsDetails
      (rName, rPhone, rEmail, rpName, rpPhone, rpEmail) = mkResContactFields igmConfig
  igmIssue <- QIGM.findByPrimaryKey (Id issueId) >>= fromMaybeM (InvalidRequest "Issue not found")
  when (igmIssue.issueStatus == DIGM.CLOSED || igmIssue.issueStatus == DIGM.RESOLVED) $
    throwError $ InvalidRequest "Issue is already closed or resolved"
  let updatedIssue =
        igmIssue
          { DIGM.issueStatus = DIGM.CLOSED,
            DIGM.updatedAt = fromMaybe now issueUpdatedAt,
            DIGM.respondentAction = Just $ show Spec.RESOLVED
          }
  QIGM.updateByPrimaryKey updatedIssue
  storeNewComplainantActions issueId complainantActions False
  issueReport <- QIR.findByBecknIssueId issueId >>= fromMaybeM (InvalidRequest "Issue Report not found")
  QIR.updateStatusAssignee issueReport.id (Just Common.CLOSED) issueReport.assignee
  void $ Common.updateTicketStatus issueReport TIT.Closed (cast merchant.id) (cast merchantOperatingCity.id) iHandle CUSTOMER "Closed by person"
  let issueRes =
        IssueRes
          { issueId = issueId,
            respondentAction = show Spec.RESOLVED,
            groName = igmConfig.groName,
            groPhone = igmConfig.groPhone,
            groEmail = igmConfig.groEmail,
            respondentName = rName,
            respondentPhone = rPhone,
            respondentEmail = rEmail,
            resolutionProviderName = rpName,
            resolutionProviderPhone = rpPhone,
            resolutionProviderEmail = rpEmail,
            createdAt = UTCTimeRFC3339 igmIssue.createdAt,
            updatedAt = UTCTimeRFC3339 now,
            merchantOperatingCity = merchantOperatingCity,
            domain = igmIssue.domain,
            isValueAddNP = isValueAddNP,
            ..
          }
  storeRespondentAction issueRes
  pure issueRes

mapStatusAndTypeToStatus :: MonadFlow m => Text -> Text -> m DIGM.Status
mapStatusAndTypeToStatus "OPEN" "ISSUE" = return DIGM.OPEN
mapStatusAndTypeToStatus "OPEN" "GRIEVANCE" = return DIGM.ESCALATED
mapStatusAndTypeToStatus "CLOSED" _ = return DIGM.CLOSED
mapStatusAndTypeToStatus _ _ = throwError $ InvalidRequest "Invalid issue status or type"

storeNewComplainantActions ::
  (BeamFlow m r, MonadFlow m) =>
  Text ->
  [Spec.ComplainantAction] ->
  Bool ->
  m ()
storeNewComplainantActions _igmIssueId [] _skipDedup = pure ()
storeNewComplainantActions igmIssueId incomingActions skipDedup = do
  now <- getCurrentTime
  existingActions <-
    if skipDedup
      then pure []
      else QIGA.findAllByIgmIssueIdAndType (Id igmIssueId) DIGA.COMPLAINANT
  let existingKeys = map (\a -> (a.action, a.updatedAt)) existingActions
  forM_ incomingActions $ \ca -> do
    let caAction = fromMaybe "OPEN" ca.complainantActionComplainantAction
        caUpdatedAt = maybe now convertRFC3339ToUTC ca.complainantActionUpdatedAt
    unless ((caAction, caUpdatedAt) `elem` existingKeys) $ do
      actionId <- generateGUID
      let igmAction =
            DIGA.IGMIssueAction
              { DIGA.id = Id actionId,
                DIGA.igmIssueId = Id igmIssueId,
                DIGA.actionType = DIGA.COMPLAINANT,
                DIGA.action = caAction,
                DIGA.shortDesc = ca.complainantActionShortDesc,
                DIGA.updatedAt = caUpdatedAt,
                DIGA.updatedByOrgName = ca.complainantActionUpdatedBy >>= (.organizationOrg) >>= (.organizationOrgName),
                DIGA.updatedByContactPhone = ca.complainantActionUpdatedBy >>= (.organizationContact) >>= (.gROContactPhone),
                DIGA.updatedByContactEmail = ca.complainantActionUpdatedBy >>= (.organizationContact) >>= (.gROContactEmail),
                DIGA.updatedByPersonName = ca.complainantActionUpdatedBy >>= (.organizationPerson) >>= (.complainantPersonName),
                DIGA.cascadedLevel = Nothing,
                DIGA.createdAt = now
              }
      QIGA.create igmAction

mkResContactFields :: IGMConfig -> (Text, Text, Text, Text, Text, Text)
mkResContactFields igmCfg =
  ( fromMaybe igmCfg.groName igmCfg.respondentName,
    fromMaybe igmCfg.groPhone igmCfg.respondentPhone,
    fromMaybe igmCfg.groEmail igmCfg.respondentEmail,
    fromMaybe igmCfg.groName igmCfg.resolutionProviderName,
    fromMaybe igmCfg.groPhone igmCfg.resolutionProviderPhone,
    fromMaybe igmCfg.groEmail igmCfg.resolutionProviderEmail
  )

mapType :: MonadFlow m => Text -> m DIGM.IssueType
mapType "ISSUE" = return DIGM.ISSUE
mapType "GRIEVANCE" = return DIGM.GRIEVANCE
mapType _ = throwError $ InvalidRequest "Invalid issue type"

mapDomainStatusToSpecStatus :: DIGM.Status -> Maybe Text
mapDomainStatusToSpecStatus DIGM.CLOSED = Just $ show Spec.CLOSED
mapDomainStatusToSpecStatus _ = Just $ show Spec.OPEN

storeRespondentAction ::
  (BeamFlow m r, MonadFlow m) =>
  IssueRes ->
  m ()
storeRespondentAction issueRes = do
  now <- getCurrentTime
  existingActions <- QIGA.findAllByIgmIssueIdAndType (Id issueRes.issueId) DIGA.RESPONDENT
  let existingKeys = map (\a -> (a.action, a.updatedAt)) existingActions
      actionUpdatedAt = convertRFC3339ToUTC issueRes.updatedAt
  unless ((issueRes.respondentAction, actionUpdatedAt) `elem` existingKeys) $ do
    actionId <- generateGUID
    let igmAction =
          DIGA.IGMIssueAction
            { DIGA.id = Id actionId,
              DIGA.igmIssueId = Id issueRes.issueId,
              DIGA.actionType = DIGA.RESPONDENT,
              DIGA.action = issueRes.respondentAction,
              DIGA.shortDesc = Just $ respondentActionToShortDesc issueRes.respondentAction,
              DIGA.updatedAt = actionUpdatedAt,
              DIGA.updatedByOrgName = mkOrgName issueRes.bppId issueRes.domain,
              DIGA.updatedByContactPhone = Just issueRes.respondentPhone,
              DIGA.updatedByContactEmail = Just issueRes.respondentEmail,
              DIGA.updatedByPersonName = Just issueRes.respondentName,
              DIGA.cascadedLevel = Just 1,
              DIGA.createdAt = now
            }
    QIGA.create igmAction

respondentActionToShortDesc :: Text -> Text
respondentActionToShortDesc "PROCESSING" = "Complaint is being processed"
respondentActionToShortDesc "RESOLVED" = "Issue has been resolved"
respondentActionToShortDesc "CASCADED" = "Issue has been cascaded"
respondentActionToShortDesc "NEED-MORE-INFO" = "Need more information from complainant"
respondentActionToShortDesc action = "Issue status: " <> action
