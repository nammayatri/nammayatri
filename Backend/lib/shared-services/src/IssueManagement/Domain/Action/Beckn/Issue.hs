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
import IssueManagement.Common
import qualified IssueManagement.Common.UI.Issue as Common
import IssueManagement.Domain.Action.UI.Issue
import qualified IssueManagement.Domain.Action.UI.Issue as Common
import IssueManagement.Domain.Types.Issue.IGMConfig
import qualified IssueManagement.Domain.Types.Issue.IGMIssue as DIGM
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IGMConfig as QIGMConfig
import qualified IssueManagement.Storage.Queries.Issue.IGMIssue as QIGM
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
    bapId :: Text
  }
  deriving (Show, Generic)

data IssueRes = IssueRes
  { issueId :: Text,
    respondentAction :: Text,
    groName :: Text,
    groPhone :: Text,
    groEmail :: Text,
    createdAt :: UTCTimeRFC3339,
    updatedAt :: UTCTimeRFC3339,
    merchant :: Merchant,
    merchantOperatingCity :: MerchantOperatingCity,
    issueStatus :: DIGM.Status,
    bapId :: Text,
    bppId :: Text
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
    bppId :: Text
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
    HasField "sosAlertsTopicARN" r Text
  ) =>
  ValidatedDIssue ->
  ServiceHandle m ->
  m IssueRes
handler ValidatedDIssue {..} iHandle = do
  now <- getCurrentTime
  case issueStatus of
    DIGM.OPEN -> openBecknIssue ValidatedDIssue {..} iHandle
    DIGM.ESCALATED -> escalateBecknIssue ValidatedDIssue {..} now
    DIGM.CLOSED -> closeBecknIssue ValidatedDIssue {..} now iHandle
    DIGM.RESOLVED -> throwError $ InvalidRequest "Issue already resolved"

openBecknIssue ::
  ( EsqDBReplicaFlow m r,
    EncFlow m r,
    BeamFlow m r,
    HasField "sosAlertsTopicARN" r Text
  ) =>
  ValidatedDIssue ->
  ServiceHandle m ->
  m IssueRes
openBecknIssue dIssue@ValidatedDIssue {..} iHandle = do
  ride <- iHandle.findOneByBookingId booking.id merchant.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
  transactionId <- generateGUID
  -- riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id") -- shrey00 : incorporate it back?
  let igmIssue =
        DIGM.IGMIssue
          { DIGM.createdAt = convertRFC3339ToUTC createdAt,
            DIGM.customerEmail = customerEmail,
            DIGM.customerName = customerName,
            DIGM.customerPhone = customerPhone,
            DIGM.riderId = Nothing,
            DIGM.respondingMerchantId = Nothing,
            DIGM.respondentEntityType = Nothing,
            DIGM.transactionId = transactionId,
            DIGM.merchantOperatingCityId = Nothing,
            DIGM.id = Id issueId,
            DIGM.bookingId = getId booking.id,
            DIGM.issueRaisedByMerchant = Just bapId,
            DIGM.issueStatus = issueStatus,
            DIGM.domain = Spec.PUBLIC_TRANSPORT,
            DIGM.issueType = issueType,
            DIGM.respondentAction = Nothing,
            DIGM.respondentName = Nothing,
            DIGM.respondentEmail = Nothing,
            DIGM.respondentPhone = Nothing,
            DIGM.updatedAt = convertRFC3339ToUTC createdAt,
            DIGM.merchantId = Just booking.providerId
          }
  category <- QIC.findByIGMIssueCategory issueCategory >>= fromMaybeM (InvalidRequest "Issue Category not found or unsupported")
  QIGM.create igmIssue
  mbOption <- QIO.findByIGMIssueSubCategory issueSubCategory
  let optionId = mbOption <&> (.id)
      description = maybe "No description provided" (.option) mbOption
  let issueReport = Common.IssueReportReq (Just $ cast ride.id) [] optionId category.id description Nothing (Just True) Nothing -- insert ticketbookingId, which is given by UI
  driverId <- fromMaybeM (RideFieldNotPresent "Driver not found") $ ride.driverId
  void $ Common.createIssueReport (cast driverId, cast dIssue.merchant.id) Nothing issueReport iHandle Common.DRIVER (Just issueId)
  pure $
    IssueRes
      { issueId = issueId,
        respondentAction = show Spec.PROCESSING,
        groName = igmConfig.groName,
        groPhone = igmConfig.groPhone,
        groEmail = igmConfig.groEmail,
        createdAt = createdAt,
        updatedAt = createdAt,
        merchantOperatingCity = merchantOperatingCity,
        ..
      }

escalateBecknIssue ::
  ( EsqDBReplicaFlow m r,
    EncFlow m r,
    BeamFlow m r
  ) =>
  ValidatedDIssue ->
  UTCTime ->
  m IssueRes
escalateBecknIssue ValidatedDIssue {..} now = do
  igmIssue <- QIGM.findByPrimaryKey (Id issueId) >>= fromMaybeM (InvalidRequest "Issue not found")
  let updatedIssue =
        igmIssue
          { DIGM.issueStatus = DIGM.ESCALATED,
            DIGM.updatedAt = now
          }
  QIGM.updateByPrimaryKey updatedIssue
  -- shrey00 : increase priority of capture issue : How?
  pure $
    IssueRes
      { issueId = issueId,
        respondentAction = show Spec.PROCESSING,
        groName = igmConfig.groName,
        groPhone = igmConfig.groPhone,
        groEmail = igmConfig.groEmail,
        createdAt = UTCTimeRFC3339 igmIssue.createdAt,
        updatedAt = UTCTimeRFC3339 now,
        merchantOperatingCity = merchantOperatingCity,
        ..
      }

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
  igmIssue <- QIGM.findByPrimaryKey (Id issueId) >>= fromMaybeM (InvalidRequest "Issue not found")
  let updatedIssue =
        igmIssue
          { DIGM.issueStatus = DIGM.CLOSED,
            DIGM.updatedAt = now,
            DIGM.respondentAction = Just $ show Spec.RESOLVED
          }
  QIGM.updateByPrimaryKey updatedIssue
  issueReport <- QIR.findByBecknIssueId issueId >>= fromMaybeM (InvalidRequest "Issue Report not found")
  QIR.updateStatusAssignee issueReport.id (Just Common.CLOSED) issueReport.assignee
  void $ Common.updateTicketStatus issueReport TIT.CL (cast merchant.id) (cast merchantOperatingCity.id) iHandle "Closed by person"
  pure $
    IssueRes
      { issueId = issueId,
        respondentAction = show Spec.RESOLVED,
        groName = igmConfig.groName,
        groPhone = igmConfig.groPhone,
        groEmail = igmConfig.groEmail,
        createdAt = UTCTimeRFC3339 igmIssue.createdAt,
        updatedAt = UTCTimeRFC3339 now,
        merchantOperatingCity = merchantOperatingCity,
        ..
      }

mapStatusAndTypeToStatus :: MonadFlow m => Text -> Text -> m DIGM.Status
mapStatusAndTypeToStatus "OPEN" "ISSUE" = return DIGM.OPEN
mapStatusAndTypeToStatus "OPEN" "GRIEVANCE" = return DIGM.ESCALATED
mapStatusAndTypeToStatus "CLOSED" _ = return DIGM.CLOSED
mapStatusAndTypeToStatus _ _ = throwError $ InvalidRequest "Invalid issue status or type"

mapType :: MonadFlow m => Text -> m DIGM.IssueType
mapType "ISSUE" = return DIGM.ISSUE
mapType "GRIEVANCE" = return DIGM.GRIEVANCE
mapType _ = throwError $ InvalidRequest "Invalid issue type"

mapDomainStatusToSpecStatus :: DIGM.Status -> Maybe Text
mapDomainStatusToSpecStatus DIGM.CLOSED = Just $ show Spec.CLOSED
mapDomainStatusToSpecStatus _ = Just $ show Spec.OPEN
