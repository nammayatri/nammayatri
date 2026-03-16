module Domain.Action.Dashboard.IssueManagement.Issue
  ( dashboardIssueHandle,
    getIssueCategoryList,
    getIssueList,
    getIssueInfo,
    getIssueInfoV2,
    putIssueUpdate,
    postIssueComment,
    getIssueMedia,
    postIssueTicketStatusCallBack,
    postIssueCategoryCreate,
    postIssueCategoryUpdate,
    postIssueOptionCreate,
    postIssueOptionUpdate,
    postIssueMessageUpsert,
    postIssueKaptureCreate,
    -- New Admin Content Management APIs
    getIssueCategoryDetail,
    getIssueOptionDetail,
    getIssueMessageDetail,
    getIssueMessageList,
    getIssueOptionList,
    deleteIssueCategory,
    deleteIssueOption,
    deleteIssueMessage,
    getIssueCategoryFlowPreview,
    getIssueTranslations,
    postIssueBulkUpsertTranslations,
    getIssueConfig,
    postIssueConfigUpdate,
    postIssueCategoryReorder,
    postIssueOptionReorder,
    postIssueMessageReorder,
    -- IGM Dashboard APIs (BAP)
    postIGMIssueRaise,
    getIGMIssueTrail,
    postIGMIssueTriggerAction,
  )
where

import qualified API.Types.RiderPlatform.IssueManagement.Issue
import qualified API.UI.Issue as AUI
import qualified Beckn.ACL.IGM.Issue as ACL
import qualified Data.Aeson
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified IGM.Enums as IGMSpec
import qualified IssueManagement.Common
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Common.Dashboard.Issue
import qualified IssueManagement.Domain.Action.Dashboard.Issue as DIssue
import qualified IssueManagement.Domain.Action.UI.Issue as DAI
import qualified IssueManagement.Domain.Types.Issue.IssueCategory
import qualified IssueManagement.Domain.Types.Issue.IssueMessage
import qualified IssueManagement.Domain.Types.Issue.IssueOption
import qualified IssueManagement.Domain.Types.Issue.IssueReport
import qualified IssueManagement.Storage.Queries.Issue.IssueCategory as QIC
import qualified IssueManagement.Storage.Queries.Issue.IGMConfig as QIGMConfig
import qualified IssueManagement.Storage.Queries.Issue.IGMIssue as QIGM
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallIGMBPP as CallBPP
import SharedLogic.FRFSUtils
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Person as QP
import Tools.Error

dashboardIssueHandle :: DAI.ServiceHandle Environment.Flow
dashboardIssueHandle = AUI.customerIssueHandle

getIssueCategoryList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow API.Types.RiderPlatform.IssueManagement.Issue.IssueCategoryListRes
getIssueCategoryList (Kernel.Types.Id.ShortId merchantShortId) opCity =
  DIssue.issueCategoryList (Kernel.Types.Id.ShortId merchantShortId) opCity dashboardIssueHandle Common.CUSTOMER

getIssueList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe IssueManagement.Common.IssueStatus ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Common.Ride) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Environment.Flow API.Types.RiderPlatform.IssueManagement.Issue.IssueReportListResponse
getIssueList (Kernel.Types.Id.ShortId merchantShortId) opCity mbLimit mbOffset mbStatus mbCategoryId mbCategoryName mbAssignee mbCountryCode mbMobileNumber mbRideShortId mbDescriptionSearch mbFromDate mbToDate mbSource =
  DIssue.issueList
    (Kernel.Types.Id.ShortId merchantShortId)
    opCity
    mbLimit
    mbOffset
    mbStatus
    (Kernel.Types.Id.cast <$> mbCategoryId)
    mbCategoryName
    mbAssignee
    mbCountryCode
    mbMobileNumber
    mbRideShortId
    mbDescriptionSearch
    mbFromDate
    mbToDate
    mbSource
    dashboardIssueHandle
    Common.CUSTOMER

getIssueInfo ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  Environment.Flow API.Types.RiderPlatform.IssueManagement.Issue.IssueInfoDRes
getIssueInfo (Kernel.Types.Id.ShortId merchantShortId) city issueReportId =
  DIssue.issueInfo (Kernel.Types.Id.ShortId merchantShortId) city (Just issueReportId) Nothing dashboardIssueHandle Common.CUSTOMER

getIssueInfoV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) ->
  Environment.Flow API.Types.RiderPlatform.IssueManagement.Issue.IssueInfoDRes
getIssueInfoV2 (Kernel.Types.Id.ShortId merchantShortId) city mbIssueReportId mbIssueReportShortId =
  DIssue.issueInfo (Kernel.Types.Id.ShortId merchantShortId) city mbIssueReportId mbIssueReportShortId dashboardIssueHandle Common.CUSTOMER

putIssueUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  IssueManagement.Common.Dashboard.Issue.IssueUpdateByUserReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putIssueUpdate (Kernel.Types.Id.ShortId merchantShortId) city issueReportId req =
  DIssue.issueUpdate (Kernel.Types.Id.ShortId merchantShortId) city (Kernel.Types.Id.cast issueReportId) dashboardIssueHandle req

postIssueComment ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  IssueManagement.Common.Dashboard.Issue.IssueAddCommentByUserReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueComment (Kernel.Types.Id.ShortId merchantShortId) city issueReportId req =
  DIssue.issueAddComment
    (Kernel.Types.Id.ShortId merchantShortId)
    city
    (Kernel.Types.Id.cast issueReportId)
    dashboardIssueHandle
    req

getIssueMedia ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Prelude.Text
getIssueMedia (Kernel.Types.Id.ShortId merchantShortId) _ = DIssue.issueFetchMedia (Kernel.Types.Id.ShortId merchantShortId)

postIssueTicketStatusCallBack ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Data.Aeson.Value ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueTicketStatusCallBack (Kernel.Types.Id.ShortId _merchantShortId) _city req = DIssue.ticketStatusCallBack req dashboardIssueHandle Common.CUSTOMER

postIssueCategoryCreate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryReq ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryRes
postIssueCategoryCreate (Kernel.Types.Id.ShortId merchantShortId) city req =
  DIssue.createIssueCategory (Kernel.Types.Id.ShortId merchantShortId) city req dashboardIssueHandle Common.CUSTOMER

postIssueCategoryUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory ->
  IssueManagement.Common.Dashboard.Issue.UpdateIssueCategoryReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueCategoryUpdate (Kernel.Types.Id.ShortId merchantShortId) city issueCategoryId req =
  DIssue.updateIssueCategory (Kernel.Types.Id.ShortId merchantShortId) city issueCategoryId req dashboardIssueHandle Common.CUSTOMER

postIssueOptionCreate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage ->
  IssueManagement.Common.Dashboard.Issue.CreateIssueOptionReq ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.CreateIssueOptionRes
postIssueOptionCreate (Kernel.Types.Id.ShortId merchantShortId) city issueCategoryId issueMessageId req =
  DIssue.createIssueOption
    (Kernel.Types.Id.ShortId merchantShortId)
    city
    Nothing
    issueCategoryId
    issueMessageId
    dashboardIssueHandle
    req
    Common.CUSTOMER

postIssueOptionUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption ->
  IssueManagement.Common.Dashboard.Issue.UpdateIssueOptionReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueOptionUpdate (Kernel.Types.Id.ShortId merchantShortId) city issueOptionId req =
  DIssue.updateIssueOption (Kernel.Types.Id.ShortId merchantShortId) city issueOptionId req dashboardIssueHandle Common.CUSTOMER

postIssueMessageUpsert ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageReq ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageRes
postIssueMessageUpsert (Kernel.Types.Id.ShortId merchantShortId) city req =
  DIssue.upsertIssueMessage (Kernel.Types.Id.ShortId merchantShortId) city req dashboardIssueHandle Common.CUSTOMER

postIssueKaptureCreate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.IssueReportReqV2 ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueKaptureCreate (Kernel.Types.Id.ShortId merchantShortId) city req =
  DIssue.createIssueReportV2
    (Kernel.Types.Id.ShortId merchantShortId)
    city
    req
    dashboardIssueHandle
    Common.CUSTOMER

-----------------------------------------------------------
-- New Admin Content Management APIs ----------------------

getIssueCategoryDetail ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory ->
  Kernel.Prelude.Maybe Kernel.External.Types.Language ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueCategoryDetailRes
getIssueCategoryDetail (Kernel.Types.Id.ShortId merchantShortId) city categoryId mbLanguage =
  DIssue.getCategoryDetail (Kernel.Types.Id.ShortId merchantShortId) city categoryId mbLanguage dashboardIssueHandle Common.CUSTOMER

getIssueOptionDetail ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption ->
  Kernel.Prelude.Maybe Kernel.External.Types.Language ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueOptionDetailRes
getIssueOptionDetail (Kernel.Types.Id.ShortId merchantShortId) city optionId mbLanguage =
  DIssue.getOptionDetail (Kernel.Types.Id.ShortId merchantShortId) city optionId mbLanguage dashboardIssueHandle Common.CUSTOMER

getIssueMessageDetail ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage ->
  Kernel.Prelude.Maybe Kernel.External.Types.Language ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueMessageDetailRes
getIssueMessageDetail (Kernel.Types.Id.ShortId merchantShortId) city messageId mbLanguage =
  DIssue.getMessageDetail (Kernel.Types.Id.ShortId merchantShortId) city messageId mbLanguage dashboardIssueHandle Common.CUSTOMER

getIssueMessageList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.External.Types.Language ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueMessageListRes
getIssueMessageList (Kernel.Types.Id.ShortId merchantShortId) city mbCategoryId mbOptionId mbIsActive mbLanguage =
  DIssue.listMessages (Kernel.Types.Id.ShortId merchantShortId) city mbCategoryId mbOptionId mbIsActive mbLanguage dashboardIssueHandle Common.CUSTOMER

getIssueOptionList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.External.Types.Language ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueOptionListDRes
getIssueOptionList (Kernel.Types.Id.ShortId merchantShortId) city mbCategoryId mbMessageId mbIsActive mbLanguage =
  DIssue.listOptions (Kernel.Types.Id.ShortId merchantShortId) city mbCategoryId mbMessageId mbIsActive mbLanguage dashboardIssueHandle Common.CUSTOMER

deleteIssueCategory ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteIssueCategory (Kernel.Types.Id.ShortId merchantShortId) city categoryId =
  DIssue.deleteIssueCategory (Kernel.Types.Id.ShortId merchantShortId) city categoryId dashboardIssueHandle Common.CUSTOMER

deleteIssueOption ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteIssueOption (Kernel.Types.Id.ShortId merchantShortId) city optionId =
  DIssue.deleteIssueOption (Kernel.Types.Id.ShortId merchantShortId) city optionId dashboardIssueHandle Common.CUSTOMER

deleteIssueMessage ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteIssueMessage (Kernel.Types.Id.ShortId merchantShortId) city messageId =
  DIssue.deleteIssueMessage (Kernel.Types.Id.ShortId merchantShortId) city messageId dashboardIssueHandle Common.CUSTOMER

getIssueCategoryFlowPreview ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory ->
  Kernel.Prelude.Maybe Kernel.External.Types.Language ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueCategoryFlowPreviewRes
getIssueCategoryFlowPreview (Kernel.Types.Id.ShortId merchantShortId) city categoryId mbLanguage =
  DIssue.previewCategoryFlow (Kernel.Types.Id.ShortId merchantShortId) city categoryId mbLanguage dashboardIssueHandle Common.CUSTOMER

getIssueTranslations ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueTranslationListRes
getIssueTranslations (Kernel.Types.Id.ShortId merchantShortId) city sentence =
  DIssue.getTranslations (Kernel.Types.Id.ShortId merchantShortId) city sentence dashboardIssueHandle Common.CUSTOMER

postIssueBulkUpsertTranslations ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.BulkUpsertTranslationsReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueBulkUpsertTranslations (Kernel.Types.Id.ShortId merchantShortId) city req =
  DIssue.bulkUpsertTranslations (Kernel.Types.Id.ShortId merchantShortId) city req dashboardIssueHandle Common.CUSTOMER

getIssueConfig ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueConfigRes
getIssueConfig (Kernel.Types.Id.ShortId merchantShortId) city =
  DIssue.getIssueConfig (Kernel.Types.Id.ShortId merchantShortId) city dashboardIssueHandle Common.CUSTOMER

postIssueConfigUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.UpdateIssueConfigReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueConfigUpdate (Kernel.Types.Id.ShortId merchantShortId) city req =
  DIssue.updateIssueConfig (Kernel.Types.Id.ShortId merchantShortId) city req dashboardIssueHandle Common.CUSTOMER

postIssueCategoryReorder ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.ReorderIssueCategoryReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueCategoryReorder (Kernel.Types.Id.ShortId merchantShortId) city req =
  DIssue.reorderCategories (Kernel.Types.Id.ShortId merchantShortId) city req dashboardIssueHandle Common.CUSTOMER

postIssueOptionReorder ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.ReorderIssueOptionReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueOptionReorder (Kernel.Types.Id.ShortId merchantShortId) city req =
  DIssue.reorderOptions (Kernel.Types.Id.ShortId merchantShortId) city req dashboardIssueHandle Common.CUSTOMER

postIssueMessageReorder ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.ReorderIssueMessageReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueMessageReorder (Kernel.Types.Id.ShortId merchantShortId) city req =
  DIssue.reorderMessages (Kernel.Types.Id.ShortId merchantShortId) city req dashboardIssueHandle Common.CUSTOMER

---------------------------------------------------------
-- IGM Dashboard APIs (BAP) ----------------------------
---------------------------------------------------------

postIGMIssueRaise ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.RaiseIssuePayload ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIGMIssueRaise (Kernel.Types.Id.ShortId merchantShortId) opCity req = do
  -- Step 1: Create DB records (IssueReport + IGMIssue) via shared-services
  _ <- DIssue.igmIssueDashboardRaise (Kernel.Types.Id.ShortId merchantShortId) opCity dashboardIssueHandle Common.CUSTOMER req
  -- Step 2: If external issue, send Beckn issue API to provider
  let isExternal = req.source == "BAP" || req.source == "BPP"
  when isExternal $ do
    let parsedDomain = case req.domain of
          "MOBILITY" -> IGMSpec.ON_DEMAND
          "PUBLIC_TRANSPORT" -> IGMSpec.PUBLIC_TRANSPORT
          _ -> IGMSpec.ON_DEMAND
    merchantOpCity <- CQMOC.findByMerchantShortIdAndCity (Kernel.Types.Id.ShortId merchantShortId) opCity
                        >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId <> "-city-" <> show opCity)
    merchant <- QMerchant.findById merchantOpCity.merchantId >>= fromMaybeM (MerchantNotFound merchantOpCity.merchantId.getId)
    igmConfig <- QIGMConfig.findByMerchantId (Kernel.Types.Id.cast merchantOpCity.merchantId)
                   >>= fromMaybeM (InternalError $ "IGMConfig not found for merchant: " <> merchantOpCity.merchantId.getId)
    -- Lookup booking and build RideBooking
    (rideBooking, providerUrl, riderId) <- case parsedDomain of
      IGMSpec.ON_DEMAND -> do
        booking <- QB.findById (Kernel.Types.Id.Id req.orderId) >>= fromMaybeM (BookingNotFound req.orderId)
        pure (AUI.fromBooking booking, booking.providerUrl, Kernel.Types.Id.cast booking.riderId)
      IGMSpec.PUBLIC_TRANSPORT -> do
        ticketBooking <- QFTB.findById (Kernel.Types.Id.Id req.orderId) >>= fromMaybeM (TicketBookingNotFound req.orderId)
        quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId ticketBooking.quoteId
        let fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
        rb <- AUI.fromFRFSTicketBooking ticketBooking fareParameters
        pUrl <- Kernel.Prelude.parseBaseUrl ticketBooking.bppSubscriberUrl
        pure (rb, pUrl, Kernel.Types.Id.cast ticketBooking.riderId)
    -- Lookup person from booking's riderId
    person <- QP.findById riderId >>= fromMaybeM (PersonNotFound riderId.getId)
    -- Lookup IGM category
    category <- QIC.findByIGMIssueCategory req.category >>= fromMaybeM (InvalidRequest $ "IGM IssueCategory not found for: " <> req.category)
    -- Read back IGMIssue created by shared-services (find latest by bookingId)
    mbIgmIssues <- QIGM.findByBookingId req.orderId
    igmIssue <- case mbIgmIssues of
      [] -> throwError $ InvalidRequest "IGMIssue not created for this booking"
      issues -> pure $ Kernel.Prelude.last $ sortBy (comparing (.createdAt)) issues
    -- Build Beckn issue request reusing existing ACL function
    (becknIssueReq, _, updatedIgmIssue) <- ACL.buildIssueReq rideBooking category Nothing req.shortDesc merchant person igmConfig merchantOpCity Nothing Nothing (Just igmIssue)
    QIGM.updateByPrimaryKey updatedIgmIssue
    fork "sending beckn issue from dashboard" . withShortRetry $ do
      void $ CallBPP.issue providerUrl becknIssueReq
  pure Kernel.Types.APISuccess.Success

getIGMIssueTrail ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IgmIssueData
getIGMIssueTrail (Kernel.Types.Id.ShortId merchantShortId) opCity issueReportId =
  DIssue.igmIssueDashboardGetTrail (Kernel.Types.Id.ShortId merchantShortId) opCity issueReportId dashboardIssueHandle Common.CUSTOMER

postIGMIssueTriggerAction ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  IssueManagement.Common.Dashboard.Issue.IgmRespondentActionPayload ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIGMIssueTriggerAction (Kernel.Types.Id.ShortId merchantShortId) opCity issueReportId req =
  DIssue.igmIssueDashboardTriggerAction (Kernel.Types.Id.ShortId merchantShortId) opCity (Kernel.Types.Id.cast issueReportId) dashboardIssueHandle req
