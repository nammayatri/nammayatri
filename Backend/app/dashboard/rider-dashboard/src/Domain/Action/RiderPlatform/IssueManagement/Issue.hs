{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.RiderPlatform.IssueManagement.Issue
  ( getIssueCategoryList,
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
  )
where

import qualified API.Client.RiderPlatform.IssueManagement
import qualified API.Types.RiderPlatform.IssueManagement.Issue
import qualified Dashboard.Common
import qualified Data.Aeson
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue
import qualified IssueManagement.Common.Dashboard.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IssueCategory
import qualified IssueManagement.Domain.Types.Issue.IssueMessage
import qualified IssueManagement.Domain.Types.Issue.IssueOption
import qualified IssueManagement.Domain.Types.Issue.IssueReport
import Kernel.Beam.Functions as B
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (PersonError (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Person as QP
import Tools.Auth.Api
import Tools.Auth.Merchant

getIssueCategoryList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Environment.Flow API.Types.RiderPlatform.IssueManagement.Issue.IssueCategoryListRes
getIssueCategoryList merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueCategoryList)

getIssueList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe IssueManagement.Common.IssueStatus ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Common.Ride) ->
  Environment.Flow API.Types.RiderPlatform.IssueManagement.Issue.IssueReportListResponse
getIssueList merchantShortId opCity apiTokenInfo limit offset status category assignee countryCode phoneNumber rideShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueList) limit offset status category assignee countryCode phoneNumber rideShortId

getIssueInfo ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  Environment.Flow API.Types.RiderPlatform.IssueManagement.Issue.IssueInfoRes
getIssueInfo merchantShortId opCity apiTokenInfo issueId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  addAuthorDetails =<< API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueInfo) issueId

getIssueInfoV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Environment.Flow API.Types.RiderPlatform.IssueManagement.Issue.IssueInfoRes)
getIssueInfoV2 merchantShortId opCity apiTokenInfo issueId issueShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  addAuthorDetails =<< API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueInfoV2) issueId issueShortId

addAuthorDetails :: Common.IssueInfoRes -> Environment.Flow Common.IssueInfoRes
addAuthorDetails Common.IssueInfoRes {..} = do
  comments_ <- mapM mkAuthorDetail comments
  pure $
    Common.IssueInfoRes
      { comments = comments_,
        ..
      }
  where
    mkAuthorDetail :: Common.IssueReportCommentItem -> Environment.Flow Common.IssueReportCommentItem
    mkAuthorDetail Common.IssueReportCommentItem {..} = do
      author <- B.runInReplica (QP.findById $ Kernel.Types.Id.cast authorDetail.authorId) >>= fromMaybeM (PersonNotFound authorDetail.authorId.getId)
      let authorDetail_ =
            Common.AuthorDetail
              { authorId = Kernel.Types.Id.cast author.id,
                firstName = Just author.firstName,
                lastName = Just author.lastName
              }
      pure $
        Common.IssueReportCommentItem
          { authorDetail = authorDetail_,
            ..
          }

putIssueUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  IssueManagement.Common.Dashboard.Issue.IssueUpdateReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putIssueUpdate merchantShortId opCity apiTokenInfo issueId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI
      checkedMerchantId
      opCity
      (.issueDSL.putIssueUpdate)
      issueId
      (mkRequest req)
  where
    mkRequest IssueManagement.Common.Dashboard.Issue.IssueUpdateReq {..} =
      IssueManagement.Common.Dashboard.Issue.IssueUpdateByUserReq
        { userId = Kernel.Types.Id.cast apiTokenInfo.personId,
          ..
        }

postIssueComment ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  IssueManagement.Common.Dashboard.Issue.IssueAddCommentReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueComment merchantShortId opCity apiTokenInfo issueId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI
      checkedMerchantId
      opCity
      (.issueDSL.postIssueComment)
      issueId
      (mkRequest req)
  where
    mkRequest IssueManagement.Common.Dashboard.Issue.IssueAddCommentReq {..} =
      IssueManagement.Common.Dashboard.Issue.IssueAddCommentByUserReq
        { userId = Kernel.Types.Id.cast apiTokenInfo.personId,
          ..
        }

getIssueMedia ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Prelude.Text
getIssueMedia merchantShortId opCity apiTokenInfo filePath = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueMedia) filePath

postIssueTicketStatusCallBack ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Data.Aeson.Value ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueTicketStatusCallBack merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI
    checkedMerchantId
    opCity
    (.issueDSL.postIssueTicketStatusCallBack)
    req

postIssueCategoryCreate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryReq ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryRes
postIssueCategoryCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI
      checkedMerchantId
      opCity
      (.issueDSL.postIssueCategoryCreate)
      req

postIssueCategoryUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory ->
  IssueManagement.Common.Dashboard.Issue.UpdateIssueCategoryReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueCategoryUpdate merchantShortId opCity apiTokenInfo issueCategoryId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI
      checkedMerchantId
      opCity
      (.issueDSL.postIssueCategoryUpdate)
      issueCategoryId
      req

postIssueOptionCreate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage ->
  IssueManagement.Common.Dashboard.Issue.CreateIssueOptionReq ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.CreateIssueOptionRes
postIssueOptionCreate merchantShortId opCity apiTokenInfo issueCategoryId issueMessageId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI
      checkedMerchantId
      opCity
      (.issueDSL.postIssueOptionCreate)
      issueCategoryId
      issueMessageId
      req

postIssueOptionUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption ->
  IssueManagement.Common.Dashboard.Issue.UpdateIssueOptionReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueOptionUpdate merchantShortId opCity apiTokenInfo issueOptionid req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI
      checkedMerchantId
      opCity
      (.issueDSL.postIssueOptionUpdate)
      issueOptionid
      req

postIssueMessageUpsert ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageReq ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageRes
postIssueMessageUpsert merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI
      checkedMerchantId
      opCity
      (Dashboard.Common.addMultipartBoundary "XXX00XXX" . (.issueDSL.postIssueMessageUpsert))
      req

postIssueKaptureCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.IssueReportReqV2 -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIssueKaptureCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.postIssueKaptureCreate) req)

--- << AUTOGENERATED Check this code, update export list and remove comment >> ---

getIssueCategoryDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueCategoryDetailRes)
getIssueCategoryDetail merchantShortId opCity apiTokenInfo categoryId language = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueCategoryDetail) categoryId language

getIssueOptionDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueOptionDetailRes)
getIssueOptionDetail merchantShortId opCity apiTokenInfo optionId language = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueOptionDetail) optionId language

getIssueMessageDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueMessageDetailRes)
getIssueMessageDetail merchantShortId opCity apiTokenInfo messageId language = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueMessageDetail) messageId language

getIssueMessageList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe ((Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory)) -> Kernel.Prelude.Maybe ((Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption)) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueMessageListRes)
getIssueMessageList merchantShortId opCity apiTokenInfo categoryId optionId isActive language = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueMessageList) categoryId optionId isActive language

getIssueOptionList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe ((Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory)) -> Kernel.Prelude.Maybe ((Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage)) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueOptionListRes)
getIssueOptionList merchantShortId opCity apiTokenInfo categoryId messageId isActive language = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueOptionList) categoryId messageId isActive language

deleteIssueCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteIssueCategory merchantShortId opCity apiTokenInfo categoryId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.deleteIssueCategory) categoryId)

deleteIssueOption :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteIssueOption merchantShortId opCity apiTokenInfo optionId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.deleteIssueOption) optionId)

deleteIssueMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteIssueMessage merchantShortId opCity apiTokenInfo messageId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.deleteIssueMessage) messageId)

getIssueCategoryFlowPreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueCategoryFlowPreviewRes)
getIssueCategoryFlowPreview merchantShortId opCity apiTokenInfo categoryId language = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueCategoryFlowPreview) categoryId language

getIssueTranslations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueTranslationListRes)
getIssueTranslations merchantShortId opCity apiTokenInfo sentence = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueTranslations) sentence

postIssueBulkUpsertTranslations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.BulkUpsertTranslationsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIssueBulkUpsertTranslations merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.postIssueBulkUpsertTranslations) req)

getIssueConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueConfigRes)
getIssueConfig merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueConfig)

postIssueConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.UpdateIssueConfigReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIssueConfigUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.postIssueConfigUpdate) req)

postIssueCategoryReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.ReorderIssueCategoryReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIssueCategoryReorder merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.postIssueCategoryReorder) req)

postIssueOptionReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.ReorderIssueOptionReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIssueOptionReorder merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.postIssueOptionReorder) req)

postIssueMessageReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.ReorderIssueMessageReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIssueMessageReorder merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.postIssueMessageReorder) req)
