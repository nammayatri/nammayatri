{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.ProviderPlatform.IssueManagement.Issue
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
  )
where

import qualified API.Client.ProviderPlatform.IssueManagement
import qualified API.Types.ProviderPlatform.IssueManagement.Issue as Common
import qualified Dashboard.Common
import qualified Data.Aeson
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue
import qualified IssueManagement.Domain.Types.Issue.IssueCategory
import qualified IssueManagement.Domain.Types.Issue.IssueMessage
import qualified IssueManagement.Domain.Types.Issue.IssueOption
import qualified IssueManagement.Domain.Types.Issue.IssueReport
import Kernel.Beam.Functions
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

getIssueCategoryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes)
getIssueCategoryList merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueCategoryList)

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
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Common.Ride) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueReportListResponse
getIssueList merchantShortId opCity apiTokenInfo limit offset status category categoryName assignee countryCode phoneNumber rideShortId descriptionSearch = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueList) limit offset status category categoryName assignee countryCode phoneNumber rideShortId descriptionSearch

getIssueInfo ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueInfoRes
getIssueInfo merchantShortId opCity apiTokenInfo issueId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  addAuthorDetails =<< API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueInfo) issueId

getIssueInfoV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueInfoRes
getIssueInfoV2 merchantShortId opCity apiTokenInfo issueId issueShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  addAuthorDetails =<< API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueInfoV2) issueId issueShortId

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
      author <- runInReplica (QP.findById $ Kernel.Types.Id.cast authorDetail.authorId) >>= fromMaybeM (PersonNotFound authorDetail.authorId.getId)
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
putIssueUpdate merchantShortId opCity apiTokenInfo issueReportId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.putIssueUpdate) issueReportId (mkRequest req)
  where
    mkRequest Common.IssueUpdateReq {..} =
      Common.IssueUpdateByUserReq
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
postIssueComment merchantShortId opCity apiTokenInfo issueReportId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.postIssueComment) issueReportId (mkRequest req)
  where
    mkRequest Common.IssueAddCommentReq {..} =
      Common.IssueAddCommentByUserReq
        { userId = Kernel.Types.Id.cast apiTokenInfo.personId,
          ..
        }

getIssueMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow Kernel.Prelude.Text)
getIssueMedia merchantShortId opCity apiTokenInfo filePath = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.getIssueMedia) filePath

postIssueTicketStatusCallBack :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIssueTicketStatusCallBack merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.postIssueTicketStatusCallBack) req

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
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI
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
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI
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
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI
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
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI checkedMerchantId opCity (.issueDSL.postIssueOptionUpdate) issueOptionid req

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
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.IssueManagement.callIssueManagementAPI
      checkedMerchantId
      opCity
      (Dashboard.Common.addMultipartBoundary "XXX00XXX" . (.issueDSL.postIssueMessageUpsert))
      req
