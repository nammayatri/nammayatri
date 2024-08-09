module API.Dashboard.Management.Issue where

import qualified API.UI.Issue as AUI
import qualified Data.Aeson as A
import qualified Domain.Types.Merchant as DM
import Environment
import qualified IssueManagement.API.Dashboard.Issue as IMD
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Common.Dashboard.Issue as Common
import qualified IssueManagement.Domain.Action.Dashboard.Issue as DIssue
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.Issue.IssueReport
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (Unauthorized, throwError)
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()

type API = IMD.DashboardIssueAPI

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  issueCategoryList merchantId city
    :<|> issueList merchantId city
    :<|> issueInfo merchantId city
    :<|> issueInfoV2 merchantId city
    :<|> issueUpdate merchantId city
    :<|> issueAddComment merchantId city
    :<|> issueFetchMedia merchantId city
    :<|> ticketStatusCallBack merchantId city
    :<|> createIssueCategory merchantId city
    :<|> updateIssueCategory merchantId city
    :<|> createIssueOption merchantId city
    :<|> updateIssueOption merchantId city
    :<|> upsertIssueMessage merchantId city

dashboardIssueHandle :: DIssue.ServiceHandle Flow
dashboardIssueHandle =
  DIssue.ServiceHandle
    { findPersonById = AUI.castPersonById,
      findByMerchantShortIdAndCity = AUI.castMOCityByMerchantShortIdAndCity,
      findMerchantConfig = AUI.buildMerchantConfig,
      mbSendUnattendedTicketAlert = Nothing
    }

issueCategoryList ::
  ShortId DM.Merchant ->
  Context.City ->
  FlowHandler Common.IssueCategoryListRes
issueCategoryList (ShortId merchantShortId) opCity = withFlowHandlerAPI $ DIssue.issueCategoryList (ShortId merchantShortId) opCity dashboardIssueHandle Common.DRIVER

issueList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.IssueStatus ->
  Maybe (Id IssueCategory) ->
  Maybe Text ->
  FlowHandler Common.IssueReportListResponse
issueList (ShortId merchantShortId) opCity mbLimit mbOffset mbStatus mbCategoryId mbAssignee = withFlowHandlerAPI $ DIssue.issueList (ShortId merchantShortId) opCity mbLimit mbOffset mbStatus (cast <$> mbCategoryId) mbAssignee dashboardIssueHandle Common.DRIVER

issueInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Id IssueReport ->
  FlowHandler Common.IssueInfoRes
issueInfo (ShortId merchantShortId) opCity issueReportId = withFlowHandlerAPI $ DIssue.issueInfo (ShortId merchantShortId) opCity (Just issueReportId) Nothing dashboardIssueHandle Common.DRIVER

issueInfoV2 ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe (Id IssueReport) ->
  Maybe (ShortId IssueReport) ->
  FlowHandler Common.IssueInfoRes
issueInfoV2 (ShortId merchantShortId) opCity mbIssueReportId mbIssueReportShortId = withFlowHandlerAPI $ DIssue.issueInfo (ShortId merchantShortId) opCity mbIssueReportId mbIssueReportShortId dashboardIssueHandle Common.DRIVER

issueUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Id IssueReport ->
  Common.IssueUpdateByUserReq ->
  FlowHandler APISuccess
issueUpdate (ShortId merchantShortId) opCity issueReportId req = withFlowHandlerAPI $ DIssue.issueUpdate (ShortId merchantShortId) opCity (cast issueReportId) dashboardIssueHandle req

issueAddComment ::
  ShortId DM.Merchant ->
  Context.City ->
  Id IssueReport ->
  Common.IssueAddCommentByUserReq ->
  FlowHandler APISuccess
issueAddComment (ShortId merchantShortId) opCity issueReportId req = withFlowHandlerAPI $ DIssue.issueAddComment (ShortId merchantShortId) opCity (cast issueReportId) dashboardIssueHandle req

issueFetchMedia :: ShortId DM.Merchant -> Context.City -> Text -> FlowHandler Text
issueFetchMedia (ShortId merchantShortId) _opCity = withFlowHandlerAPI . DIssue.issueFetchMedia (ShortId merchantShortId)

ticketStatusCallBack :: ShortId DM.Merchant -> Context.City -> A.Value -> FlowHandler APISuccess
ticketStatusCallBack (ShortId _merchantShortId) _opCity req = withFlowHandlerAPI $ DIssue.ticketStatusCallBack req dashboardIssueHandle Common.DRIVER

createIssueCategory :: ShortId DM.Merchant -> Context.City -> Common.CreateIssueCategoryReq -> FlowHandler APISuccess
createIssueCategory (ShortId merchantShortId) city req = withFlowHandlerAPI $ DIssue.createIssueCategory (ShortId merchantShortId) city req dashboardIssueHandle Common.DRIVER

updateIssueCategory :: ShortId DM.Merchant -> Context.City -> Id IssueCategory -> Common.UpdateIssueCategoryReq -> FlowHandler APISuccess
updateIssueCategory (ShortId merchantShortId) city issueCategoryId req = withFlowHandlerAPI $ DIssue.updateIssueCategory (ShortId merchantShortId) city issueCategoryId req dashboardIssueHandle Common.DRIVER

createIssueOption :: ShortId DM.Merchant -> Context.City -> Id IssueCategory -> Id IssueMessage -> Common.CreateIssueOptionReq -> FlowHandler APISuccess
createIssueOption (ShortId merchantShortId) city issueCategoryId issueMessageId req = withFlowHandlerAPI $ DIssue.createIssueOption (ShortId merchantShortId) city Nothing issueCategoryId issueMessageId dashboardIssueHandle req Common.DRIVER

updateIssueOption :: ShortId DM.Merchant -> Context.City -> Id IssueOption -> Common.UpdateIssueOptionReq -> FlowHandler APISuccess
updateIssueOption (ShortId merchantShortId) city issueOptionId req = withFlowHandlerAPI $ DIssue.updateIssueOption (ShortId merchantShortId) city issueOptionId req dashboardIssueHandle Common.DRIVER

upsertIssueMessage :: ShortId DM.Merchant -> Context.City -> Common.UpsertIssueMessageReq -> FlowHandler APISuccess
upsertIssueMessage (ShortId merchantShortId) city req = withFlowHandlerAPI $ DIssue.upsertIssueMessage (ShortId merchantShortId) city req dashboardIssueHandle Common.DRIVER
