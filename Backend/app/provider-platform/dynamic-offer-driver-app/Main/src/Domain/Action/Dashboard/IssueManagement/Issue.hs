module Domain.Action.Dashboard.IssueManagement.Issue
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

import qualified API.UI.Issue as AUI
import qualified Data.Aeson
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Common.Dashboard.Issue
import qualified IssueManagement.Domain.Action.Dashboard.Issue as DIssue
import qualified IssueManagement.Domain.Action.UI.Issue as DAI
import qualified IssueManagement.Domain.Types.Issue.IssueCategory
import qualified IssueManagement.Domain.Types.Issue.IssueMessage
import qualified IssueManagement.Domain.Types.Issue.IssueOption
import qualified IssueManagement.Domain.Types.Issue.IssueReport
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id

dashboardIssueHandle :: DAI.ServiceHandle Environment.Flow
dashboardIssueHandle = AUI.driverIssueHandle

getIssueCategoryList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes
getIssueCategoryList (Kernel.Types.Id.ShortId merchantShortId) opCity =
  DIssue.issueCategoryList (Kernel.Types.Id.ShortId merchantShortId) opCity dashboardIssueHandle Common.DRIVER

getIssueList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Common.IssueStatus ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Common.Ride) ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueReportListResponse
getIssueList (Kernel.Types.Id.ShortId merchantShortId) opCity mbLimit mbOffset mbStatus mbCategoryId mbAssignee mbCountryCode mbMobileNumber mbRideShortId =
  DIssue.issueList (Kernel.Types.Id.ShortId merchantShortId) opCity mbLimit mbOffset mbStatus (Kernel.Types.Id.cast <$> mbCategoryId) mbAssignee mbCountryCode mbMobileNumber mbRideShortId dashboardIssueHandle Common.DRIVER

getIssueInfo ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueInfoRes
getIssueInfo (Kernel.Types.Id.ShortId merchantShortId) opCity issueReportId =
  DIssue.issueInfo (Kernel.Types.Id.ShortId merchantShortId) opCity (Just issueReportId) Nothing dashboardIssueHandle Common.DRIVER

getIssueInfoV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.IssueInfoRes
getIssueInfoV2 (Kernel.Types.Id.ShortId merchantShortId) opCity mbIssueReportId mbIssueReportShortId =
  DIssue.issueInfo (Kernel.Types.Id.ShortId merchantShortId) opCity mbIssueReportId mbIssueReportShortId dashboardIssueHandle Common.DRIVER

putIssueUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  IssueManagement.Common.Dashboard.Issue.IssueUpdateByUserReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putIssueUpdate (Kernel.Types.Id.ShortId merchantShortId) opCity issueReportId req =
  DIssue.issueUpdate (Kernel.Types.Id.ShortId merchantShortId) opCity (Kernel.Types.Id.cast issueReportId) dashboardIssueHandle req

postIssueComment ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport ->
  IssueManagement.Common.Dashboard.Issue.IssueAddCommentByUserReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueComment (Kernel.Types.Id.ShortId merchantShortId) opCity issueReportId req =
  DIssue.issueAddComment (Kernel.Types.Id.ShortId merchantShortId) opCity (Kernel.Types.Id.cast issueReportId) dashboardIssueHandle req

getIssueMedia ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Prelude.Text
getIssueMedia (Kernel.Types.Id.ShortId merchantShortId) _opCity = DIssue.issueFetchMedia (Kernel.Types.Id.ShortId merchantShortId)

postIssueTicketStatusCallBack ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Data.Aeson.Value ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueTicketStatusCallBack (Kernel.Types.Id.ShortId _merchantShortId) _opCity req = DIssue.ticketStatusCallBack req dashboardIssueHandle Common.DRIVER

postIssueCategoryCreate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryReq ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryRes
postIssueCategoryCreate (Kernel.Types.Id.ShortId merchantShortId) city req =
  DIssue.createIssueCategory (Kernel.Types.Id.ShortId merchantShortId) city req dashboardIssueHandle Common.DRIVER

postIssueCategoryUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory ->
  IssueManagement.Common.Dashboard.Issue.UpdateIssueCategoryReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueCategoryUpdate (Kernel.Types.Id.ShortId merchantShortId) city issueCategoryId req =
  DIssue.updateIssueCategory (Kernel.Types.Id.ShortId merchantShortId) city issueCategoryId req dashboardIssueHandle Common.DRIVER

postIssueOptionCreate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage ->
  IssueManagement.Common.Dashboard.Issue.CreateIssueOptionReq ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.CreateIssueOptionRes
postIssueOptionCreate (Kernel.Types.Id.ShortId merchantShortId) city issueCategoryId issueMessageId req =
  DIssue.createIssueOption (Kernel.Types.Id.ShortId merchantShortId) city Nothing issueCategoryId issueMessageId dashboardIssueHandle req Common.DRIVER

postIssueOptionUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption ->
  IssueManagement.Common.Dashboard.Issue.UpdateIssueOptionReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueOptionUpdate (Kernel.Types.Id.ShortId merchantShortId) city issueOptionId req =
  DIssue.updateIssueOption (Kernel.Types.Id.ShortId merchantShortId) city issueOptionId req dashboardIssueHandle Common.DRIVER

postIssueMessageUpsert ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageReq ->
  Environment.Flow IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageRes
postIssueMessageUpsert (Kernel.Types.Id.ShortId merchantShortId) city req =
  DIssue.upsertIssueMessage (Kernel.Types.Id.ShortId merchantShortId) city req dashboardIssueHandle Common.DRIVER
