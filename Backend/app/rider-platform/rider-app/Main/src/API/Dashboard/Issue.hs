module API.Dashboard.Issue where

import qualified Domain.Types.Merchant as DM
import Environment
import qualified IssueManagement.API.Dashboard.Issue as IMD
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Common.Dashboard.Issue as Common
import qualified IssueManagement.Domain.Action.Dashboard.Issue as DIssue
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueReport
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QP

type API = IMD.DashboardIssueAPIV2

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  issueCategoryList merchantId city
    :<|> issueList merchantId city
    :<|> issueInfo merchantId city
    :<|> issueUpdate merchantId city
    :<|> issueAddComment merchantId city
    :<|> issueFetchMedia merchantId city
    :<|> ticketStatusCallBack merchantId city

dashboardIssueHandle :: DIssue.ServiceHandle Flow
dashboardIssueHandle =
  DIssue.ServiceHandle
    { findPersonById = castPersonById,
      findByMerchantShortIdAndCity = castfindByMerchantShortIdAndCity
    }

castPersonById :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Common.Person -> m (Maybe Common.Person)
castPersonById driverId = do
  person <- runInReplica $ QP.findById (cast driverId)
  return $ fmap castPerson person
  where
    castPerson person =
      Common.Person
        { id = cast person.id,
          language = person.language,
          firstName = person.firstName,
          lastName = person.lastName,
          middleName = person.middleName,
          mobileNumber = person.mobileNumber,
          merchantOperatingCityId = cast person.merchantOperatingCityId
        }

castfindByMerchantShortIdAndCity :: (CacheFlow m r, EsqDBFlow m r) => ShortId Common.Merchant -> Context.City -> m (Maybe Common.MerchantOperatingCity)
castfindByMerchantShortIdAndCity (ShortId merchantShortId) opCity = do
  merchantOpCity <- CQMOC.findByMerchantShortIdAndCity (ShortId merchantShortId) opCity
  return $ fmap castMerchantOperatingCity merchantOpCity
  where
    castMerchantOperatingCity moCity =
      Common.MerchantOperatingCity
        { id = cast moCity.id,
          city = moCity.city,
          merchantId = cast moCity.merchantId
        }

issueCategoryList ::
  ShortId DM.Merchant ->
  Context.City ->
  FlowHandler Common.IssueCategoryListRes
issueCategoryList (ShortId merchantShortId) city = withFlowHandlerAPI $ DIssue.issueCategoryList (ShortId merchantShortId) city Common.CUSTOMER

issueList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.IssueStatus ->
  Maybe (Id IssueCategory) ->
  Maybe Text ->
  FlowHandler Common.IssueReportListResponse
issueList (ShortId merchantShortId) city mbLimit mbOffset mbStatus mbCategoryId mbAssignee = withFlowHandlerAPI $ DIssue.issueList (ShortId merchantShortId) city mbLimit mbOffset mbStatus (cast <$> mbCategoryId) mbAssignee dashboardIssueHandle Common.CUSTOMER

issueInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Id IssueReport ->
  FlowHandler Common.IssueInfoRes
issueInfo (ShortId merchantShortId) city issueReportId = withFlowHandlerAPI $ DIssue.issueInfo (ShortId merchantShortId) city (cast issueReportId) dashboardIssueHandle Common.CUSTOMER

issueUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Id IssueReport ->
  Common.IssueUpdateByUserReq ->
  FlowHandler APISuccess
issueUpdate (ShortId merchantShortId) city issueReportId req = withFlowHandlerAPI $ DIssue.issueUpdate (ShortId merchantShortId) city (cast issueReportId) dashboardIssueHandle req

issueAddComment ::
  ShortId DM.Merchant ->
  Context.City ->
  Id IssueReport ->
  Common.IssueAddCommentByUserReq ->
  FlowHandler APISuccess
issueAddComment (ShortId merchantShortId) city issueReportId req = withFlowHandlerAPI $ DIssue.issueAddComment (ShortId merchantShortId) city (cast issueReportId) dashboardIssueHandle req

issueFetchMedia :: ShortId DM.Merchant -> Context.City -> Text -> FlowHandler Text
issueFetchMedia (ShortId merchantShortId) _ = withFlowHandlerAPI . DIssue.issueFetchMedia (ShortId merchantShortId)

ticketStatusCallBack :: ShortId DM.Merchant -> Context.City -> Common.TicketStatusCallBackReq -> FlowHandler APISuccess
ticketStatusCallBack (ShortId merchantShortId) city = withFlowHandlerAPI . DIssue.ticketStatusCallBack (ShortId merchantShortId) city Common.CUSTOMER
