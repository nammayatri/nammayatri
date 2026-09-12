{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Operator.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Driver
import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Data.Time.Calendar
import qualified Domain.Action.Dashboard.Operator.Driver
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("driver" :> (GetDriverOperatorFetchHubRequests :<|> GetDriverOperationGetAllHubs :<|> PostDriverOperatorRespondHubRequest :<|> PostDriverOperatorCreateRequest :<|> GetDriverOperatorList :<|> PostDriverOperatorSendJoiningOtp :<|> PostDriverOperatorVerifyJoiningOtp :<|> GetDriverOperatorDashboardAnalyticsAllTime :<|> GetDriverOperatorDashboardAnalytics :<|> GetDriverReviewQueueRequest :<|> PostDriverSubmitReviewRequest :<|> GetDriverRequestReviewHistory))

type GetDriverOperatorFetchHubRequests =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_FETCH_HUB_REQUESTS"
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverOperatorFetchHubRequests
  )

type GetDriverOperationGetAllHubs =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATION_GET_ALL_HUBS"
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverOperationGetAllHubs
  )

type PostDriverOperatorRespondHubRequest =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_RESPOND_HUB_REQUEST"
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverOperatorRespondHubRequest
  )

type PostDriverOperatorCreateRequest =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_CREATE_REQUEST"
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverOperatorCreateRequest
  )

type GetDriverOperatorList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_LIST"
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverOperatorList
  )

type PostDriverOperatorSendJoiningOtp =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_SEND_JOINING_OTP"
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverOperatorSendJoiningOtp
  )

type PostDriverOperatorVerifyJoiningOtp =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_VERIFY_JOINING_OTP"
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverOperatorVerifyJoiningOtp
  )

type GetDriverOperatorDashboardAnalyticsAllTime =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_DASHBOARD_ANALYTICS_ALL_TIME"
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverOperatorDashboardAnalyticsAllTime
  )

type GetDriverOperatorDashboardAnalytics =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_DASHBOARD_ANALYTICS"
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverOperatorDashboardAnalytics
  )

type GetDriverReviewQueueRequest =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/GET_DRIVER_REVIEW_QUEUE_REQUEST"
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverReviewQueueRequest
  )

type PostDriverSubmitReviewRequest =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/POST_DRIVER_SUBMIT_REVIEW_REQUEST"
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverSubmitReviewRequest
  )

type GetDriverRequestReviewHistory =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/DRIVER/GET_DRIVER_REQUEST_REVIEW_HISTORY"
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverRequestReviewHistory
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverOperatorFetchHubRequests merchantId city :<|> getDriverOperationGetAllHubs merchantId city :<|> postDriverOperatorRespondHubRequest merchantId city :<|> postDriverOperatorCreateRequest merchantId city :<|> getDriverOperatorList merchantId city :<|> postDriverOperatorSendJoiningOtp merchantId city :<|> postDriverOperatorVerifyJoiningOtp merchantId city :<|> getDriverOperatorDashboardAnalyticsAllTime merchantId city :<|> getDriverOperatorDashboardAnalytics merchantId city :<|> getDriverReviewQueueRequest merchantId city :<|> postDriverSubmitReviewRequest merchantId city :<|> getDriverRequestReviewHistory merchantId city

getDriverOperatorFetchHubRequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Operator.Driver.RequestStatus) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Operator.Driver.RequestType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id API.Types.ProviderPlatform.Operator.Driver.OperationHub) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp)
getDriverOperatorFetchHubRequests a15 a14 _a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperatorFetchHubRequests a15 a14 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverOperationGetAllHubs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [API.Types.ProviderPlatform.Operator.Driver.OperationHub])
getDriverOperationGetAllHubs a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperationGetAllHubs a3 a2

postDriverOperatorRespondHubRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.Driver.RespondHubRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorRespondHubRequest a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverOperatorRespondHubRequest a4 a3 a1

postDriverOperatorCreateRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorCreateRequest a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverOperatorCreateRequest a4 a3 a1

getDriverOperatorList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Endpoints.Driver.DriverMode) -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.DriverInfoResp)
getDriverOperatorList a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperatorList a11 a10 a8 a7 a6 a5 a4 a3 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9)

postDriverOperatorSendJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverOperatorSendJoiningOtp a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverOperatorSendJoiningOtp a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postDriverOperatorVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Operator.Driver.VerifyOperatorJoiningOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorVerifyJoiningOtp a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverOperatorVerifyJoiningOtp a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1

getDriverOperatorDashboardAnalyticsAllTime :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.AllTimeOperatorAnalyticsRes)
getDriverOperatorDashboardAnalyticsAllTime a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperatorDashboardAnalyticsAllTime a3 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a1)

getDriverOperatorDashboardAnalytics :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Time.Calendar.Day -> Data.Time.Calendar.Day -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.FilteredOperatorAnalyticsRes)
getDriverOperatorDashboardAnalytics a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperatorDashboardAnalytics a5 a4 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2 a1

getDriverReviewQueueRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.Driver.EntityType -> API.Types.ProviderPlatform.Operator.Driver.ReviewRequestType -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.ReviewQueueResp)
getDriverReviewQueueRequest a14 a13 _a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverReviewQueueRequest a14 a13 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverSubmitReviewRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.Driver.SubmitReviewRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSubmitReviewRequest a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverSubmitReviewRequest a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

getDriverRequestReviewHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.Driver.EntityType -> API.Types.ProviderPlatform.Operator.Driver.ReviewRequestType -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Operator.Driver.ReviewRequestStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.ReviewRequestHistoryList)
getDriverRequestReviewHistory a15 a14 _a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverRequestReviewHistory a15 a14 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
