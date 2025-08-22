{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Operator.Driver
  ( API.Types.ProviderPlatform.Operator.Driver.API,
    handler,
  )
where

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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Operator.Driver.API)
handler merchantId city = getDriverOperatorFetchHubRequests merchantId city :<|> getDriverOperationGetAllHubs merchantId city :<|> postDriverOperatorRespondHubRequest merchantId city :<|> postDriverOperatorCreateRequest merchantId city :<|> getDriverOperatorList merchantId city :<|> postDriverOperatorSendJoiningOtp merchantId city :<|> postDriverOperatorVerifyJoiningOtp merchantId city :<|> getDriverOperatorDashboardAnalyticsAllTime merchantId city :<|> getDriverOperatorDashboardAnalytics merchantId city

getDriverOperatorFetchHubRequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Operator.Driver.RequestStatus -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Operator.Driver.RequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id API.Types.ProviderPlatform.Operator.Driver.OperationHub) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp)
getDriverOperatorFetchHubRequests a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperatorFetchHubRequests a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverOperationGetAllHubs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler [API.Types.ProviderPlatform.Operator.Driver.OperationHub])
getDriverOperationGetAllHubs a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperationGetAllHubs a2 a1

postDriverOperatorRespondHubRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Driver.RespondHubRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorRespondHubRequest a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverOperatorRespondHubRequest a3 a2 a1

postDriverOperatorCreateRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorCreateRequest a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverOperatorCreateRequest a3 a2 a1

getDriverOperatorList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.DriverInfoResp)
getDriverOperatorList a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperatorList a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverOperatorSendJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverOperatorSendJoiningOtp a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverOperatorSendJoiningOtp a4 a3 a2 a1

postDriverOperatorVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Operator.Driver.VerifyOperatorJoiningOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorVerifyJoiningOtp a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverOperatorVerifyJoiningOtp a5 a4 a3 a2 a1

getDriverOperatorDashboardAnalyticsAllTime :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.AllTimeOperatorAnalyticsRes)
getDriverOperatorDashboardAnalyticsAllTime a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperatorDashboardAnalyticsAllTime a3 a2 a1

getDriverOperatorDashboardAnalytics :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> Data.Time.Calendar.Day -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.FilteredOperatorAnalyticsRes)
getDriverOperatorDashboardAnalytics a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperatorDashboardAnalytics a5 a4 a3 a2 a1
