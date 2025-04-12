{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Operator.Driver
  ( API.Types.ProviderPlatform.Operator.Driver.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Operator.Driver as Domain.Action.Dashboard.Operator.Driver
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
handler merchantId city = getDriverOperatorFetchHubRequests merchantId city :<|> postDriverOperatorRespondHubRequest merchantId city :<|> postDriverOperatorCreateRequest merchantId city

getDriverOperatorFetchHubRequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Operator.Driver.RequestStatus -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Operator.Driver.RequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.OperationHub) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp)
getDriverOperatorFetchHubRequests a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.getDriverOperatorFetchHubRequests a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverOperatorRespondHubRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Driver.RespondHubRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorRespondHubRequest a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverOperatorRespondHubRequest a3 a2 a1

postDriverOperatorCreateRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorCreateRequest a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Driver.postDriverOperatorCreateRequest a3 a2 a1
