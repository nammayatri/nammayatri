module API.FRFSMetrics where

import qualified Domain.Action.UI.GetDailyMetrics as DMetrics
import Environment
import Kernel.Prelude
-- import Kernel.Types.Id
import Servant

type API =
  "frfs" :> "daily"
    :> Header "x-api-key" Text
    :> Header "x-forwarded-for" Text
    :> ReqBody '[JSON] DMetrics.DailyMetricsRequest
    :> Post '[JSON] DMetrics.DailyMetricsResponse

handler :: FlowServer API
handler = getDailyMetricsWithAuth

getDailyMetricsWithAuth ::
  Maybe Text ->
  Maybe Text ->
  DMetrics.DailyMetricsRequest ->
  FlowHandler DMetrics.DailyMetricsResponse
getDailyMetricsWithAuth = DMetrics.getDailyMetricsWithAuth
