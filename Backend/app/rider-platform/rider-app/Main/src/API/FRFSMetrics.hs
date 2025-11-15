module API.FRFSMetrics where

import qualified Domain.Action.UI.GetDailyMetrics as DMetrics
import Environment
import Kernel.Prelude
import Kernel.Types.Common
-- import Kernel.Types.Id
import Servant

type API =
  "frfs" :> "daily"
    :> Header "x-api-key" Text
    :> Header "x-forwarded-for" Text
    :> Header "x-merchant-operating-city-id" Text
    :> MandatoryQueryParam "date" Text
    :> Get '[JSON] DMetrics.DailyMetricsResponse

handler :: FlowServer API
handler = getDailyMetricsWithAuth

getDailyMetricsWithAuth ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Text ->
  FlowHandler DMetrics.DailyMetricsResponse
getDailyMetricsWithAuth = DMetrics.getDailyMetricsWithAuth
