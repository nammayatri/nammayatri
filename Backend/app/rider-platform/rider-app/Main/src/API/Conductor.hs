module API.Conductor where

import qualified Domain.Action.UI.Conductor.Stats as DStats
import Environment
import Kernel.Prelude
import Kernel.Types.Common
import Servant

type API =
  "conductor" :> "stats" :> "summary"
    :> MandatoryQueryParam "conductor_token" Text
    :> Get '[JSON] DStats.StatsResp

handler :: FlowServer API
handler = DStats.statsHandler
