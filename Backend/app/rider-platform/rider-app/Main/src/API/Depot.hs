module API.Depot where

import qualified Domain.Action.UI.Depot.Stats as DStats
import Environment
import Servant

type API =
  "depot" :> "stats" :> "summary"
    :> ReqBody '[JSON] DStats.DepotStatsReq
    :> Post '[JSON] DStats.DepotStatsResp

handler :: FlowServer API
handler = DStats.depotStatsHandler
