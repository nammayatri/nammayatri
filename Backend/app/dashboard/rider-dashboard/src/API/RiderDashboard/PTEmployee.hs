module API.RiderDashboard.PTEmployee where

import qualified Domain.Action.RiderDashboard.PTEmployee as DPTEmployee
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant
import "lib-dashboard" Storage.Beam.BeamFlow
import "lib-dashboard" Tools.Auth

type API =
  "ptEmployee"
    :> "bulkCreate"
    :> DashboardAuth 'DASHBOARD_ADMIN
    :> ReqBody '[JSON] DPTEmployee.BulkCreatePersonReq
    :> Post '[JSON] DPTEmployee.BulkCreatePersonResp

handler :: BeamFlow' => FlowServer API
handler = bulkCreate

bulkCreate :: BeamFlow' => TokenInfo -> DPTEmployee.BulkCreatePersonReq -> FlowHandler DPTEmployee.BulkCreatePersonResp
bulkCreate tokenInfo req = withFlowHandlerAPI' (DPTEmployee.bulkCreate tokenInfo.personId req)
