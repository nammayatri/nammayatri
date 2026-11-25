module API.UI.FleetSync
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Dispatcher as DDispatcher
import qualified Domain.Types.PartnerOrgConfig as DPOC
import Domain.Types.PartnerOrganization
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import qualified Kernel.Types.Logging as Log
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "fleetSync"
    :> FleetSyncAPIKey
    :> "scheduleChange"
    :> ReqBody '[JSON] DDispatcher.FleetScheduleChangeReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = scheduleChange

scheduleChange :: (PartnerOrganization, DPOC.FleetSyncConfig) -> DDispatcher.FleetScheduleChangeReq -> FlowHandler APISuccess
scheduleChange auth@(partnerOrg, _) req =
  withFlowHandlerAPI . Log.withLogTag ("FleetSync:PartnerOrgId:" <> getId partnerOrg.orgId) $
    DDispatcher.postFleetSyncScheduleChange auth req
