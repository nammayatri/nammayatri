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
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import Tools.Auth
import Tools.Error

type API =
  "fleetSync"
    :> PartnerOrganizationAPIKey
    :> "scheduleChange"
    :> ReqBody '[JSON] DDispatcher.FleetScheduleChangeReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = scheduleChange

scheduleChange :: PartnerOrganization -> DDispatcher.FleetScheduleChangeReq -> FlowHandler APISuccess
scheduleChange partnerOrg req =
  withFlowHandlerAPI . Log.withLogTag ("FleetSync:PartnerOrgId:" <> getId partnerOrg.orgId) $ do
    fleetSyncCfg <- CQPOC.findByIdAndCfgType partnerOrg.orgId DPOC.FLEET_SYNC >>= fromMaybeM (InvalidAPIKey "") >>= DPOC.getFleetSyncConfig . (.config)
    DDispatcher.postFleetSyncScheduleChange (partnerOrg, fleetSyncCfg) req
