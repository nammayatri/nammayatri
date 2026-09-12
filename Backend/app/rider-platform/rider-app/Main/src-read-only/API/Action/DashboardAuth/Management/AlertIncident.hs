{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.AlertIncident
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.AlertIncident
import qualified Domain.Action.Dashboard.AlertIncident
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("alertIncident" :> GetAlertIncidentAlertsIncidents)

type GetAlertIncidentAlertsIncidents =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/ALERT_INCIDENT/GET_ALERT_INCIDENT_ALERTS_INCIDENTS"
      :> API.Types.RiderPlatform.Management.AlertIncident.GetAlertIncidentAlertsIncidents
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getAlertIncidentAlertsIncidents merchantId city

getAlertIncidentAlertsIncidents :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.RiderPlatform.Management.AlertIncident.AlertIncidentsResponse)
getAlertIncidentAlertsIncidents a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AlertIncident.getAlertIncidentAlertsIncidents a5 a4 a2 a1
