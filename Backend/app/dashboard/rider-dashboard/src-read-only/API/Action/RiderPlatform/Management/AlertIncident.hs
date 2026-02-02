{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.AlertIncident
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.AlertIncident
import qualified Domain.Action.RiderPlatform.Management.AlertIncident
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("alertIncident" :> GetAlertIncidentAlertsIncidents)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getAlertIncidentAlertsIncidents merchantId city

type GetAlertIncidentAlertsIncidents =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.ALERT_INCIDENT) / ('API.Types.RiderPlatform.Management.AlertIncident.GET_ALERT_INCIDENT_ALERTS_INCIDENTS))
      :> API.Types.RiderPlatform.Management.AlertIncident.GetAlertIncidentAlertsIncidents
  )

getAlertIncidentAlertsIncidents :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.RiderPlatform.Management.AlertIncident.AlertIncidentsResponse)
getAlertIncidentAlertsIncidents merchantShortId opCity apiTokenInfo fromTime toTime = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.AlertIncident.getAlertIncidentAlertsIncidents merchantShortId opCity apiTokenInfo fromTime toTime
