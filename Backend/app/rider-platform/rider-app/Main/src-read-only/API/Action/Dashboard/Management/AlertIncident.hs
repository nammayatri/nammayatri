{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.AlertIncident
  ( API.Types.RiderPlatform.Management.AlertIncident.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.AlertIncident.API)
handler merchantId city = getAlertIncidentAlertsIncidents merchantId city

getAlertIncidentAlertsIncidents :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.RiderPlatform.Management.AlertIncident.AlertIncidentsResponse)
getAlertIncidentAlertsIncidents a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AlertIncident.getAlertIncidentAlertsIncidents a4 a3 a2 a1
