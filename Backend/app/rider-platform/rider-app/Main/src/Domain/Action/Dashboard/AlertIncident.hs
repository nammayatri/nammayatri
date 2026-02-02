{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AlertIncident (getAlertIncidentAlertsIncidents) where

import qualified API.Types.RiderPlatform.Management.AlertIncident as Types
import qualified Domain.Types.AlertIncident as Domain
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Clickhouse.AlertIncident as CHAlertIncident
import Tools.Auth

-- | Fetch alert incidents from ClickHouse with optional time filtering
-- Defaults to last 24 hours if no time range provided
getAlertIncidentAlertsIncidents ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Kernel.Prelude.UTCTime ->
  Maybe Kernel.Prelude.UTCTime ->
  Environment.Flow Types.AlertIncidentsResponse
getAlertIncidentAlertsIncidents _merchantShortId _opCity fromTime toTime = do
  -- Default to last 24 hours if no time range provided
  now <- getCurrentTime
  let actualFromTime = fromMaybe (addUTCTime (-86400) now) fromTime
      actualToTime = fromMaybe now toTime

  logInfo $
    "Fetching alert incidents from ClickHouse: fromTime="
      <> show actualFromTime
      <> ", toTime="
      <> show actualToTime

  -- Fetch from ClickHouse
  incidents <- CHAlertIncident.findIncidentsByTimeRange actualFromTime actualToTime

  let totalCount = length incidents
  logInfo $ "Found " <> show totalCount <> " incidents"

  -- Convert to response type
  let incidentInfos = map toAlertIncidentInfo incidents

  pure $
    Types.AlertIncidentsResponse
      { incidents = incidentInfos,
        totalCount = totalCount
      }

-- | Convert domain type to API response type
toAlertIncidentInfo :: Domain.AlertIncident -> Types.AlertIncidentInfo
toAlertIncidentInfo incident =
  Types.AlertIncidentInfo
    { id = Kernel.Types.Id.getId incident.id, -- Convert Id to Text
      alertName = incident.alertName,
      serviceName = incident.serviceName,
      alertGroup = incident.alertGroup,
      description = incident.description,
      severity = incident.severity,
      firingTime = incident.firingTime,
      resolvedTime = incident.resolvedTime,
      downtimeSeconds = incident.downtimeSeconds,
      status = toApiStatus incident.status,
      receiver = incident.receiver,
      externalURL = incident.externalURL,
      createdAt = incident.createdAt
    }

-- | Convert domain status to API status
toApiStatus :: Domain.IncidentStatus -> Types.IncidentStatus
toApiStatus Domain.FIRING = Types.FIRING
toApiStatus Domain.RESOLVED = Types.RESOLVED
