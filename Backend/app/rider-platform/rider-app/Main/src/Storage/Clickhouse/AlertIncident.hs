{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Clickhouse.AlertIncident where

import Data.Time
import qualified Domain.Types.AlertIncident as Domain
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import Kernel.Types.Id

data AlertIncidentT f = AlertIncidentT
  { incidentId :: C f Text,
    alertName :: C f Text,
    serviceName :: C f Text,
    alertGroup :: C f (Maybe Text),
    description :: C f (Maybe Text),
    severity :: C f (Maybe Text),
    firingTime :: C f UTCTime,
    resolvedTime :: C f (Maybe UTCTime),
    downtimeSeconds :: C f (Maybe Int),
    status :: C f Text,
    receiver :: C f (Maybe Text),
    externalURL :: C f (Maybe Text),
    isManuallyEntered :: C f (Maybe Bool),
    createdAt :: C f UTCTime,
    updatedAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show AlertIncident

alertIncidentTTable :: AlertIncidentT (FieldModification AlertIncidentT)
alertIncidentTTable =
  AlertIncidentT
    { incidentId = "id",
      alertName = "alert_name",
      serviceName = "service_name",
      alertGroup = "alert_group",
      description = "description",
      severity = "severity",
      firingTime = "firing_time",
      resolvedTime = "resolved_time",
      downtimeSeconds = "downtime_seconds",
      status = "status",
      receiver = "receiver",
      externalURL = "external_url",
      isManuallyEntered = "is_manually_entered",
      createdAt = "created_at",
      updatedAt = "updated_at"
    }

type AlertIncident = AlertIncidentT Identity

$(TH.mkClickhouseInstances ''AlertIncidentT 'NO_SELECT_MODIFIER)

-- | Fetch alert incidents from ClickHouse with time range filtering
findIncidentsByTimeRange ::
  (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  UTCTime ->
  UTCTime ->
  m [Domain.AlertIncident]
findIncidentsByTimeRange fromTime toTime = do
  -- Query to get incidents
  incidents <-
    CH.findAll $
      CH.select_ (\incident -> CH.notGrouped incident) $
        CH.filter_
          ( \incident ->
              incident.firingTime >=. fromTime
                CH.&&. incident.firingTime <. toTime
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE alertIncidentTTable)

  pure (map toAlertIncident incidents)

-- | Convert ClickHouse row to domain type
toAlertIncident :: AlertIncident -> Domain.AlertIncident
toAlertIncident incident =
  Domain.AlertIncident
    { id = Id incident.incidentId,
      alertName = incident.alertName,
      serviceName = incident.serviceName,
      alertGroup = incident.alertGroup,
      description = incident.description,
      severity = incident.severity,
      firingTime = incident.firingTime,
      resolvedTime = incident.resolvedTime,
      downtimeSeconds = incident.downtimeSeconds,
      status = parseStatus incident.status,
      receiver = incident.receiver,
      externalURL = incident.externalURL,
      rawPayload = Nothing, -- Not fetched from ClickHouse for performance
      isManuallyEntered = incident.isManuallyEntered,
      createdAt = incident.createdAt,
      updatedAt = incident.updatedAt
    }

-- | Parse status string to IncidentStatus
parseStatus :: Text -> Domain.IncidentStatus
parseStatus "FIRING" = Domain.FIRING
parseStatus "RESOLVED" = Domain.RESOLVED
parseStatus _ = Domain.FIRING -- Default fallback
