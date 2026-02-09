{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.AlertWebhook (postApiV1AlertsUpdate) where

import qualified API.Types.UI.AlertWebhook as Types
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, diffUTCTime)
import qualified Domain.Types.AlertIncident as Domain
import qualified Environment
import EulerHS.Prelude hiding (forM_, id, length)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.AlertIncident as QAlertIncident

-- | Process VictoriaMetrics vmalert webhook
-- Handles both 'firing' and 'resolved' alert statuses
postApiV1AlertsUpdate :: Maybe Bool -> Types.VmAlertWebhookReq -> Environment.Flow APISuccess.APISuccess
postApiV1AlertsUpdate isManual req = do
  logInfo $
    "Received alert webhook: status=" <> req.status
      <> ", alerts_count="
      <> show (length req.alerts)
      <> ", receiver="
      <> fromMaybe "unknown" req.receiver

  -- Serialize the entire request to JSON for rawPayload
  let rawPayload = decodeUtf8 $ A.encode req

  forM_ req.alerts $ \alert -> do
    case T.toLower req.status of
      "firing" -> handleFiringAlert isManual req alert rawPayload
      "resolved" -> handleResolvedAlert req alert rawPayload
      unknownStatus -> do
        logError $ "Unknown alert status: " <> unknownStatus <> " for alert: " <> alert.labels.alertname
        -- Don't throw error to avoid blocking other valid alerts
        pure ()

  pure APISuccess.Success

-- | Helper to get service name from labels
getServiceName :: Types.AlertLabels -> Text
getServiceName labels = fromMaybe "unknown" (labels.alert)

-- | Handle firing alert - create new incident if not already exists
handleFiringAlert :: Maybe Bool -> Types.VmAlertWebhookReq -> Types.AlertDetail -> Text -> Environment.Flow ()
handleFiringAlert isManual req alert rawPayload = do
  let alertName = alert.labels.alertname
      serviceName = getServiceName alert.labels
      alertGroup' = alert.labels.alertgroup
      description' = alert.annotations.description
      severity' = alert.labels.severity
      firingTime = alert.startsAt
      receiver' = req.receiver
      externalURL' = req.externalURL

  -- Query: Check if FIRING incident with same description already exists
  existingIncidents <-
    QAlertIncident.findFiringIncident
      (Just 1) -- limit
      Nothing -- offset
      description'
      Domain.FIRING

  case existingIncidents of
    (existingIncident : _) -> do
      -- Incident already exists, ignore duplicate
      logInfo $
        "Ignoring duplicate firing alert for incident: " <> show existingIncident.id
          <> ", alertName: "
          <> alertName
          <> ", service: "
          <> serviceName
    [] -> do
      -- No existing firing incident, create new one
      logInfo $
        "Creating new incident for alert: " <> alertName
          <> ", service: "
          <> serviceName
          <> ", description: "
          <> fromMaybe "N/A" description'

      incidentId <- generateGUID
      now <- getCurrentTime

      let incident =
            Domain.AlertIncident
              { id = incidentId,
                alertName = alertName,
                serviceName = serviceName,
                alertGroup = alertGroup',
                description = description',
                severity = severity',
                firingTime = firingTime,
                resolvedTime = Nothing,
                downtimeSeconds = Nothing,
                status = Domain.FIRING,
                receiver = receiver',
                externalURL = externalURL',
                rawPayload = Just rawPayload,
                isManuallyEntered = isManual,
                createdAt = now,
                updatedAt = now
              }

      QAlertIncident.create incident
      logInfo $
        "Created incident with id: " <> show incidentId
          <> " for alert: "
          <> alertName

-- | Handle resolved alert - find unresolved incident and update it
handleResolvedAlert :: Types.VmAlertWebhookReq -> Types.AlertDetail -> Text -> Environment.Flow ()
handleResolvedAlert _req alert _rawPayload = do
  let alertName = alert.labels.alertname
      serviceName = getServiceName alert.labels
      description' = alert.annotations.description
      resolvedTime = alert.endsAt

  -- Query: Find unresolved incident (resolvedTime is NULL) with same description
  unresolvedIncidents <-
    QAlertIncident.findIncidentToResolve
      (Just 1) -- limit
      Nothing -- offset
      description'
      Nothing -- resolvedTime IS NULL
  case unresolvedIncidents of
    (incident : _) -> do
      -- Update the incident with resolved time and downtime
      let downtimeSeconds = calculateDowntime incident.firingTime resolvedTime

      logInfo $
        "Resolving incident " <> show incident.id
          <> " for alert: "
          <> alertName
          <> ", service: "
          <> serviceName
          <> ", downtime: "
          <> show downtimeSeconds
          <> "s"

      QAlertIncident.updateToResolved
        Domain.RESOLVED
        (Just resolvedTime)
        (Just downtimeSeconds)
        incident.id

      logInfo $ "Successfully resolved incident " <> show incident.id
    [] -> do
      -- No unresolved incident found
      logWarning $
        "No unresolved incident found for resolved alert: "
          <> alertName
          <> ", service: "
          <> serviceName
          <> ", description: "
          <> fromMaybe "N/A" description'
          <> ". This might happen if the firing alert never arrived or was already resolved."

-- | Calculate downtime in seconds between firing and resolved times
calculateDowntime :: UTCTime -> UTCTime -> Int
calculateDowntime firingTime resolvedTime =
  floor $ diffUTCTime resolvedTime firingTime
