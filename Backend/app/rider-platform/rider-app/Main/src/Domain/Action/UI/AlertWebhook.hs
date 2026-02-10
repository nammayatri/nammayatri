{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.AlertWebhook (postApiV1AlertsUpdate) where

import qualified API.Types.UI.AlertWebhook as Types
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, diffUTCTime)
import qualified Domain.Types.AlertIncident as Domain
import qualified Domain.Types.AlertIncidentTimeline as DomainTimeline
import qualified Environment
import EulerHS.Prelude hiding (forM_, id, length)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.AlertIncident as QAlertIncident
import qualified Storage.Queries.AlertIncidentTimeline as QAlertIncidentTimeline

-- | Process VictoriaMetrics vmalert webhook
postApiV1AlertsUpdate ::
  Maybe Bool ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Types.VmAlertWebhookReq ->
  Environment.Flow APISuccess.APISuccess
postApiV1AlertsUpdate isManual mbIncidentId mbUpdateType mbUpdateValue req = do
  logInfo $
    "Received alert webhook: status=" <> req.status
      <> ", updateType="
      <> fromMaybe "N/A" mbUpdateType
      <> ", incidentId="
      <> fromMaybe "N/A" mbIncidentId

  -- Validation: if isManual is True, incidentId must be provided
  let isManual' = fromMaybe False isManual
  when (isManual' && isNothing mbIncidentId) $ do
    logError "Manual update requested but no incidentId provided"
    throwError $ InternalError "Manual update requested but no incidentId provided"

  -- Serialize the entire request to JSON for rawPayload (used when creating new incident)
  let rawPayload = decodeUtf8 $ A.encode req

  forM_ req.alerts $ \alert -> do
    -- 1. Find or Create Incident
    mbIncident <- getIncident isManual' mbIncidentId alert

    case mbIncident of
      Nothing -> do
        -- If no incident found and it's firing, create one
        let statusStr = fromMaybe req.status mbUpdateType
        if T.toLower statusStr == "firing"
          then do
            incidentId <- handleFiringAlert isManual req alert rawPayload
            -- 2. Add to Timeline
            addTimelineEntry incidentId (fromMaybe "firing" mbUpdateType) mbUpdateValue alert
          else logWarning $ "No incident found to update for alert: " <> alert.labels.alertname
      Just incident -> do
        -- 2. Update AlertIncident Table if status-related
        let statusStr = fromMaybe req.status mbUpdateType
        case T.toLower statusStr of
          "firing" -> do
            logInfo $ "Incident " <> show incident.id <> " is already firing or being updated to firing"
          "resolved" -> do
            resolvedTime <- getCurrentTime
            let actualResolvedTime = if mbUpdateType == Just "resolved" then resolvedTime else alert.endsAt
            handleResolvedStatus incident actualResolvedTime
          _ -> logInfo $ "Non-status update for incident " <> show incident.id

        -- 3. Always add to Timeline
        addTimelineEntry incident.id (fromMaybe req.status mbUpdateType) mbUpdateValue alert

  pure APISuccess.Success

-- | Helper to find incident based on isManual and incidentId or description
getIncident :: Bool -> Maybe Text -> Types.AlertDetail -> Environment.Flow (Maybe Domain.AlertIncident)
getIncident True (Just incidentIdText) _ = do
  let incidentId = Id incidentIdText
  QAlertIncident.findById incidentId
getIncident _ _ alert = do
  let description' = alert.annotations.description
  existingIncidents <-
    QAlertIncident.findFiringIncident
      (Just 1)
      Nothing
      description'
      Domain.FIRING
  pure $ listToMaybe existingIncidents

-- | Handle firing alert - create new incident and return its ID
handleFiringAlert :: Maybe Bool -> Types.VmAlertWebhookReq -> Types.AlertDetail -> Text -> Environment.Flow (Id Domain.AlertIncident)
handleFiringAlert isManual req alert rawPayload = do
  let alertName = alert.labels.alertname
      serviceName = getServiceName alert.labels
      alertGroup' = alert.labels.alertgroup
      description' = alert.annotations.description
      severity' = alert.labels.severity
      firingTime = alert.startsAt
      receiver' = req.receiver
      externalURL' = req.externalURL

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
            rca = Nothing,
            createdAt = now,
            updatedAt = now
          }

  QAlertIncident.create incident
  logInfo $ "Created incident with id: " <> show incidentId <> " for alert: " <> alertName
  pure incidentId

-- | Handle resolved status update in AlertIncident table
handleResolvedStatus :: Domain.AlertIncident -> UTCTime -> Environment.Flow ()
handleResolvedStatus incident resolvedTime = do
  let downtimeSeconds = floor $ diffUTCTime resolvedTime incident.firingTime
  now <- getCurrentTime
  QAlertIncident.updateToResolved
    Domain.RESOLVED
    (Just resolvedTime)
    (Just downtimeSeconds)
    now
    incident.id

-- | Add entry to AlertIncidentTimeline
addTimelineEntry :: Id Domain.AlertIncident -> Text -> Maybe Text -> Types.AlertDetail -> Environment.Flow ()
addTimelineEntry incidentId eventType mbContent alert = do
  timelineId <- generateGUID
  now <- getCurrentTime
  let content' = mbContent <|> alert.annotations.description

  let timelineEntry =
        DomainTimeline.AlertIncidentTimeline
          { id = timelineId,
            incidentId = incidentId,
            eventTime = now,
            eventType = eventType,
            content = content',
            attachments = Nothing,
            createdAt = now,
            updatedAt = now
          }

  QAlertIncidentTimeline.create timelineEntry
  logInfo $ "Added timeline entry for incident: " <> show incidentId <> ", type: " <> eventType

-- | Helper to get service name from labels
getServiceName :: Types.AlertLabels -> Text
getServiceName labels = fromMaybe "unknown" (labels.alert)
