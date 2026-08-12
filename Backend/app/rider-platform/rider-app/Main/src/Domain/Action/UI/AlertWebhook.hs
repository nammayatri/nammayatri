{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.AlertWebhook (postApiV1AlertsUpdate) where

import qualified API.Types.UI.AlertWebhook as Types
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (diffUTCTime)
import qualified Domain.Types.AlertIncident as Domain
import qualified Environment
import EulerHS.Prelude hiding (forM_, id)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.AlertIncident as QAlertIncident

-- | Process VictoriaMetrics vmalert webhook
-- Handles both 'firing' and 'resolved' alert statuses
postApiV1AlertsUpdate :: Maybe (Id Domain.AlertIncident) -> Maybe Bool -> Maybe Text -> Types.VmAlertWebhookReq -> Environment.Flow APISuccess.APISuccess
postApiV1AlertsUpdate mbIncidentId isManual mbRca req = do
  let rawPayload = decodeUtf8 $ A.encode req
  forM_ req.alerts $ \alert ->
    case T.toLower req.status of
      "firing" -> handleFiringAlert isManual req alert rawPayload
      "resolved" -> handleResolvedAlert mbIncidentId isManual mbRca alert
      _ -> pure ()
  pure APISuccess.Success

handleFiringAlert :: Maybe Bool -> Types.VmAlertWebhookReq -> Types.AlertDetail -> Text -> Environment.Flow ()
handleFiringAlert isManual req alert rawPayload = do
  existingIncident <- findFiringIncident isManual alert
  whenNothing_ existingIncident $ do
    incidentId <- generateGUID
    now <- getCurrentTime
    QAlertIncident.create $
      Domain.AlertIncident
        { id = incidentId,
          alertName = alert.labels.alertname,
          serviceName = getServiceName alert.labels,
          alertGroup = alert.labels.alertgroup,
          description = alert.annotations.description,
          severity = alert.labels.severity,
          firingTime = alert.startsAt,
          resolvedTime = Nothing,
          downtimeSeconds = Nothing,
          status = Domain.FIRING,
          receiver = req.receiver,
          externalURL = req.externalURL,
          rawPayload = Just rawPayload,
          isManuallyEntered = isManual,
          rca = Nothing,
          createdAt = now,
          updatedAt = now
        }

handleResolvedAlert :: Maybe (Id Domain.AlertIncident) -> Maybe Bool -> Maybe Text -> Types.AlertDetail -> Environment.Flow ()
handleResolvedAlert mbIncidentId isManual mbRca alert = do
  incident <-
    findIncidentToResolve mbIncidentId isManual alert
      >>= fromMaybeM (InvalidRequest $ "No unresolved incident found for alert: " <> alert.labels.alertname)
  let resolvedTime = alert.endsAt
      downtimeSeconds = floor $ diffUTCTime resolvedTime incident.firingTime
  QAlertIncident.updateToResolved Domain.RESOLVED (Just resolvedTime) (Just downtimeSeconds) mbRca incident.id

findFiringIncident :: Maybe Bool -> Types.AlertDetail -> Environment.Flow (Maybe Domain.AlertIncident)
findFiringIncident isManual alert =
  listToMaybe
    <$> if fromMaybe False isManual
      then QAlertIncident.findFiringIncidentByAlertName (Just 1) Nothing alert.labels.alertname Domain.FIRING
      else QAlertIncident.findFiringIncident (Just 1) Nothing alert.annotations.description Domain.FIRING

findIncidentToResolve :: Maybe (Id Domain.AlertIncident) -> Maybe Bool -> Types.AlertDetail -> Environment.Flow (Maybe Domain.AlertIncident)
findIncidentToResolve mbIncidentId isManual alert =
  case mbIncidentId of
    Just incidentId -> do
      mbIncident <- QAlertIncident.findById incidentId
      pure $ mbIncident >>= \incident -> guard (isNothing incident.resolvedTime) $> incident
    Nothing ->
      listToMaybe
        <$> if fromMaybe False isManual
          then QAlertIncident.findIncidentToResolveByAlertName (Just 1) Nothing alert.labels.alertname Nothing
          else QAlertIncident.findIncidentToResolve (Just 1) Nothing alert.annotations.description Nothing

getServiceName :: Types.AlertLabels -> Text
getServiceName labels = fromMaybe "unknown" labels.alert
