module Domain.Action.Dashboard.FinancialAlert
  ( listAlertRules,
    createAlertRule,
    updateAlertRule,
    deleteAlertRule,
    listAlerts,
    acknowledgeAlert,
    resolveAlert,
    getAlertSummary,
    AlertRuleCreateReq (..),
    AlertRuleUpdateReq (..),
    AlertSummaryResp (..),
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.FinancialAlert as DFA
import qualified Domain.Types.FinancialAlertRule as DFAR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, logInfo)
import qualified Storage.Queries.FinancialAlert as QFA
import qualified Storage.Queries.FinancialAlertRule as QFAR

-- | Request type for creating an alert rule
data AlertRuleCreateReq = AlertRuleCreateReq
  { name :: Text,
    alertType :: DFAR.AlertType,
    category :: DFAR.AlertCategory,
    conditionJson :: Text,
    severity :: DFAR.AlertSeverity,
    channels :: Text,
    cooldownMinutes :: Int,
    escalationMinutes :: Maybe Int,
    isActive :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Request type for updating an alert rule
data AlertRuleUpdateReq = AlertRuleUpdateReq
  { name :: Maybe Text,
    alertType :: Maybe DFAR.AlertType,
    category :: Maybe DFAR.AlertCategory,
    conditionJson :: Maybe Text,
    severity :: Maybe DFAR.AlertSeverity,
    channels :: Maybe Text,
    cooldownMinutes :: Maybe Int,
    escalationMinutes :: Maybe (Maybe Int),
    isActive :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Alert summary response with counts by severity
data AlertSummaryResp = AlertSummaryResp
  { totalOpen :: Int,
    totalAcknowledged :: Int,
    totalResolved :: Int,
    totalEscalated :: Int,
    criticalCount :: Int,
    warningCount :: Int,
    infoCount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | List all alert rules for a merchant
listAlertRules ::
  ShortId DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Flow [DFAR.FinancialAlertRule]
listAlertRules _merchantShortId merchantOpCityId = do
  logInfo "Listing financial alert rules"
  QFAR.findAllByMerchantId (cast merchantOpCityId)

-- | Create a new alert rule
createAlertRule ::
  ShortId DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  AlertRuleCreateReq ->
  Flow DFAR.FinancialAlertRule
createAlertRule _merchantShortId merchantOpCityId merchantId req = do
  logInfo $ "Creating financial alert rule: " <> req.name
  now <- getCurrentTime
  newId <- generateGUID
  let rule =
        DFAR.FinancialAlertRule
          { id = newId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            name = req.name,
            alertType = req.alertType,
            category = req.category,
            conditionJson = req.conditionJson,
            severity = req.severity,
            channels = req.channels,
            cooldownMinutes = req.cooldownMinutes,
            escalationMinutes = req.escalationMinutes,
            isActive = req.isActive,
            createdAt = now,
            updatedAt = now
          }
  QFAR.create rule
  pure rule

-- | Update an existing alert rule
updateAlertRule ::
  ShortId DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DFAR.FinancialAlertRule ->
  AlertRuleUpdateReq ->
  Flow APISuccess
updateAlertRule _merchantShortId _merchantOpCityId ruleId req = do
  logInfo $ "Updating financial alert rule: " <> ruleId.getId
  existingRule <- QFAR.findById ruleId >>= fromMaybeM (InvalidRequest "Alert rule not found")
  now <- getCurrentTime
  let updatedRule =
        existingRule
          { DFAR.name = fromMaybe existingRule.name req.name,
            DFAR.alertType = fromMaybe existingRule.alertType req.alertType,
            DFAR.category = fromMaybe existingRule.category req.category,
            DFAR.conditionJson = fromMaybe existingRule.conditionJson req.conditionJson,
            DFAR.severity = fromMaybe existingRule.severity req.severity,
            DFAR.channels = fromMaybe existingRule.channels req.channels,
            DFAR.cooldownMinutes = fromMaybe existingRule.cooldownMinutes req.cooldownMinutes,
            DFAR.escalationMinutes = fromMaybe existingRule.escalationMinutes <$> req.escalationMinutes <|> Just existingRule.escalationMinutes,
            DFAR.isActive = fromMaybe existingRule.isActive req.isActive,
            DFAR.updatedAt = now
          }
  QFAR.updateIsActiveById updatedRule.isActive now ruleId
  pure Success

-- | Delete an alert rule
deleteAlertRule ::
  ShortId DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DFAR.FinancialAlertRule ->
  Flow APISuccess
deleteAlertRule _merchantShortId _merchantOpCityId ruleId = do
  logInfo $ "Deleting financial alert rule: " <> ruleId.getId
  _ <- QFAR.findById ruleId >>= fromMaybeM (InvalidRequest "Alert rule not found")
  QFAR.deleteById ruleId
  pure Success

-- | List alerts for a merchant, optionally filtered by status
listAlerts ::
  ShortId DM.Merchant ->
  Id DM.Merchant ->
  Maybe DFA.AlertStatus ->
  Flow [DFA.FinancialAlert]
listAlerts _merchantShortId merchantId mbStatus = do
  logInfo "Listing financial alerts"
  case mbStatus of
    Just status -> QFA.findAllByMerchantIdAndStatus merchantId status
    Nothing -> QFA.findAllByMerchantId merchantId

-- | Acknowledge an alert
acknowledgeAlert ::
  ShortId DM.Merchant ->
  Id DFA.FinancialAlert ->
  Text ->
  Flow APISuccess
acknowledgeAlert _merchantShortId alertId acknowledger = do
  logInfo $ "Acknowledging financial alert: " <> alertId.getId
  _ <- QFA.findById alertId >>= fromMaybeM (InvalidRequest "Alert not found")
  now <- getCurrentTime
  QFA.updateAcknowledgeById DFA.ACKNOWLEDGED (Just acknowledger) (Just now) alertId
  pure Success

-- | Resolve an alert
resolveAlert ::
  ShortId DM.Merchant ->
  Id DFA.FinancialAlert ->
  Text ->
  Flow APISuccess
resolveAlert _merchantShortId alertId resolver = do
  logInfo $ "Resolving financial alert: " <> alertId.getId
  _ <- QFA.findById alertId >>= fromMaybeM (InvalidRequest "Alert not found")
  now <- getCurrentTime
  QFA.updateResolveById DFA.RESOLVED (Just resolver) (Just now) alertId
  pure Success

-- | Get alert summary counts by severity and status
getAlertSummary ::
  ShortId DM.Merchant ->
  Id DM.Merchant ->
  Flow AlertSummaryResp
getAlertSummary _merchantShortId merchantId = do
  logInfo "Getting financial alert summary"
  allAlerts <- QFA.findAllByMerchantId merchantId
  let openAlerts = filter (\a -> a.status == DFA.OPEN) allAlerts
      acknowledgedAlerts = filter (\a -> a.status == DFA.ACKNOWLEDGED) allAlerts
      resolvedAlerts = filter (\a -> a.status == DFA.RESOLVED) allAlerts
      escalatedAlerts = filter (\a -> a.status == DFA.ESCALATED) allAlerts
      criticalAlerts = filter (\a -> a.severity == DFA.CRITICAL && a.status == DFA.OPEN) allAlerts
      warningAlerts = filter (\a -> a.severity == DFA.WARNING && a.status == DFA.OPEN) allAlerts
      infoAlerts = filter (\a -> a.severity == DFA.INFO && a.status == DFA.OPEN) allAlerts
  pure $
    AlertSummaryResp
      { totalOpen = length openAlerts,
        totalAcknowledged = length acknowledgedAlerts,
        totalResolved = length resolvedAlerts,
        totalEscalated = length escalatedAlerts,
        criticalCount = length criticalAlerts,
        warningCount = length warningAlerts,
        infoCount = length infoAlerts
      }
