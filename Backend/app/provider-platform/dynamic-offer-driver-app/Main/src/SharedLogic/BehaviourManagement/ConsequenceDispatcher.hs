{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BehaviourManagement.ConsequenceDispatcher
  ( DispatchContext (..),
    handleConsequences,
    handleCommunications,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.BlockTracker as BT
import qualified Lib.BehaviorTracker.Recorder as BTRecorder
import qualified Lib.BehaviorTracker.Types as BTT
import qualified Lib.CommunicationEngine.Parser as CMParser
import qualified Lib.CommunicationEngine.Types as CMT
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.ConsequenceEngine.Parser as CEParser
import qualified Lib.ConsequenceEngine.Types as CET
import Lib.Scheduler.Environment
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import qualified Lib.Yudhishthira.Flow.Dashboard as YudhishthiraFlow
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Allocator
import qualified SharedLogic.DriverCancellationPenalty as DCP
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Flow as SFlags
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.External.LocationTrackingService.Types
import SharedLogic.VehicleServiceTier (ServiceTierFilterMode (..), fetchVehicleTierForDriverWithUsageRestriction)
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify

-- | App-level context needed by consequence handlers.
data DispatchContext = DispatchContext
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    counterConfig :: Maybe BTT.CounterConfig,
    actionEvent :: Maybe BTT.ActionEvent
  }

-- | Dispatch all consequence directives for a driver.
handleConsequences ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisLTSFlowEnv r,
    CoreMetrics m,
    HasLocationService m r,
    JobCreator r m,
    HasShortDurationRetryCfg r c
  ) =>
  DispatchContext ->
  Id DP.Person ->
  [CET.ConsequenceDirective] ->
  m ()
handleConsequences ctx driverId directives = do
  let (actions, errors) = CEParser.parseDirectives directives
  unless (null errors) $
    logError $ "Consequence parse errors for driver " <> driverId.getId <> ": " <> show errors
  forM_ actions $ \action -> do
    result <- try @_ @SomeException $ dispatchConsequence ctx driverId action
    case result of
      Right () -> logDebug $ "Consequence executed for driver " <> driverId.getId <> ": " <> show action
      Left err -> logError $ "Consequence failed for driver " <> driverId.getId <> ": " <> show err

-- | Dispatch a single parsed consequence action.
dispatchConsequence ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    HasLocationService m r,
    JobCreator r m,
    HasShortDurationRetryCfg r c,
    Redis.HedisLTSFlowEnv r
  ) =>
  DispatchContext ->
  Id DP.Person ->
  CET.ConsequenceAction ->
  m ()
dispatchConsequence ctx driverId = \case
  CET.NoAction -> pure ()
  CET.FeatureBlock params -> do
    logWarning $ "Feature blocking driver " <> driverId.getId <> " for " <> params.featureName <> ", duration: " <> show params.blockDurationHours <> "h"
    now <- getCurrentTime
    let blockUntil = addUTCTime (fromIntegral params.blockDurationHours * 3600) now
    case params.featureName of
      "TOLL_ROUTES" -> QDriverInformation.updateTollRouteBlockedTill (Just blockUntil) (cast driverId)
      "AC_USAGE" -> do
        QDriverInformation.updateAcUsageRestrictionAndScore DI.ToggleNotAllowed (Just 0.0) (cast driverId)
        logInfo $ "AC usage restricted for driver " <> driverId.getId
        serviceTiers <- fetchVehicleTierForDriverWithUsageRestriction SelectedServiceTiers Nothing Nothing Nothing Nothing (cast driverId) ctx.merchantOperatingCityId
        let newTiers = (.serviceTierType) . fst <$> filter (not . snd) serviceTiers
        QVehicle.updateSelectedServiceTiers newTiers (cast driverId)
      other -> logWarning $ "Unknown feature for FeatureBlock: " <> other
    let tag = fromMaybe params.featureName params.blockReasonTag
    BT.writeBlockAndCooldownKeys BTT.DRIVER driverId.getId BTT.FEATURE_BLOCK tag params.blockDurationHours params.blockReason (A.toJSON params.featureName) params.cooldownHours
  CET.SoftBlock params -> do
    logWarning $ "Soft blocking driver " <> driverId.getId <> " from tiers: " <> show params.blockedFeatures <> ", duration: " <> show params.blockDurationHours <> "h"
    now <- getCurrentTime
    let blockUntil = addUTCTime (fromIntegral params.blockDurationHours * 3600) now
    let blockedTiers = case params.blockedServiceTiers of
          Just tiers | not (null tiers) -> Just (mapMaybe (readMaybe . toString) tiers)
          _ -> Nothing
    QDriverInformation.updateSoftBlock blockedTiers (Just blockUntil) (Just params.blockReason) (cast driverId)
    -- Schedule auto-unblock
    let unblockJobTs = secondsToNominalDiffTime (fromIntegral params.blockDurationHours) * 60 * 60
    JC.createJobIn @_ @'UnblockSoftBlockedDriver (Just ctx.merchantId) (Just ctx.merchantOperatingCityId) unblockJobTs $
      UnblockSoftBlockedDriverRequestJobData {driverId = cast driverId}
    let tag = fromMaybe "SOFT_BLOCK" params.blockReasonTag
    BT.writeBlockAndCooldownKeys BTT.DRIVER driverId.getId BTT.SOFT_BLOCK tag params.blockDurationHours params.blockReason (A.toJSON params.blockedFeatures) params.cooldownHours
  CET.HardBlock params -> do
    logWarning $ "Hard blocking driver " <> driverId.getId <> ", duration: " <> show params.blockDurationHours <> "h"
    let reasonFlag = parseBlockReasonFlag params.blockReasonTag
    SFlags.markBlockFlags (cast driverId) $
      SFlags.Block
        SFlags.BlockPayload
          { SFlags.bpReason = Just params.blockReason,
            SFlags.bpExpiryHours = Just params.blockDurationHours,
            SFlags.bpDashboardUserName = "BehaviorManagementFramework",
            SFlags.bpMerchantId = ctx.merchantId,
            SFlags.bpReasonCode = params.blockReason,
            SFlags.bpMerchantOperatingCityId = ctx.merchantOperatingCityId,
            SFlags.bpBlockedBy = DTDBT.Application,
            SFlags.bpActive = Just False,
            SFlags.bpMode = Just DriverInfo.OFFLINE,
            SFlags.bpFlag = reasonFlag
          }
    -- Block location tracking + schedule auto-unblock
    now <- getCurrentTime
    let expiryTime = addUTCTime (fromIntegral params.blockDurationHours * 60 * 60) now
    void $ LTS.blockDriverLocationsTill ctx.merchantId (cast driverId) expiryTime
    when (params.blockDurationHours > 0) $ do
      let unblockJobTs = secondsToNominalDiffTime (fromIntegral params.blockDurationHours) * 60 * 60
      JC.createJobIn @_ @'UnblockDriver (Just ctx.merchantId) (Just ctx.merchantOperatingCityId) unblockJobTs $
        UnblockDriverRequestJobData {driverId = cast driverId}
    let tag = fromMaybe "HARD_BLOCK" params.blockReasonTag
    BT.writeBlockAndCooldownKeys BTT.DRIVER driverId.getId BTT.HARD_BLOCK tag params.blockDurationHours params.blockReason (A.Object mempty) params.cooldownHours
  CET.PermanentBlock params -> do
    logWarning $ "Permanently blocking driver " <> driverId.getId <> ", reason: " <> params.blockReason
    SFlags.markBlockFlags (cast driverId) $
      SFlags.SimpleBlock
        SFlags.SimplePayload
          { SFlags.spModifier = Just "BehaviorManagementFramework",
            SFlags.spMerchantId = ctx.merchantId,
            SFlags.spMerchantOperatingCityId = ctx.merchantOperatingCityId,
            SFlags.spBlockedBy = DTDBT.Application
          }
    let tag = fromMaybe "PERMANENT_BLOCK" params.blockReasonTag
    BT.writeBlockKey BTT.DRIVER driverId.getId BTT.PERMANENT_BLOCK tag 0 params.blockReason (A.Object mempty)
  CET.Nudge params -> sendOverlayByKey ctx driverId params.nudgeKey
  CET.Warn params -> sendOverlayByKey ctx driverId params.warnKey
  CET.ChargeFee params -> do
    logInfo $ "Charge fee requested for driver " <> driverId.getId <> ": " <> show params.penaltyAmount <> " " <> params.currency <> " (" <> params.chargeReason <> ")"
    let penaltyAmount :: HighPrecMoney = realToFrac params.penaltyAmount
        currency = fromMaybe INR (readMaybe $ toString params.currency)
    if penaltyAmount <= 0
      then logWarning $ "Ignoring non-positive ChargeFee amount for driver " <> driverId.getId
      else do
        mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ctx.merchantOperatingCityId.getId}) Nothing
        case mbTransporterConfig of
          Nothing -> logError $ "ChargeFee skipped: TransporterConfig not found for city " <> ctx.merchantOperatingCityId.getId
          Just transporterConfig -> do
            feeId <- DCP.chargeDriverPenaltyFee ctx.merchantId ctx.merchantOperatingCityId (cast driverId) penaltyAmount currency transporterConfig
            logInfo $ "Charged behavior penalty fee " <> feeId.getId <> " (" <> params.chargeReason <> ") of " <> show penaltyAmount <> " to driver " <> driverId.getId
            -- Belt against rule-authoring mistakes: rules must also guard on
            -- {"var": "cooldowns.<feeCooldownTag>"} to avoid re-emitting CHARGE_FEE.
            whenJust params.feeCooldownTag $ \cooldownTag ->
              BT.writeCooldownKey BTT.DRIVER driverId.getId cooldownTag (fromMaybe 24 params.feeCooldownHours)
  CET.IncrementCounter params -> do
    case (ctx.counterConfig, ctx.actionEvent) of
      (Just config, Just event) -> do
        let mbCounterType = case params.counterType of
              "ACTION_COUNT" -> Just BTT.ACTION_COUNT
              "ELIGIBLE_COUNT" -> Just BTT.ELIGIBLE_COUNT
              _ -> Nothing
        case mbCounterType of
          Just counterType -> do
            logInfo $ "Incrementing counter " <> params.counterType <> " for driver " <> driverId.getId
            BTRecorder.incrementCounterOnly config event.entityType event.entityId event.actionType counterType
          Nothing -> logWarning $ "Unknown counterType '" <> params.counterType <> "' for driver " <> driverId.getId
      _ -> logWarning $ "INCREMENT_COUNTER consequence for driver " <> driverId.getId <> " but no counterConfig/actionEvent in DispatchContext"
  CET.AssignTag params -> do
    logInfo $ "Assigning tag " <> params.tagName <> " with value " <> params.tagValue <> " for driver " <> driverId.getId
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    now <- getCurrentTime
    let tag = Yudhishthira.mkTagNameValue (LYT.TagName params.tagName) (LYT.TextValue params.tagValue)
    mbRegisteredTag <- catch (YudhishthiraFlow.verifyTag (cast ctx.merchantOperatingCityId) tag) $ \(err :: SomeException) -> do
      logWarning $ "AssignTag: tag verification failed for '" <> params.tagName <> "': " <> show err
      pure Nothing
    let expiryHours = maybe (mbRegisteredTag >>= (.validity)) (Just . Hours) params.validityHours
        tagWithExpiry = Yudhishthira.addTagExpiry tag expiryHours now
        updatedTags = Yudhishthira.replaceTagNameValue driver.driverTag tagWithExpiry
    QPerson.updateDriverTag (Just updatedTags) driverId

-- | Map blockReasonTag text to BlockReasonFlag enum
parseBlockReasonFlag :: Maybe Text -> BlockReasonFlag
parseBlockReasonFlag = \case
  Just "CancellationRateDaily" -> CancellationRateDaily
  Just "CancellationRateWeekly" -> CancellationRateWeekly
  Just "CancellationRate" -> CancellationRate
  Just "ExtraFareDaily" -> ExtraFareDaily
  Just "ExtraFareWeekly" -> ExtraFareWeekly
  Just "DrunkAndDriveViolation" -> DrunkAndDriveViolation
  Just "DocumentExpiry" -> DocumentExpiry
  Just "PickupStall" -> PickupStall
  Just "ByDashboard" -> ByDashboard
  Just other -> fromMaybe ByDashboard (readMaybe $ toString other)
  Nothing -> ByDashboard

-- | Send an overlay notification to a driver using a PNKey
sendOverlayByKey ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DispatchContext ->
  Id DP.Person ->
  Text -> -- overlayPNKey
  m ()
sendOverlayByKey ctx driverId overlayKey = do
  logInfo $ "Sending overlay for driver " <> driverId.getId <> ": " <> overlayKey
  mbDriver <- QPerson.findById driverId
  whenJust mbDriver $ \driver -> do
    mbOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory ctx.merchantOperatingCityId overlayKey (fromMaybe ENGLISH driver.language) Nothing Nothing Nothing
    whenJust mbOverlay $ \overlay -> do
      let fcmOverlayReq = Notify.mkOverlayReq overlay
      Notify.sendOverlay ctx.merchantOperatingCityId driver fcmOverlayReq

-- | Dispatch all communication directives for a driver.
handleCommunications ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DispatchContext ->
  Id DP.Person ->
  [CMT.CommunicationDirective] ->
  m ()
handleCommunications ctx driverId directives = do
  let (actions, errors) = CMParser.parseDirectives directives
  unless (null errors) $
    logError $ "Communication parse errors for driver " <> driverId.getId <> ": " <> show errors
  forM_ actions $ \action -> do
    result <- try @_ @SomeException $ dispatchCommunicationAction ctx driverId action
    case result of
      Right () -> logDebug $ "Communication dispatched for driver " <> driverId.getId <> ": " <> show action
      Left err -> logError $ "Communication failed for driver " <> driverId.getId <> ": " <> show err

-- | Dispatch a single parsed communication action.
--
-- Title/body text comes from the RULE's templateParams (dynamic logic), not from
-- DB templates: templateParams: {"title": "...", "body": "...", "okButtonText": "..."}
dispatchCommunicationAction ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DispatchContext ->
  Id DP.Person ->
  CMT.CommunicationAction ->
  m ()
dispatchCommunicationAction ctx driverId = \case
  CMT.NoCommunication -> pure ()
  CMT.FcmNotification params -> do
    let title = fromMaybe params.templateKey (textFromParams "title" params.templateParams)
        body = fromMaybe "" (textFromParams "body" params.templateParams)
    withDriver $ \driver -> do
      logInfo $ "FCM notification for driver " <> driverId.getId <> ": " <> params.templateKey
      Notify.notifyDriver ctx.merchantOperatingCityId FCM.DRIVER_NOTIFY title body driver driver.deviceToken
  CMT.InAppOverlay params ->
    withDriver $ \driver -> do
      logInfo $ "In-app overlay for driver " <> driverId.getId <> ": " <> params.overlayKey
      Notify.sendOverlay ctx.merchantOperatingCityId driver (mkRuleOverlayReq params)
  CMT.InAppMessage params -> do
    -- Interim: delivered as an FCM push. Persistent message-center delivery
    -- (message table + translations) is a follow-up.
    let title = fromMaybe params.messageKey (textFromParams "title" params.templateParams)
        body = fromMaybe "" (textFromParams "body" params.templateParams)
    withDriver $ \driver -> do
      logInfo $ "In-app message (as FCM) for driver " <> driverId.getId <> ": " <> params.messageKey
      Notify.notifyDriver ctx.merchantOperatingCityId FCM.DRIVER_NOTIFY title body driver driver.deviceToken
  CMT.SmsCommunication params ->
    logInfo $ "SMS (not yet handled) for driver " <> driverId.getId <> ": " <> params.templateKey
  CMT.BadgeCommunication params ->
    logInfo $ "Badge (not yet handled) for driver " <> driverId.getId <> ": " <> params.badgeKey
  where
    withDriver actionFn = do
      mbDriver <- QPerson.findById driverId
      case mbDriver of
        Just driver -> actionFn driver
        Nothing -> logWarning $ "Communication skipped, driver not found: " <> driverId.getId

-- | Extract a text field from the rule-provided templateParams object.
textFromParams :: Text -> A.Value -> Maybe Text
textFromParams key (A.Object o) = case AKM.lookup (AK.fromText key) o of
  Just (A.String t) -> Just t
  _ -> Nothing
textFromParams _ _ = Nothing

-- | Overlay built entirely from the rule's params (no DB overlay template).
mkRuleOverlayReq :: CMT.InAppOverlayParams -> FCM.FCMOverlayReq
mkRuleOverlayReq params =
  FCM.FCMOverlayReq
    { title = textFromParams "title" params.templateParams,
      description = textFromParams "body" params.templateParams,
      imageUrl = Nothing,
      okButtonText = textFromParams "okButtonText" params.templateParams,
      cancelButtonText = if params.showCloseButton then Just "Close" else Nothing,
      actions = [],
      actions2 = [],
      secondaryActions2 = Nothing,
      link = Nothing,
      endPoint = Nothing,
      method = Nothing,
      reqBody = A.Null,
      delay = Nothing,
      contactSupportNumber = Nothing,
      toastMessage = Nothing,
      secondaryActions = Nothing,
      socialMediaLinks = Nothing,
      showPushNotification = Nothing
    }
