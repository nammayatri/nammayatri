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
    DispatchOutcome (..),
    behaviourEventCity,
    handleConsequences,
    handleCommunications,
    sendOverlayByKey,
  )
where

import qualified Dashboard.Common as DC
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BSL
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorEngine.EventLog as EventLog
import qualified Lib.BehaviorTracker.BlockTracker as BT
import qualified Lib.BehaviorTracker.EventsWindow as BTEW
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
import qualified Lib.Yudhishthira.Types.NammaTagV2 as Fields
import SharedLogic.Allocator
import qualified SharedLogic.DriverCancellationPenalty as DCP
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Flow as SFlags
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.External.LocationTrackingService.Types
import SharedLogic.Finance.WalletAccount (isWalletSubscriptionCity)
import SharedLogic.VehicleServiceTier (recomputeSelectedServiceTiers, selectedServiceTiersLockKey)
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
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

-- | Merchant short id and city name for behaviour metrics events (cached read).
-- Passed to the orchestrator, which runs it in the background with the event.
behaviourEventCity :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m (Maybe EventLog.BehaviourEventCity)
behaviourEventCity mocId = fmap toEventCity <$> CQMOC.findById mocId
  where
    toEventCity moc =
      EventLog.BehaviourEventCity
        { EventLog.merchantShortId = moc.merchantShortId.getShortId,
          EventLog.city = show moc.city
        }

-- | What a consequence run means for the rest of the run.
-- 'blockWon' is False when a block or restriction in the run was dropped (the driver is
-- already blocked at least as long) or failed; the run's Warn/Nudge and communications are
-- then skipped, so the driver isn't told about a block that didn't happen. True when the
-- run has no block or restriction.
newtype DispatchOutcome = DispatchOutcome {blockWon :: Bool}

-- | Result of one consequence action. 'BlockDropped': a block or restriction that lost under
-- "longest block wins". 'BlockNotApplied': a block or restriction that restricts nothing (unknown
-- feature name, no valid service tier), so nothing was written. Either way the rung's Warn/Nudge
-- and the run's communications are skipped.
data ActionOutcome = ActionDone | BlockDropped | BlockNotApplied
  deriving (Eq, Show)

-- | Dispatch all consequence directives for a driver.
-- Blocks and restrictions run first (a run fires one rung, so these all belong to it);
-- every other action keeps its order and runs after them.
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
  m DispatchOutcome
handleConsequences ctx driverId directives = do
  let (actions, errors) = CEParser.parseDirectives directives
  unless (null errors) $
    logError $ "Consequence parse errors for driver " <> driverId.getId <> ": " <> show errors
  let (blockActions, otherActions) = splitBlockActions actions
  blockOutcomes <- mapM runAction blockActions
  let blockWon = all (== Just ActionDone) blockOutcomes
  forM_ otherActions $ \action ->
    if not (runsAfterBlocks blockWon action)
      then logInfo $ "Consequence skipped for driver " <> driverId.getId <> " because this run's block or restriction was dropped or failed: " <> show action
      else void $ runAction action
  pure $ DispatchOutcome blockWon
  where
    runAction action = do
      result <- try @_ @SomeException $ dispatchConsequence ctx driverId action
      case result of
        Right outcome -> do
          logDebug $ "Consequence executed for driver " <> driverId.getId <> " (" <> show outcome <> "): " <> show action
          pure (Just outcome)
        Left err -> do
          logError $ "Consequence failed for driver " <> driverId.getId <> ": " <> show err
          pure Nothing

-- | Blocks and restrictions first; every other action after them, in its original order.
splitBlockActions :: [CET.ConsequenceAction] -> ([CET.ConsequenceAction], [CET.ConsequenceAction])
splitBlockActions = partition isBlockAction

-- | Does a non-block action still run after the run's blocks? Warn/Nudge only if every block
-- and restriction won (or there were none); other actions always.
runsAfterBlocks :: Bool -> CET.ConsequenceAction -> Bool
runsAfterBlocks blockWon action = blockWon || not (isMessageAction action)

isBlockAction :: CET.ConsequenceAction -> Bool
isBlockAction = \case
  CET.HardBlock _ -> True
  CET.PermanentBlock _ -> True
  CET.SoftBlock _ -> True
  CET.FeatureBlock _ -> True
  _ -> False

isMessageAction :: CET.ConsequenceAction -> Bool
isMessageAction = \case
  CET.Warn _ -> True
  CET.Nudge _ -> True
  _ -> False

toActionOutcome :: QDriverInformation.BlockWriteResult -> ActionOutcome
toActionOutcome = \case
  QDriverInformation.Applied _ -> ActionDone
  QDriverInformation.Dropped -> BlockDropped

-- | The rung's cooldown is written whether its block won or was dropped: the rung fired.
writeRungCooldown :: (MonadFlow m, Redis.HedisFlow m r) => Id DP.Person -> Text -> Maybe Int -> m ()
writeRungCooldown driverId tag mbCooldownHours =
  whenJust mbCooldownHours $ BT.writeCooldownKey BTT.DRIVER driverId.getId tag

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
  m ActionOutcome
dispatchConsequence ctx driverId = \case
  CET.NoAction -> pure ActionDone
  CET.FeatureBlock params -> do
    logWarning $ "Feature blocking driver " <> driverId.getId <> " for " <> params.featureName <> ", duration: " <> show params.blockDurationHours <> "h"
    now <- getCurrentTime
    let blockUntil = addUTCTime (fromIntegral params.blockDurationHours * 3600) now
    outcome <- case params.featureName of
      "TOLL_ROUTES" -> toActionOutcome <$> QDriverInformation.updateTollRouteBlockedTill blockUntil (cast driverId)
      "AC_USAGE" -> do
        QDriverInformation.updateAcUsageRestrictionAndScore DI.ToggleNotAllowed (Just 0.0) (cast driverId)
        logInfo $ "AC usage restricted for driver " <> driverId.getId
        recomputeSelectedServiceTiers Nothing Nothing Nothing Nothing (cast driverId) ctx.merchantOperatingCityId
        pure ActionDone
      other -> do
        -- Restricts nothing, so it must not look applied: no block key, no messages.
        logWarning $ "FeatureBlock not applied for driver " <> driverId.getId <> ": unknown feature name " <> other
        pure BlockNotApplied
    let tag = fromMaybe params.featureName params.blockReasonTag
    when (outcome == ActionDone) $
      BT.writeBlockKey BTT.DRIVER driverId.getId BTT.FEATURE_BLOCK tag params.blockDurationHours params.blockReason (A.toJSON params.featureName)
    writeRungCooldown driverId tag params.cooldownHours
    pure outcome
  CET.SoftBlock params -> do
    logWarning $ "Soft blocking driver " <> driverId.getId <> " from tiers: " <> show params.blockedFeatures <> ", duration: " <> show params.blockDurationHours <> "h"
    now <- getCurrentTime
    let blockUntil = addUTCTime (fromIntegral params.blockDurationHours * 3600) now
    let requestedTiers = fromMaybe [] params.blockedServiceTiers
        parsedTiers = map (\name -> (name, readMaybe (toString name))) requestedTiers
        validTiers = mapMaybe snd parsedTiers
        unknownTiers = [name | (name, Nothing) <- parsedTiers]
        tag = fromMaybe "SOFT_BLOCK" params.blockReasonTag
    unless (null unknownTiers) $
      logWarning $ "Soft block for driver " <> driverId.getId <> ": ignoring unknown service tiers " <> show unknownTiers
    if null validTiers
      then do
        -- No tier to restrict: write nothing (it would otherwise wipe an active soft block's tiers)
        -- and send nothing.
        logWarning $ "Soft block not applied for driver " <> driverId.getId <> ": no valid service tier in " <> show params.blockedServiceTiers
        writeRungCooldown driverId tag params.cooldownHours
        pure BlockNotApplied
      else do
        result <- QDriverInformation.setSoftBlock (Just validTiers) blockUntil (Just params.blockReason) (cast driverId)
        case result of
          QDriverInformation.Applied mbEndTime -> do
            -- Schedule auto-unblock at the soft block's end time
            let endTime = fromMaybe blockUntil mbEndTime
            JC.createJobIn @_ @'UnblockSoftBlockedDriver (Just ctx.merchantId) (Just ctx.merchantOperatingCityId) (diffUTCTime endTime now) $
              UnblockSoftBlockedDriverRequestJobData {driverId = cast driverId}
            BT.writeBlockKey BTT.DRIVER driverId.getId BTT.SOFT_BLOCK tag params.blockDurationHours params.blockReason (A.toJSON params.blockedFeatures)
          QDriverInformation.Dropped -> pure ()
        writeRungCooldown driverId tag params.cooldownHours
        pure $ toActionOutcome result
  CET.HardBlock params -> do
    logWarning $ "Hard blocking driver " <> driverId.getId <> ", duration: " <> show params.blockDurationHours <> "h"
    let reasonFlag = parseBlockReasonFlag params.blockReasonTag
    result <-
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
    -- The driver is blocked from here on (by this block, or by one that ends at least as
    -- late when this one is dropped), so their offences are served: reset the window now,
    -- before the follow-up steps below, so a failure in one of them can't leave the old
    -- offences to block the driver again after the unblock.
    resetEventsWindow ctx driverId
    let tag = fromMaybe "HARD_BLOCK" params.blockReasonTag
    case result of
      QDriverInformation.Applied mbEndTime -> do
        -- Block location tracking + schedule auto-unblock, both at the block's end time
        now <- getCurrentTime
        let expiryTime = fromMaybe (addUTCTime (fromIntegral params.blockDurationHours * 60 * 60) now) mbEndTime
        void $ LTS.blockDriverLocationsTill ctx.merchantId (cast driverId) expiryTime
        when (params.blockDurationHours > 0) $
          JC.createJobIn @_ @'UnblockDriver (Just ctx.merchantId) (Just ctx.merchantOperatingCityId) (diffUTCTime expiryTime now) $
            UnblockDriverRequestJobData {driverId = cast driverId}
        BT.writeBlockKey BTT.DRIVER driverId.getId BTT.HARD_BLOCK tag params.blockDurationHours params.blockReason (A.Object mempty)
      QDriverInformation.Dropped -> pure ()
    writeRungCooldown driverId tag params.cooldownHours
    pure $ toActionOutcome result
  CET.PermanentBlock params -> do
    logWarning $ "Permanently blocking driver " <> driverId.getId <> ", reason: " <> params.blockReason
    result <-
      SFlags.markBlockFlags (cast driverId) $
        SFlags.SimpleBlock
          SFlags.SimplePayload
            { SFlags.spModifier = Just "BehaviorManagementFramework",
              SFlags.spMerchantId = ctx.merchantId,
              SFlags.spMerchantOperatingCityId = ctx.merchantOperatingCityId,
              SFlags.spBlockedBy = DTDBT.Application
            }
    -- Blocked from here on (applied, or already blocked with no end time): reset the window
    -- before the follow-up step (see HardBlock).
    resetEventsWindow ctx driverId
    let tag = fromMaybe "PERMANENT_BLOCK" params.blockReasonTag
    when (result /= QDriverInformation.Dropped) $
      BT.writeBlockKey BTT.DRIVER driverId.getId BTT.PERMANENT_BLOCK tag 0 params.blockReason (A.Object mempty)
    pure $ toActionOutcome result
  CET.Nudge params -> ActionDone <$ sendOverlayByKey ctx driverId params.nudgeKey params.templateParams
  CET.Warn params -> do
    mbSentOverlay <- sendOverlayByKey ctx driverId params.warnKey params.templateParams
    when params.showOnProfile $ do
      now <- getCurrentTime
      let hours = fromMaybe 168 params.profileWarningHours
      BT.writeProfileWarning BTT.DRIVER driverId.getId $
        BTT.ProfileWarningInfo
          { reasonTag = params.warnKey,
            title = (.title) =<< mbSentOverlay,
            message = (.description) =<< mbSentOverlay,
            warnedAt = now,
            expiresAt = addUTCTime (fromIntegral hours * 3600) now
          }
    pure ActionDone
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
            walletSubscriptionCity <- isWalletSubscriptionCity transporterConfig
            if walletSubscriptionCity
              then do
                -- Wallet-subscription city: debit the driver's wallet right away (a retried job) instead of
                -- creating a fee that would wait for a daily collection.
                chargeId <- generateGUIDText
                JC.createJobIn @_ @'ChargeWalletPenalty (Just ctx.merchantId) (Just ctx.merchantOperatingCityId) 0 $
                  ChargeWalletPenaltyJobData
                    { merchantId = ctx.merchantId,
                      merchantOperatingCityId = ctx.merchantOperatingCityId,
                      driverId = driverId,
                      walletPenaltyAmount = penaltyAmount,
                      walletPenaltyChargeId = chargeId
                    }
                logInfo $ "Queued wallet debit " <> chargeId <> " (" <> params.chargeReason <> ") of " <> show penaltyAmount <> " for driver " <> driverId.getId
              else do
                feeId <- DCP.chargeDriverPenaltyFee ctx.merchantId ctx.merchantOperatingCityId (cast driverId) penaltyAmount currency transporterConfig
                logInfo $ "Charged behavior penalty fee " <> feeId.getId <> " (" <> params.chargeReason <> ") of " <> show penaltyAmount <> " to driver " <> driverId.getId
            -- Belt against rule-authoring mistakes: rules must also guard on
            -- {"var": "cooldowns.<feeCooldownTag>"} to avoid re-emitting CHARGE_FEE.
            whenJust params.feeCooldownTag $ \cooldownTag ->
              BT.writeCooldownKey BTT.DRIVER driverId.getId cooldownTag (fromMaybe 24 params.feeCooldownHours)
    pure ActionDone
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
    pure ActionDone
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
    pure ActionDone
  CET.OptOutAutoAssign params -> do
    logInfo $ "Opting driver " <> driverId.getId <> " out of auto-assign: " <> params.reason
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    let remainingTags = filter ((/= Just (LYT.TagName "AutoAssign")) . Yudhishthira.parseTagName) (fromMaybe [] driver.driverTag)
    QPerson.updateDriverTag (Just remainingTags) driverId
    Redis.withWaitAndLockRedisWithTimeout (selectedServiceTiersLockKey driverId) 5 10 50000 $ do
      mbVehicle <- QVehicle.findById driverId
      whenJust mbVehicle $ \(vehicle :: DVeh.Vehicle) -> do
        cityServiceTiers <- CQVST.findAllByMerchantOpCityId ctx.merchantOperatingCityId Nothing
        let remainingServiceTiers = filter (`notElem` autoAcceptOnlyTierTypes cityServiceTiers) vehicle.selectedServiceTiers
        when (not (null (fromMaybe [] vehicle.selectedAutoAcceptTiers)) || remainingServiceTiers /= vehicle.selectedServiceTiers) $
          QVehicle.updateSelectedServiceTiersAndAutoAcceptTiers remainingServiceTiers [] driverId
    pure ActionDone

-- | A hard or permanent block clears the events window of the behaviour that fired it, so
-- the driver starts fresh (Time and Events versions alike; windows are kept in every city).
-- Behaviours without a window have no key, so the delete does nothing. A failed reset is
-- logged and never undoes the block.
-- Note: it deletes by the event's action type, which is the window key for cancellation
-- (RIDE_CANCELLATION). Switch it to the pack's window key when a behaviour whose events use
-- other action types (quote response: QUOTE_RESPONSE_ACCEPT/_REJECT) gets events support.
resetEventsWindow :: (MonadFlow m, Redis.HedisFlow m r) => DispatchContext -> Id DP.Person -> m ()
resetEventsWindow ctx driverId =
  whenJust ctx.actionEvent $ \event -> do
    result <- withTryCatch "eventsWindow:resetAfterBlock" $ BTEW.resetWindow event.entityType event.actionType event.entityId
    case result of
      Right () -> logInfo $ "Events window reset after a block for driver " <> driverId.getId <> " (" <> event.actionType <> ")"
      Left err -> logError $ "Events window: reset after a block failed for driver " <> driverId.getId <> " (" <> event.actionType <> ") because the Redis delete failed: " <> show err

autoAcceptOnlyTierTypes :: [DVST.VehicleServiceTier] -> [DriverInfo.ServiceTierType]
autoAcceptOnlyTierTypes cityServiceTiers =
  [ tier.serviceTierType
    | tier <- cityServiceTiers,
      Just cfg <- [tier.autoAcceptanceConfig],
      cfg.enabled && cfg.mode == DC.AutoAcceptOnly
  ]

-- | Map blockReasonTag text to BlockReasonFlag enum
parseBlockReasonFlag :: Maybe Text -> BlockReasonFlag
parseBlockReasonFlag = \case
  Just "CancellationRateDaily" -> CancellationRateDaily
  Just "CancellationRateWeekly" -> CancellationRateWeekly
  Just "CancellationRate" -> CancellationRate
  -- a block by an Events ("last X events") cancellation rulebook
  Just "CancellationRateEvents" -> CancellationRateEvents
  Just "ExtraFareDaily" -> ExtraFareDaily
  Just "ExtraFareWeekly" -> ExtraFareWeekly
  Just "DrunkAndDriveViolation" -> DrunkAndDriveViolation
  Just "DocumentExpiry" -> DocumentExpiry
  Just "PickupStall" -> PickupStall
  Just "LOW_RATING_BLOCK" -> LowRating
  Just "ByDashboard" -> ByDashboard
  Just other -> fromMaybe ByDashboard (readMaybe $ toString other)
  Nothing -> ByDashboard

-- | Send an overlay notification to a driver using a PNKey. The rule's
-- templateParams fill {#key#} placeholders in the city's per-language template,
-- so copy stays localized and ops-editable while the numbers come from the rule.
-- Returns the substituted request it sent (Nothing = driver or template missing),
-- so callers can persist the exact text the driver saw.
sendOverlayByKey ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DispatchContext ->
  Id DP.Person ->
  Text -> -- overlayPNKey
  Maybe A.Value -> -- placeholder values from the rule
  m (Maybe FCM.FCMOverlayReq)
sendOverlayByKey ctx driverId overlayKey mbTemplateParams = do
  logInfo $ "Sending overlay for driver " <> driverId.getId <> ": " <> overlayKey
  mbDriver <- QPerson.findById driverId
  case mbDriver of
    Nothing -> pure Nothing
    Just driver -> do
      mbOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory ctx.merchantOperatingCityId overlayKey (fromMaybe ENGLISH driver.language) Nothing Nothing Nothing
      forM mbOverlay $ \overlay -> do
        let fcmOverlayReq = substituteOverlayParams mbTemplateParams (Notify.mkOverlayReq overlay)
        Notify.sendOverlay ctx.merchantOperatingCityId driver fcmOverlayReq
        pure fcmOverlayReq

-- | Fill {#key#} placeholders in the overlay's user-visible text fields from the
-- rule's templateParams object. Unknown placeholders are left as-is.
substituteOverlayParams :: Maybe A.Value -> FCM.FCMOverlayReq -> FCM.FCMOverlayReq
substituteOverlayParams (Just (A.Object o)) req =
  req{FCM.title = subst <$> req.title,
      FCM.description = subst <$> req.description,
      FCM.okButtonText = subst <$> req.okButtonText,
      FCM.toastMessage = subst <$> req.toastMessage
     }
  where
    subst txt = foldl' (\acc (k, v) -> T.replace ("{#" <> AK.toText k <> "#}") (renderParam v) acc) txt (AKM.toList o)
    renderParam = \case
      A.String s -> s
      v -> TE.decodeUtf8 (BSL.toStrict (A.encode v)) -- numbers render bare: 4, 12.5
substituteOverlayParams _ req = req

-- | Dispatch all communication directives for a driver. Skipped when the run's block or
-- restriction was dropped or failed ('handleConsequences' result).
handleCommunications ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DispatchContext ->
  Id DP.Person ->
  DispatchOutcome ->
  [CMT.CommunicationDirective] ->
  m ()
handleCommunications ctx driverId outcome directives
  | not outcome.blockWon =
    unless (null directives) $
      logInfo $ "Communications skipped for driver " <> driverId.getId <> " because this run's block or restriction was dropped or failed"
  | otherwise = do
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
        notifType =
          fromMaybe FCM.TRIGGER_FCM $
            textFromParams "notificationType" params.templateParams >>= readMaybe . T.unpack
    withDriver $ \driver -> do
      logInfo $ "FCM notification for driver " <> driverId.getId <> ": " <> params.templateKey <> " as " <> show notifType
      Notify.notifyDriver ctx.merchantOperatingCityId notifType (Notify.prebuiltPushContent title body) driver driver.deviceToken
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
      Notify.notifyDriver ctx.merchantOperatingCityId FCM.DRIVER_NOTIFY (Notify.prebuiltPushContent title body) driver driver.deviceToken
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
      imageUrl = Just $ fromMaybe "" (textFromParams "imageUrl" params.templateParams),
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
