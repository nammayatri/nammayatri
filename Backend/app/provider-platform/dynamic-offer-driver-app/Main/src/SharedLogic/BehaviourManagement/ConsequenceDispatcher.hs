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

import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.CommunicationEngine.Parser as CMParser
import qualified Lib.CommunicationEngine.Types as CMT
import qualified Lib.ConsequenceEngine.Parser as CEParser
import qualified Lib.ConsequenceEngine.Types as CET
import qualified Storage.Queries.DriverInformation as QDriverInformation
import Tools.Error (BlockReasonFlag (..))

-- | App-level context needed by some consequence handlers (e.g. HardBlock, PermanentBlock).
-- Callsites construct this from booking/ride context.
data DispatchContext = DispatchContext
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }

-- | Dispatch all consequence directives for a driver.
--
-- Consequence type → driver-app action:
--   FeatureBlock   → updateTollRouteBlockedTill (TOLL_ROUTES), extensible for other features
--   SoftBlock      → updateSoftBlock (blocks from specific service tiers)
--   HardBlock      → updateDynamicBlockedStateWithActivity (full block with expiry)
--   PermanentBlock → updateBlockedState (permanent, requires manual unblock)
--   Nudge/Warn     → no DB change; communication pipeline handles notification
--   ChargeFee      → payment system integration (TBD)
handleConsequences ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DispatchContext ->
  Id DP.Person ->
  [CET.ConsequenceDirective] ->
  m ()
handleConsequences ctx driverId directives = do
  let (actions, errors) = CEParser.parseDirectives directives
  unless (null errors) $
    logError $ "Consequence parse errors for driver " <> driverId.getId <> ": " <> show errors
  -- TODO: Integrate with a proper audit trail (e.g. DriverBlockTransactions or a dedicated
  -- consequence audit table) to persist dispatched consequences for compliance and debugging.
  forM_ actions $ \action -> do
    result <- try @_ @SomeException $ dispatchConsequence ctx driverId action
    case result of
      Right () -> do
        logDebug $ "Consequence executed for driver " <> driverId.getId <> ": " <> show action
        logInfo $ "[AUDIT] Consequence dispatched: type=" <> showConsequenceType action <> ", target=driver:" <> driverId.getId <> ", status=SUCCESS"
      Left err -> do
        logError $ "Consequence failed for driver " <> driverId.getId <> ": " <> show err
        logInfo $ "[AUDIT] Consequence dispatched: type=" <> showConsequenceType action <> ", target=driver:" <> driverId.getId <> ", status=FAILED, error=" <> show err

-- | Dispatch a single parsed consequence action.
dispatchConsequence ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
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
      other -> logWarning $ "Unknown feature for FeatureBlock: " <> other
  CET.SoftBlock params -> do
    logWarning $ "Soft blocking driver " <> driverId.getId <> " from tiers: " <> show params.blockedFeatures <> ", duration: " <> show params.blockDurationHours <> "h"
    now <- getCurrentTime
    let blockUntil = addUTCTime (fromIntegral params.blockDurationHours * 3600) now
    QDriverInformation.updateSoftBlock Nothing (Just blockUntil) (Just params.blockReason) (cast driverId)
  CET.HardBlock params -> do
    logWarning $ "Hard blocking driver " <> driverId.getId <> ", duration: " <> show params.blockDurationHours <> "h"
    QDriverInformation.updateDynamicBlockedStateWithActivity
      (cast driverId)
      (Just params.blockReason)
      (Just params.blockDurationHours)
      "BehaviorManagementFramework"
      ctx.merchantId
      params.blockReason
      ctx.merchantOperatingCityId
      DTDBT.Application
      True
      (Just False) -- set active = False
      Nothing -- no mode change
      CancellationRate -- default reason flag; callers can extend
  CET.PermanentBlock params -> do
    logWarning $ "Permanently blocking driver " <> driverId.getId <> ", reason: " <> params.blockReason
    QDriverInformation.updateBlockedState
      (cast driverId)
      True
      (Just "BehaviorManagementFramework")
      ctx.merchantId
      ctx.merchantOperatingCityId
      DTDBT.Application
  CET.Nudge _params -> pure ()
  CET.Warn _params -> pure ()
  CET.ChargeFee params -> do
    -- TODO: Integrate with the fee/penalty system (e.g. Payment.chargePenalty or driver dues framework)
    -- to actually deduct the fee from the driver's account. Currently this is a no-op.
    logWarning $ "ChargeFee not implemented — fee not applied for driver " <> driverId.getId <> ": " <> show params.penaltyAmount <> " " <> params.currency

-- | Helper to extract a human-readable consequence type for audit logging.
showConsequenceType :: CET.ConsequenceAction -> Text
showConsequenceType = \case
  CET.NoAction -> "NO_ACTION"
  CET.FeatureBlock _ -> "FEATURE_BLOCK"
  CET.SoftBlock _ -> "SOFT_BLOCK"
  CET.HardBlock _ -> "HARD_BLOCK"
  CET.PermanentBlock _ -> "PERMANENT_BLOCK"
  CET.Nudge _ -> "NUDGE"
  CET.Warn _ -> "WARN"
  CET.ChargeFee _ -> "CHARGE_FEE"

-- | Dispatch all communication directives for a driver.
handleCommunications ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  [CMT.CommunicationDirective] ->
  m ()
handleCommunications driverId directives = do
  forM_ directives $ \directive -> do
    case CMParser.parseDirective directive of
      Left err ->
        logError $ "Communication parse error for driver " <> driverId.getId <> ": " <> err
      Right action -> do
        let delaySec = directive.delaySeconds
        if delaySec > 0
          then do
            -- WARNING: This is fire-and-forget — success is assumed before actual dispatch.
            -- TODO: Replace fork + threadDelaySec with a durable job queue (e.g. Lib.Scheduler)
            -- to ensure delayed communications are reliably delivered.
            logWarning $ "Scheduling delayed communication (" <> show delaySec <> "s) for driver " <> driverId.getId <> " — fire-and-forget via fork"
            fork ("delayed-comm-" <> driverId.getId) $ do
              threadDelaySec (fromIntegral delaySec)
              void $ try @_ @SomeException $ dispatchCommunicationAction driverId action
          else do
            result <- try @_ @SomeException $ dispatchCommunicationAction driverId action
            case result of
              Right () -> logDebug $ "Communication dispatched for driver " <> driverId.getId <> ": " <> show action
              Left err -> logError $ "Communication failed for driver " <> driverId.getId <> ": " <> show err

-- | Dispatch a single parsed communication action.
dispatchCommunicationAction ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  CMT.CommunicationAction ->
  m ()
dispatchCommunicationAction driverId = \case
  CMT.NoCommunication -> pure ()
  CMT.FcmNotification params -> do
    -- TODO: Integrate with actual FCM dispatch via Tools.Notifications (e.g. Notify.notifyDriver)
    -- once the behavior management notification templates are registered in the FCM notification system.
    logInfo $ "FCM notification for driver " <> driverId.getId <> ": " <> params.templateKey
    logInfo $ "[AUDIT] Consequence communication dispatched: type=FCM_NOTIFICATION, target=driver:" <> driverId.getId <> ", templateKey=" <> params.templateKey <> ", status=STUB_ONLY"
  CMT.InAppOverlay params -> do
    -- TODO: Integrate with actual overlay dispatch via Tools.Notifications.sendOverlay
    -- once overlay templates are configured for behavior management directives.
    logInfo $ "In-app overlay for driver " <> driverId.getId <> ": " <> params.overlayKey
    logInfo $ "[AUDIT] Consequence communication dispatched: type=IN_APP_OVERLAY, target=driver:" <> driverId.getId <> ", overlayKey=" <> params.overlayKey <> ", status=STUB_ONLY"
  CMT.InAppMessage params -> do
    logInfo $ "In-app message for driver " <> driverId.getId <> ": " <> params.messageKey
    logInfo $ "[AUDIT] Consequence communication dispatched: type=IN_APP_MESSAGE, target=driver:" <> driverId.getId <> ", messageKey=" <> params.messageKey <> ", status=STUB_ONLY"
  CMT.SmsCommunication params -> do
    -- TODO: Integrate with SMS sending infrastructure (e.g. Tools.SMS or Exotel/Gupshup provider)
    logWarning $ "SMS dispatch not implemented — dropping SMS for driver " <> driverId.getId <> ": " <> params.templateKey
    logInfo $ "[AUDIT] Consequence communication dispatched: type=SMS, target=driver:" <> driverId.getId <> ", templateKey=" <> params.templateKey <> ", status=NOT_IMPLEMENTED"
  CMT.BadgeCommunication params -> do
    -- TODO: Integrate with badge/gamification system once available
    logWarning $ "Badge dispatch not implemented — dropping badge for driver " <> driverId.getId <> ": " <> params.badgeKey
    logInfo $ "[AUDIT] Consequence communication dispatched: type=BADGE, target=driver:" <> driverId.getId <> ", badgeKey=" <> params.badgeKey <> ", status=NOT_IMPLEMENTED"
