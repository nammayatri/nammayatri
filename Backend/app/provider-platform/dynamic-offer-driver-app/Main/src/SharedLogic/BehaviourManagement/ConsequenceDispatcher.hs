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
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.BlockTracker as BT
import qualified Lib.BehaviorTracker.Recorder as BTRecorder
import qualified Lib.BehaviorTracker.Types as BTT
import qualified Lib.CommunicationEngine.Parser as CMParser
import qualified Lib.CommunicationEngine.Types as CMT
import qualified Lib.ConsequenceEngine.Parser as CEParser
import qualified Lib.ConsequenceEngine.Types as CET
import qualified Storage.Queries.DriverInformation as QDriverInformation
import Tools.Error (BlockReasonFlag (..))

-- | App-level context needed by some consequence handlers.
-- Callsites construct this from booking/ride context.
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
    Redis.HedisFlow m r
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
    Redis.HedisFlow m r
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
    let tag = fromMaybe params.featureName params.blockReasonTag
    BT.writeBlockAndCooldownKeys BTT.DRIVER driverId.getId BTT.FEATURE_BLOCK tag params.blockDurationHours params.blockReason (A.toJSON params.featureName) params.cooldownHours
  CET.SoftBlock params -> do
    logWarning $ "Soft blocking driver " <> driverId.getId <> " from tiers: " <> show params.blockedFeatures <> ", duration: " <> show params.blockDurationHours <> "h"
    now <- getCurrentTime
    let blockUntil = addUTCTime (fromIntegral params.blockDurationHours * 3600) now
    QDriverInformation.updateSoftBlock Nothing (Just blockUntil) (Just params.blockReason) (cast driverId)
    let tag = fromMaybe "SOFT_BLOCK" params.blockReasonTag
    BT.writeBlockAndCooldownKeys BTT.DRIVER driverId.getId BTT.SOFT_BLOCK tag params.blockDurationHours params.blockReason (A.toJSON params.blockedFeatures) params.cooldownHours
  CET.HardBlock params -> do
    logWarning $ "Hard blocking driver " <> driverId.getId <> ", duration: " <> show params.blockDurationHours <> "h"
    let reasonFlag = parseBlockReasonFlag params.blockReasonTag
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
      (Just False)
      Nothing
      reasonFlag
    let tag = fromMaybe "HARD_BLOCK" params.blockReasonTag
    BT.writeBlockAndCooldownKeys BTT.DRIVER driverId.getId BTT.HARD_BLOCK tag params.blockDurationHours params.blockReason (A.Object mempty) params.cooldownHours
  CET.PermanentBlock params -> do
    logWarning $ "Permanently blocking driver " <> driverId.getId <> ", reason: " <> params.blockReason
    QDriverInformation.updateBlockedState
      (cast driverId)
      True
      (Just "BehaviorManagementFramework")
      ctx.merchantId
      ctx.merchantOperatingCityId
      DTDBT.Application
    let tag = fromMaybe "PERMANENT_BLOCK" params.blockReasonTag
    BT.writeBlockKey BTT.DRIVER driverId.getId BTT.PERMANENT_BLOCK tag 0 params.blockReason (A.Object mempty)
  CET.Nudge _params -> pure ()
  CET.Warn _params -> pure ()
  CET.ChargeFee params -> do
    logInfo $ "Charge fee requested for driver " <> driverId.getId <> ": " <> show params.penaltyAmount <> " " <> params.currency
    pure ()
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

-- | Map blockReasonTag text to BlockReasonFlag enum
parseBlockReasonFlag :: Maybe Text -> BlockReasonFlag
parseBlockReasonFlag = \case
  Just "CancellationRateDaily" -> CancellationRateDaily
  Just "CancellationRateWeekly" -> CancellationRateWeekly
  Just "CancellationRate" -> CancellationRate
  Just "ExtraFareDaily" -> ExtraFareDaily
  Just "ExtraFareWeekly" -> ExtraFareWeekly
  Just "DrunkAndDriveViolation" -> DrunkAndDriveViolation
  Just "ByDashboard" -> ByDashboard
  _ -> CancellationRate -- default fallback

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
  let (actions, errors) = CMParser.parseDirectives directives
  unless (null errors) $
    logError $ "Communication parse errors for driver " <> driverId.getId <> ": " <> show errors
  forM_ actions $ \action -> do
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
  CMT.FcmNotification params ->
    logInfo $ "FCM notification for driver " <> driverId.getId <> ": " <> params.templateKey
  CMT.InAppOverlay params ->
    logInfo $ "In-app overlay for driver " <> driverId.getId <> ": " <> params.overlayKey
  CMT.InAppMessage params ->
    logInfo $ "In-app message for driver " <> driverId.getId <> ": " <> params.messageKey
  CMT.SmsCommunication params ->
    logInfo $ "SMS for driver " <> driverId.getId <> ": " <> params.templateKey
  CMT.BadgeCommunication params ->
    logInfo $ "Badge for driver " <> driverId.getId <> ": " <> params.badgeKey
