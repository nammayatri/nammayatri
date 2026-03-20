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
  forM_ actions $ \action -> do
    result <- try @_ @SomeException $ dispatchConsequence ctx driverId action
    case result of
      Right () -> logDebug $ "Consequence executed for driver " <> driverId.getId <> ": " <> show action
      Left err -> logError $ "Consequence failed for driver " <> driverId.getId <> ": " <> show err

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
    logDebug $ "Charge fee requested for driver " <> driverId.getId <> ": " <> show params.penaltyAmount <> " " <> params.currency
    pure ()

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
    logDebug $ "FCM notification for driver " <> driverId.getId <> ": " <> params.templateKey
  CMT.InAppOverlay params ->
    logDebug $ "In-app overlay for driver " <> driverId.getId <> ": " <> params.overlayKey
  CMT.InAppMessage params ->
    logDebug $ "In-app message for driver " <> driverId.getId <> ": " <> params.messageKey
  CMT.SmsCommunication params ->
    logDebug $ "SMS for driver " <> driverId.getId <> ": " <> params.templateKey
  CMT.BadgeCommunication params ->
    logDebug $ "Badge for driver " <> driverId.getId <> ": " <> params.badgeKey
