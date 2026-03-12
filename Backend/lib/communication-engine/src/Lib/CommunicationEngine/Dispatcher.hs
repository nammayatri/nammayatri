{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.CommunicationEngine.Dispatcher
  ( dispatchCommunications,
    dispatchCommunication,
  )
where

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.CommunicationEngine.Parser (parseDirective)
import Lib.CommunicationEngine.Types
import Lib.RuleOrchestrator.Types (CommunicationDirective)

-- | Dispatch all communication directives for a given entity.
--
-- For each directive:
-- 1. Parse into typed CommunicationAction
-- 2. Record for audit (via typeclass)
-- 3. Dispatch to the appropriate handler method
-- 4. Respect delay if specified (fork a delayed action)
-- 5. Log any errors but continue processing remaining directives
--
-- @entityId@ - the driver or rider ID to communicate with
-- @directives@ - communication directives from rule-orchestrator output
dispatchCommunications ::
  ( CommunicationHandler m,
    MonadFlow m
  ) =>
  Text -> -- entityId
  [CommunicationDirective] ->
  m [CommunicationResult]
dispatchCommunications entityId directives =
  forM directives $ \directive -> do
    case parseDirective directive of
      Left err -> do
        logError $ "Failed to parse communication directive for entity " <> entityId <> ": " <> err
        return $
          CommunicationResult
            { action = NoCommunication,
              success = False,
              errorMessage = Just err
            }
      Right action ->
        if directive.delaySeconds > 0
          then do
            -- Fork a delayed communication
            let delaySecs = fromIntegral directive.delaySeconds
            logDebug $ "Scheduling delayed communication (" <> show directive.delaySeconds <> "s) for entity " <> entityId
            fork ("delayed-comm-" <> entityId) $ do
              threadDelaySec delaySecs
              void $ dispatchCommunication entityId action
            return $
              CommunicationResult
                { action = action,
                  success = True,
                  errorMessage = Nothing
                }
          else dispatchCommunication entityId action

-- | Dispatch a single parsed communication action.
dispatchCommunication ::
  ( CommunicationHandler m,
    MonadFlow m
  ) =>
  Text -> -- entityId
  CommunicationAction ->
  m CommunicationResult
dispatchCommunication entityId action = do
  result <- try @_ @SomeException $ do
    -- Record for audit trail
    recordCommunication entityId action (A.toJSON action)
    -- Dispatch to handler
    case action of
      NoCommunication -> handleNoCommunication entityId
      FcmNotification params -> sendFcmNotification entityId params
      InAppOverlay params -> sendInAppOverlay entityId params
      InAppMessage params -> sendInAppMessage entityId params
      SmsCommunication params -> sendSms entityId params
      BadgeCommunication params -> sendBadge entityId params
  case result of
    Right () -> do
      logDebug $ "Successfully dispatched communication " <> show action <> " for entity " <> entityId
      return $
        CommunicationResult
          { action = action,
            success = True,
            errorMessage = Nothing
          }
    Left err -> do
      logError $ "Failed to dispatch communication " <> show action <> " for entity " <> entityId <> ": " <> show err
      return $
        CommunicationResult
          { action = action,
            success = False,
            errorMessage = Just (show err)
          }

