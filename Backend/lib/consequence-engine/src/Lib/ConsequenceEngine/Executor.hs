{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.ConsequenceEngine.Executor
  ( executeConsequences,
    executeConsequence,
  )
where

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.ConsequenceEngine.Parser (parseDirective)
import Lib.ConsequenceEngine.Types

-- | Execute all consequence directives for a given entity.
--
-- For each directive:
-- 1. Parse into typed ConsequenceAction
-- 2. Record for audit (via typeclass)
-- 3. Dispatch to the appropriate handler method
-- 4. Log any errors but continue processing remaining directives
--
-- @entityId@ - the driver or rider ID the consequence applies to
-- @directives@ - consequence directives from rule-orchestrator output
executeConsequences ::
  ( ConsequenceHandler m,
    MonadFlow m
  ) =>
  Text -> -- entityId
  [ConsequenceDirective] ->
  m [ConsequenceResult]
executeConsequences entityId directives =
  forM directives $ \directive -> do
    case parseDirective directive of
      Left err -> do
        logError $ "Failed to parse consequence directive for entity " <> entityId <> ": " <> err
        return $
          ConsequenceResult
            { action = NoAction,
              success = False,
              errorMessage = Just err
            }
      Right action -> executeConsequence entityId action

-- | Execute a single parsed consequence action.
executeConsequence ::
  ( ConsequenceHandler m,
    MonadFlow m
  ) =>
  Text -> -- entityId
  ConsequenceAction ->
  m ConsequenceResult
executeConsequence entityId action = do
  result <- try @_ @SomeException $ do
    -- Record for audit trail
    recordConsequence entityId action (A.toJSON action)
    -- Dispatch to handler
    case action of
      NoAction -> handleNoAction entityId
      Nudge params -> handleNudge entityId params
      Warn params -> handleWarn entityId params
      SoftBlock params -> handleSoftBlock entityId params
      FeatureBlock params -> handleFeatureBlock entityId params
      HardBlock params -> handleHardBlock entityId params
      PermanentBlock params -> handlePermanentBlock entityId params
      ChargeFee params -> handleChargeFee entityId params
      IncrementCounter _ -> logDebug $ "INCREMENT_COUNTER skipped in generic executor for entity " <> entityId <> " (handled by app dispatcher)"
  case result of
    Right () -> do
      logDebug $ "Successfully executed consequence " <> show action <> " for entity " <> entityId
      return $
        ConsequenceResult
          { action = action,
            success = True,
            errorMessage = Nothing
          }
    Left err -> do
      logError $ "Failed to execute consequence " <> show action <> " for entity " <> entityId <> ": " <> show err
      return $
        ConsequenceResult
          { action = action,
            success = False,
            errorMessage = Just (show err)
          }
