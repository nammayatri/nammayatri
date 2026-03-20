{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Utils.CircuitBreaker
  ( CircuitBreakerConfig (..),
    withCircuitBreaker,
    withCircuitBreakerAndRetry,
    mkDefaultConfig,
  )
where

import qualified Data.Text as T
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common

-- | Configuration for a circuit breaker instance.
data CircuitBreakerConfig = CircuitBreakerConfig
  { serviceName :: Text,
    failureThreshold :: Int,
    halfOpenAfterSec :: Int,
    enabled :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Create a default circuit breaker configuration for a given service.
mkDefaultConfig :: Text -> CircuitBreakerConfig
mkDefaultConfig name =
  CircuitBreakerConfig
    { serviceName = name,
      failureThreshold = 5,
      halfOpenAfterSec = 30,
      enabled = True
    }

-- Redis key helpers -----------------------------------------------------------

mkFailureCountKey :: Text -> Text
mkFailureCountKey svc = "cb:failures:" <> svc

mkCircuitStateKey :: Text -> Text
mkCircuitStateKey svc = "cb:state:" <> svc

-- Core logic ------------------------------------------------------------------

-- | Run an action with circuit breaker protection.
-- When the circuit is open the action is skipped and an error is thrown.
-- Failures are tracked in Redis; when the threshold is reached the circuit
-- opens for @halfOpenAfterSec@ seconds.
withCircuitBreaker ::
  ( MonadFlow m,
    Hedis.HedisFlow m r,
    MonadReader r m,
    Log m
  ) =>
  CircuitBreakerConfig ->
  m a ->
  m a
withCircuitBreaker cfg action
  | not cfg.enabled = action
  | otherwise = do
      isOpen <- checkCircuitOpen cfg
      if isOpen
        then do
          logWarning $ "Circuit breaker OPEN for " <> cfg.serviceName <> ", skipping call"
          throwError $ InternalError $ "Circuit breaker open for service: " <> cfg.serviceName
        else do
          result <- withTryCatch ("cb:" <> cfg.serviceName) action
          case result of
            Right val -> do
              resetFailureCount cfg
              pure val
            Left err -> do
              recordFailureAndMaybeTrip cfg
              throwError $ InternalError $ "Service " <> cfg.serviceName <> " failed: " <> T.pack (show err)

-- | Run an action with circuit breaker protection and a single retry.
-- On the first failure the action is retried once.  If the retry also fails
-- the failure is recorded and the exception propagates.
withCircuitBreakerAndRetry ::
  ( MonadFlow m,
    Hedis.HedisFlow m r,
    MonadReader r m,
    Log m
  ) =>
  CircuitBreakerConfig ->
  m a ->
  m a
withCircuitBreakerAndRetry cfg action
  | not cfg.enabled = action
  | otherwise = do
      isOpen <- checkCircuitOpen cfg
      if isOpen
        then do
          logWarning $ "Circuit breaker OPEN for " <> cfg.serviceName <> ", skipping call"
          throwError $ InternalError $ "Circuit breaker open for service: " <> cfg.serviceName
        else do
          result <- withTryCatch ("cb:" <> cfg.serviceName) action
          case result of
            Right val -> do
              resetFailureCount cfg
              pure val
            Left _firstErr -> do
              -- Retry once
              logInfo $ "Circuit breaker retrying for " <> cfg.serviceName
              retryResult <- withTryCatch ("cb:retry:" <> cfg.serviceName) action
              case retryResult of
                Right val -> do
                  resetFailureCount cfg
                  pure val
                Left retryErr -> do
                  recordFailureAndMaybeTrip cfg
                  throwError $ InternalError $ "Service " <> cfg.serviceName <> " failed after retry: " <> T.pack (show retryErr)

-- Internal helpers ------------------------------------------------------------

checkCircuitOpen ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  CircuitBreakerConfig ->
  m Bool
checkCircuitOpen cfg = do
  let stateKey = mkCircuitStateKey cfg.serviceName
  mbState :: Maybe Text <- Hedis.get stateKey
  pure $ mbState == Just "open"

recordFailureAndMaybeTrip ::
  ( MonadFlow m,
    Hedis.HedisFlow m r,
    Log m
  ) =>
  CircuitBreakerConfig ->
  m ()
recordFailureAndMaybeTrip cfg = do
  let countKey = mkFailureCountKey cfg.serviceName
  count <- Hedis.incr countKey
  -- Set a TTL on the failure counter so it resets if failures stop
  when (count == 1) $ void $ Hedis.expire countKey (cfg.halfOpenAfterSec * 2)
  when (fromIntegral count >= cfg.failureThreshold) $ do
    logError $ "Circuit breaker TRIPPED for " <> cfg.serviceName <> " after " <> show count <> " failures"
    let stateKey = mkCircuitStateKey cfg.serviceName
    Hedis.setExp stateKey ("open" :: Text) cfg.halfOpenAfterSec
    -- Reset failure counter
    void $ Hedis.del countKey

resetFailureCount ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  CircuitBreakerConfig ->
  m ()
resetFailureCount cfg = do
  let countKey = mkFailureCountKey cfg.serviceName
  void $ Hedis.del countKey
