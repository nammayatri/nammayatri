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
    CircuitBreakerState (..),
    withCircuitBreaker,
    withCircuitBreakerAndRetry,
    getCircuitBreakerState,
    mkDefaultConfig,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common
import qualified Prometheus as P
import Utils.Common.Sanitize (sanitizeShowError)
import System.IO.Unsafe (unsafePerformIO)
import Utils.Retry (defaultRetryConfig, withTransientRetry)

data CircuitBreakerConfig = CircuitBreakerConfig
  { serviceName :: Text,
    failureThreshold :: Int,
    halfOpenAfterSec :: Int,
    enabled :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CircuitBreakerState = CBClosed | CBOpen | CBHalfOpen
  deriving (Show, Eq)

mkDefaultConfig :: Text -> CircuitBreakerConfig
mkDefaultConfig name =
  CircuitBreakerConfig
    { serviceName = name,
      failureThreshold = 5,
      halfOpenAfterSec = 30,
      enabled = True
    }

{-# NOINLINE cbStateGauge #-}
cbStateGauge :: P.Vector P.Label1 P.Gauge
cbStateGauge =
  unsafePerformIO $
    P.register $
      P.vector "service" $
        P.gauge $
          P.Info "circuit_breaker_state" "Circuit breaker state (0=closed, 1=half_open, 2=open)"

-- Redis keys
mkStateKey :: Text -> Text
mkStateKey svc = "cb:" <> svc <> ":state"

mkFailuresKey :: Text -> Text
mkFailuresKey svc = "cb:" <> svc <> ":failures"

mkOpenedAtKey :: Text -> Text
mkOpenedAtKey svc = "cb:" <> svc <> ":openedAt"

getCircuitBreakerState ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  CircuitBreakerConfig ->
  m CircuitBreakerState
getCircuitBreakerState config = do
  stateVal <- Hedis.get (mkStateKey config.serviceName)
  case stateVal of
    Just ("open" :: Text) -> do
      openedAtVal <- Hedis.get (mkOpenedAtKey config.serviceName)
      case openedAtVal of
        Just (openedAtMs :: Integer) -> do
          nowMs <- getCurrentTimeMillis
          if (nowMs - openedAtMs) >= fromIntegral (config.halfOpenAfterSec * 1000)
            then pure CBHalfOpen
            else pure CBOpen
        Nothing -> pure CBOpen
    _ -> pure CBClosed

setStateMetric :: MonadIO m => Text -> CircuitBreakerState -> m ()
setStateMetric svc st = liftIO $ P.withLabel cbStateGauge svc $ \g ->
  P.setGauge g (stateToDouble st)
  where
    stateToDouble :: CircuitBreakerState -> Double
    stateToDouble CBClosed = 0
    stateToDouble CBHalfOpen = 1
    stateToDouble CBOpen = 2

withCircuitBreakerAndRetry ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  CircuitBreakerConfig ->
  m a ->
  m a
withCircuitBreakerAndRetry config action =
  withCircuitBreaker config $ withTransientRetry (defaultRetryConfig config.serviceName) action

withCircuitBreaker ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  CircuitBreakerConfig ->
  m a ->
  m a
withCircuitBreaker config action
  | not config.enabled = action
  | otherwise = do
  st <- getCircuitBreakerState config
  case st of
    CBOpen -> do
      logWarning $ "Circuit breaker OPEN for " <> config.serviceName <> " - rejecting request"
      setStateMetric config.serviceName CBOpen
      throwError $ InternalError $ "Service " <> config.serviceName <> " circuit breaker is open"
    CBHalfOpen -> do
      logWarning $ "Circuit breaker HALF-OPEN for " <> config.serviceName <> " - allowing trial request"
      setStateMetric config.serviceName CBHalfOpen
      executeAction config CBHalfOpen action
    CBClosed -> do
      setStateMetric config.serviceName CBClosed
      executeAction config CBClosed action

executeAction ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  CircuitBreakerConfig ->
  CircuitBreakerState ->
  m a ->
  m a
executeAction config previousState action = do
  result <- withTryCatch ("cb:" <> config.serviceName) action
  case result of
    Right val -> do
      Hedis.del (mkFailuresKey config.serviceName)
      when (previousState /= CBClosed) $ do
        logWarning $ "Circuit breaker CLOSED for " <> config.serviceName <> " - service recovered"
        Hedis.del (mkStateKey config.serviceName)
        Hedis.del (mkOpenedAtKey config.serviceName)
        setStateMetric config.serviceName CBClosed
      pure val
    Left e -> do
      failCount <- Hedis.incr (mkFailuresKey config.serviceName)
      let ttl = config.halfOpenAfterSec + 300
      Hedis.expire (mkFailuresKey config.serviceName) ttl
      if failCount >= fromIntegral config.failureThreshold
        then do
          logWarning $ "Circuit breaker OPENED for " <> config.serviceName <> " after " <> show failCount <> " failures"
          nowMs <- getCurrentTimeMillis
          Hedis.setExp (mkStateKey config.serviceName) ("open" :: Text) ttl
          Hedis.setExp (mkOpenedAtKey config.serviceName) nowMs ttl
          setStateMetric config.serviceName CBOpen
        else when (previousState == CBHalfOpen) $ do
          logWarning $ "Circuit breaker re-OPENED for " <> config.serviceName <> " - half-open trial failed"
          nowMs <- getCurrentTimeMillis
          Hedis.setExp (mkStateKey config.serviceName) ("open" :: Text) ttl
          Hedis.setExp (mkOpenedAtKey config.serviceName) nowMs ttl
          setStateMetric config.serviceName CBOpen
      logError $ "Circuit breaker error for " <> config.serviceName <> ": " <> sanitizeShowError e
      throwError $ InternalError $ "Service temporarily unavailable: " <> config.serviceName

getCurrentTimeMillis :: MonadFlow m => m Integer
getCurrentTimeMillis = round . utcToMilliseconds <$> getCurrentTime
