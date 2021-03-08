module Product.HealthCheck (healthCheck) where

import qualified App.Types as App
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Utils.Common
import Control.Concurrent.STM.TMVar (isEmptyTMVar)
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude

healthCheck :: TMVar () -> App.FlowHandler Text
healthCheck shutdown = withFlowHandler $ do
  isNotShuttingDown <- L.runIO $ liftIO $ atomically $ isEmptyTMVar shutdown
  if isNotShuttingDown
    then do
      mbTime <- Redis.getKeyRedis "beckn:allocation:service"
      maybe markAsDead checkLastUpdateTime mbTime
    else markAsDead
  where
    markAsDead = throwError503 "SERVICE_UNAVAILABLE"
    checkLastUpdateTime lastUpdateTime = do
      now <- getCurrTime
      let diffTime = diffUTCTime now lastUpdateTime
      if diffTime > 10
        then markAsDead
        else return "Service is up!"
