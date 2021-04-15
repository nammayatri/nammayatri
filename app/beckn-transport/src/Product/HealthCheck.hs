module Product.HealthCheck (healthCheck) where

import qualified App.Types as App
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Control.Concurrent.STM.TMVar (isEmptyTMVar)
import Data.Time (diffUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Types.Error
import Utils.Common

healthCheck :: TMVar () -> App.FlowHandler Text
healthCheck shutdown = withFlowHandlerAPI $ do
  isNotShuttingDown <- L.runIO $ liftIO $ atomically $ isEmptyTMVar shutdown
  if isNotShuttingDown
    then do
      mbTime <- Redis.getKeyRedis "beckn:allocation:service"
      maybe markAsDead checkLastUpdateTime mbTime
    else markAsDead
  where
    markAsDead = throwError ServiceUnavailable
    checkLastUpdateTime lastUpdateTime = do
      now <- getCurrentTime
      let diffTime = diffUTCTime now lastUpdateTime
      if diffTime > 10
        then markAsDead
        else return "Service is up!"
