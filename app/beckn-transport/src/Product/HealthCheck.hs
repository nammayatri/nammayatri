module Product.HealthCheck (healthCheck) where

import qualified App.Types as App
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude
import System.Environment

healthCheck :: App.FlowHandler Text
healthCheck = do
  mode <- liftIO $ lookupEnv "APP_MODE"
  case mode of
    Just "API" -> return "App is UP"
    Just "BACKGROUND_TASK_MANAGER" -> serviceHealthCheck
    _ -> return "App is UP"
  where
    serviceHealthCheck = withFlowHandler $ do
      mbTime <- Redis.getKeyRedis "beckn:allocation:service"
      maybe markAsDead checkLastUpdateTime mbTime
    markAsDead = throwError500 "Service is dead."
    checkLastUpdateTime lastUpdateTime = do
      now <- getCurrTime
      let diffTime = diffUTCTime now lastUpdateTime
      if diffTime > 10
        then markAsDead
        else return "Service is up!"
