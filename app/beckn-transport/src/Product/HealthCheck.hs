module Product.HealthCheck (healthCheck) where

import App.BackgroundTaskManager.Types (FlowHandler)
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Data.Time (diffUTCTime)
import EulerHS.Prelude
import Types.Error
import Utils.Common

healthCheck :: FlowHandler Text
healthCheck = withFlowHandlerAPI $ do
  mbTime <- Redis.getKeyRedis "beckn:allocation:service"
  maybe markAsDead checkLastUpdateTime mbTime
  where
    markAsDead = throwError ServiceUnavailable
    checkLastUpdateTime lastUpdateTime = do
      now <- getCurrentTime
      let diffTime = diffUTCTime now lastUpdateTime
      if diffTime > 10
        then markAsDead
        else return "Service is up!"
