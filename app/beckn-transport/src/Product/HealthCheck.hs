module Product.HealthCheck (healthCheck) where

import qualified App.Types as App
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude

healthCheck :: Text -> App.FlowHandler Text
healthCheck _ = withFlowHandler $ do
  mbTime <- Redis.getKeyRedis "beckn:allocation:service"
  maybe markAsDead checkLastUpdateTime mbTime
  where
    markAsDead = throwError500 "Service is dead."
    checkLastUpdateTime lastUpdateTime = do
      now <- getCurrTime
      let diffTime = diffUTCTime now lastUpdateTime
      if diffTime > 10
        then markAsDead
        else return "Service is up!"