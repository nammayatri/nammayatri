module Product.HealthCheck (healthCheck) where

import qualified App.Types as App
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Utils.Common
import EulerHS.Prelude

healthCheck :: Text -> App.FlowHandler Text
healthCheck _ = withFlowHandler $ do
  mbTime <- Redis.getKeyRedis "beckn:allocation:service"
  case mbTime of
    Just a -> checkLastUpdateTime a
    Nothing -> throwError500 ""
  where
    checkLastUpdateTime a = return a
