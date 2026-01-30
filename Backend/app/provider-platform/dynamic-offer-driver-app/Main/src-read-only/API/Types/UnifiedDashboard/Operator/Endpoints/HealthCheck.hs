{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UnifiedDashboard.Operator.Endpoints.HealthCheck where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

type API = ("healthCheck" :> GetHealthCheckTest)

type GetHealthCheckTest = ("test" :> Get ('[JSON]) Kernel.Types.APISuccess.APISuccess)

newtype HealthCheckAPIs = HealthCheckAPIs {getHealthCheckTest :: (EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}

mkHealthCheckAPIs :: (Client EulerHS.Types.EulerClient API -> HealthCheckAPIs)
mkHealthCheckAPIs healthCheckClient = (HealthCheckAPIs {..})
  where
    getHealthCheckTest = healthCheckClient

data HealthCheckUserActionType
  = GET_HEALTH_CHECK_TEST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON HealthCheckUserActionType where
  toJSON (GET_HEALTH_CHECK_TEST) = Data.Aeson.String "GET_HEALTH_CHECK_TEST"

instance FromJSON HealthCheckUserActionType where
  parseJSON (Data.Aeson.String "GET_HEALTH_CHECK_TEST") = pure GET_HEALTH_CHECK_TEST
  parseJSON _ = fail "GET_HEALTH_CHECK_TEST expected"

$(Data.Singletons.TH.genSingletons [(''HealthCheckUserActionType)])
