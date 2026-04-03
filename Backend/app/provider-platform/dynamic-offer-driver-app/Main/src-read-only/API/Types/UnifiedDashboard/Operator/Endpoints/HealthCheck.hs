{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.UnifiedDashboard.Operator.Endpoints.HealthCheck where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Types.APISuccess
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Data.Aeson



type API = ("healthCheck" :> GetHealthCheckTest)
type GetHealthCheckTest = ("test" :> Get ('[JSON]) Kernel.Types.APISuccess.APISuccess)
newtype HealthCheckAPIs = HealthCheckAPIs {getHealthCheckTest :: (EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}
mkHealthCheckAPIs :: (Client EulerHS.Types.EulerClient API -> HealthCheckAPIs)
mkHealthCheckAPIs healthCheckClient = (HealthCheckAPIs {..})
                      where getHealthCheckTest = healthCheckClient
data HealthCheckUserActionType
    = GET_HEALTH_CHECK_TEST
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass ToSchema
instance ToJSON HealthCheckUserActionType
    where toJSON (GET_HEALTH_CHECK_TEST) = Data.Aeson.String "GET_HEALTH_CHECK_TEST"
instance FromJSON HealthCheckUserActionType
    where parseJSON (Data.Aeson.String "GET_HEALTH_CHECK_TEST") = pure GET_HEALTH_CHECK_TEST
          parseJSON _ = fail "GET_HEALTH_CHECK_TEST expected"

$(Data.Singletons.TH.genSingletons [(''HealthCheckUserActionType)])

