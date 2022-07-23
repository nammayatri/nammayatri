module Mobility.ARDU.HealthCheck where

import Common (getAppBaseUrl)
import Data.Text.Encoding as DT
import EulerHS.Prelude
import qualified Mobility.ARDU.APICalls as API
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant hiding (Context)
import Servant.Client
import Test.Hspec
import Utils

type HealthCheckAPI = Get '[JSON] Text

healthCheckBackendC :: ClientM Text
healthCheckBackendC = client (Proxy :: Proxy HealthCheckAPI)

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl = getAppBaseUrl
      appClientEnv = mkClientEnv appManager appBaseUrl
      driverFlowClientEnv = mkClientEnv appManager API.getDriverOfferBppBaseUrl
      gatewayClientEnv =
        mkClientEnv appManager $
          API.getDriverOfferBppBaseUrl
            { baseUrlPort = 8015,
              baseUrlPath = "/v1"
            }
  describe "Testing App Backend APIs" $
    it "Testing health check API" $
      hspec $
        it "Health Check API should return success" do
          appResult <- runClient appClientEnv healthCheckBackendC
          appResult `shouldBe` Right (DT.decodeUtf8 "App is UP")
          driverOfferBppResult <- runClient driverFlowClientEnv healthCheckBackendC
          driverOfferBppResult `shouldBe` Right (DT.decodeUtf8 "App is UP")
          gwResult <- runClient gatewayClientEnv healthCheckBackendC
          gwResult `shouldBe` Right (DT.decodeUtf8 "Gateway is UP")
