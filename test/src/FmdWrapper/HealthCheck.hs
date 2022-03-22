module FmdWrapper.HealthCheck where

import Common
import Data.Text.Encoding as DT
import EulerHS.Prelude
import Fmd
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant hiding (Context)
import Servant.Client
import Test.Hspec
import Utils

type HealthCheckAPI = Get '[JSON] Text

healthCheckApp :: ClientM Text
healthCheckApp = client (Proxy :: Proxy HealthCheckAPI)

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let fmdClientEnv = mkClientEnv appManager fmdWrapperBaseUrl
  describe "Testing FMD APIs" $
    it "Testing health check API" $
      hspec $
        it "Health Check API should return success" do
          gwResult <- runClient gatewayClientEnv healthCheckApp
          gwResult `shouldBe` Right (DT.decodeUtf8 "Gateway is UP")
          fmdResult <- runClient fmdClientEnv healthCheckApp
          fmdResult `shouldBe` Right (DT.decodeUtf8 "FMD wrapper backend is UP")
