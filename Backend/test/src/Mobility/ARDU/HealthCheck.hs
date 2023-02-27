{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
          driverOfferBppResult <- runClient driverFlowClientEnv API.ui.healthCheck
          driverOfferBppResult `shouldBe` Right (DT.decodeUtf8 "App is UP")
          gwResult <- runClient gatewayClientEnv healthCheckBackendC
          gwResult `shouldBe` Right (DT.decodeUtf8 "Gateway is UP")
