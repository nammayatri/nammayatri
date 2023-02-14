 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module PublicTransport.HealthCheck where

import Common
import HSpec
import Kernel.Prelude
import PublicTransport.Common (callMockPublicTransportBpp, callPublicTransportBap)
import Servant hiding (Context)
import Servant.Client

type HealthCheckAPI = Get '[JSON] Text

healthCheckClientM :: ClientM Text
healthCheckClientM = client (Proxy :: Proxy HealthCheckAPI)

spec :: Spec
spec = describe "Testing healthcheck" $
  it "BAP, BPP, gateway servers should be up" $ do
    gwResult <- callGateway healthCheckClientM
    gwResult `shouldBe` "Gateway is UP"
    bapResult <- callPublicTransportBap healthCheckClientM
    bapResult `shouldBe` "App is UP"
    mockBppResult <- callMockPublicTransportBpp healthCheckClientM
    mockBppResult `shouldBe` "Mock is up!"
