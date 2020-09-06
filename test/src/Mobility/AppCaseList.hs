module Mobility.AppCaseList where

import EulerHS.Prelude
import Mobility.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import qualified "app-backend" Types.API.Registration as Reg
import Utils

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl =
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = "localhost",
            baseUrlPort = 8013,
            baseUrlPath = "/v1"
          }
      appClientEnv = mkClientEnv appManager appBaseUrl
  describe "Testing App Backend APIs" $
    it "Testing List case API" $
      hspec $
        it "List case API should return success" do
          initiateLoginRes <- runClient appClientEnv initiateLoginReq
          initiateLoginRes `shouldSatisfy` isRight
          let Right Reg.InitiateLoginRes {..} = initiateLoginRes
          loginRes <- runClient appClientEnv (verifyLoginReq tokenId)
          loginRes `shouldSatisfy` isRight
          let Right Reg.LoginRes {..} = loginRes
          result <- runClient appClientEnv (buildCaseListRes registrationToken)
          result `shouldSatisfy` isRight
