module MockAppSearch where

import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import qualified EulerHS.Types as T
import Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified "mock-app-backend" Product.Trigger as MockAppTrigger
import Servant.Client
import Test.Hspec

spec :: Spec
spec = do
  mockAppManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl =
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = "localhost",
            baseUrlPort = 8016,
            baseUrlPath = "/v1"
          }
      appClientEnv = mkClientEnv mockAppManager appBaseUrl
      loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True,
            T._logFilePath = "/tmp/mock-app-backend-test",
            T._isAsync = False
          }
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Mock App Backend Search Api" $
      it "should return valid ack response" $
        \_flowRt ->
          do
            initiateSearchRes <- runClient appClientEnv $ triggerSearchReq MockAppTrigger.NoSearchResult
            initiateSearchRes `shouldSatisfy` isRight
