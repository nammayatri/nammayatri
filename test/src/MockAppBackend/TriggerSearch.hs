module MockAppBackend.TriggerSearch where

import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import MockAppBackend.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified "mock-app-backend" Product.Trigger as MockAppTrigger
import Servant.Client
import Test.Hspec

spec :: Spec
spec = do
  mockAppManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv mockAppManager mockAppBaseUrl
      loggerCfg = getLoggerConfig "mock-app-backend"
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Mock App Backend Search Api" $
      it "should return valid ack response" $
        \_flowRt ->
          do
            initiateSearchRes <- runClient appClientEnv $ triggerSearchReq MockAppTrigger.NoSearchResult
            print $ "initiateSearchRes"
            print $ show initiateSearchRes
            initiateSearchRes `shouldSatisfy` isRight
