module MockProviderBackend.Select where

import Data.Time
import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import MockProviderBackend.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import Utils

spec :: Spec
spec = do
  mockProviderManager <- runIO $ Client.newManager tlsManagerSettings
  let providerClientEnv = mkClientEnv mockProviderManager mockProviderBaseUrl
  let loggerCfg = getLoggerCfg "mock-provider-backend"
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Mock Provider Backend Select Api" $
      it "should return valid ack response" $
        \_flowRt ->
          do
            now <- getCurrentTime
            let ctx = buildContext "select" "dummy-txn-id" now
                selectReq = buildFMDSelectReq ctx
            initiateSelectRes <- runClient providerClientEnv $ selectFlow "" selectReq
            initiateSelectRes `shouldSatisfy` isRight
