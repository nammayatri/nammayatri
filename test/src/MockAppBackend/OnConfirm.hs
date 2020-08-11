module MockAppBackend.OnConfirm where

import Data.Time
import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import MockAppBackend.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import Utils

spec :: Spec
spec = do
  mockAppManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv mockAppManager mockAppBaseUrl
      loggerCfg = getLoggerCfg "mock-app-backend"
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Mock App Backend OnConfirm Api" $
      it "should return valid ack response" $
        \_flowRt ->
          do
            now <- getCurrentTime
            let ctx = buildContext "on_confirm" "dummy-txn-id" now
                onConfirmReq = buildOnConfirmReq ctx
            eitherConfirmCbRes <- runClient appClientEnv $ onConfirmFlow "" onConfirmReq
            eitherConfirmCbRes `shouldSatisfy` isRight
