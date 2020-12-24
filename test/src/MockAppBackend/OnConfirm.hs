module MockAppBackend.OnConfirm where

import EulerHS.Prelude
import Fmd
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
  describe "Mock App Backend OnConfirm Api" $
    it "should return valid ack response" do
      ctx <- buildContext "on_confirm" "dummy-txn-id" Nothing Nothing
      let onConfirmReq = buildOnConfirmReq ctx
      eitherConfirmCbRes <- runClient appClientEnv $ onConfirmFlow onConfirmReq
      eitherConfirmCbRes `shouldSatisfy` isRight
