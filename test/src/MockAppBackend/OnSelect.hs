module MockAppBackend.OnSelect where

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
  describe "Mock App Backend OnSelect Api" $
    it "should return valid ack response" do
      ctx <- buildContext "on_select" "dummy-txn-id" Nothing Nothing
      let onSelectReq = buildOnSelectReq ctx
      eitherSelectCbRes <- runClient appClientEnv $ onSelectFlow mockAppApiKey onSelectReq
      eitherSelectCbRes `shouldSatisfy` isRight
