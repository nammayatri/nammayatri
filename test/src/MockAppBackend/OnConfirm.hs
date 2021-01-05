module MockAppBackend.OnConfirm where

import Common
import Data.Time.Clock.POSIX (getPOSIXTime)
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
      now <- getPOSIXTime
      let signature = decodeUtf8 $ signRequest onConfirmReq now mockAppSelfId mockAppApiKey
      eitherConfirmCbRes <- runClient appClientEnv $ onConfirmFlow (Just signature) onConfirmReq
      eitherConfirmCbRes `shouldSatisfy` isRight
