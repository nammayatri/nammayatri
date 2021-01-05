module MockProviderBackend.Select where

import Common
import Data.Time.Clock.POSIX
import EulerHS.Prelude
import Fmd
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
  describe "Mock Provider Backend Select Api" $
    it "should return valid ack response" do
      ctx <- buildContext "select" "dummy-txn-id" Nothing Nothing
      let selectReq = buildFMDSelectReq ctx
      now <- getPOSIXTime
      let signature = decodeUtf8 $ signRequest selectReq now mockProviderSelfId mockProviderApiKey
      initiateSelectRes <- runClient providerClientEnv $ selectFlow (Just signature) selectReq
      initiateSelectRes `shouldSatisfy` isRight
