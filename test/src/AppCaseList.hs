module AppCaseList where

import qualified "app-backend" App as AppBE
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import qualified EulerHS.Types as T
import Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant hiding (Context)
import Servant.Client
import Test.Hspec

startServer :: IO ThreadId
startServer = forkIO AppBE.runAppBackend

withBecknServer :: IO () -> IO ()
withBecknServer action = do
  bracket
    startServer
    killThread
    (const $ threadDelay 100000 >> action)

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient clientEnv x = runClientM x clientEnv

spec :: Spec
spec = do
  reqHeadersKey <- runIO V.newKey
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl =
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = "localhost",
            baseUrlPort = 8013,
            baseUrlPath = "/v1"
          }
      appClientEnv = mkClientEnv appManager appBaseUrl
      loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True,
            T._logFilePath = "/tmp/app-backend-caselist-test",
            T._isAsync = False
          }
  around (withFlowRuntime (Just loggerCfg))
    $ describe "Testing App Backend APIs"
    $ it "Testing List case API"
    $ \flowRt -> do
      hspec
        $ around_ withBecknServer
        $ it "List case API should return success"
        $ do
          result <- runClient appClientEnv buildCaseListRes
          result `shouldSatisfy` isRight
