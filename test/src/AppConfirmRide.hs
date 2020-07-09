module AppConfirmRide where

import qualified "app-backend" App as AppBE
import qualified "beckn-transport" App as TransporterBE
import "app-backend" App.Routes
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App
import Beckn.Types.Common as Common
import Beckn.Types.Core.Ack as Ack
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Products as Products
import Data.Text.Encoding as DT
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
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
import qualified "app-backend" Types.API.Case as AppCase
import qualified "beckn-transport" Types.API.Case as TbeCase

startServers :: IO (ThreadId, ThreadId)
startServers = do
  appTid <- forkIO AppBE.runAppBackend
  tbeTid <- forkIO TransporterBE.runTransporterBackendApp
  return (appTid, tbeTid)

withBecknServers :: IO () -> IO ()
withBecknServers action =
  bracket
    startServers
    (\(appTid, tbeTid) -> killThread appTid >> killThread tbeTid)
    (const $ threadDelay 100000 >> action)

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient clientEnv x = runClientM x clientEnv

spec :: Spec
spec = do
  reqHeadersKey <- runIO V.newKey
  appManager <- runIO $ Client.newManager tlsManagerSettings
  tbeManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl =
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = "localhost",
            baseUrlPort = 8013,
            baseUrlPath = "/v1"
          }
      transporterBaseUrl = appBaseUrl {baseUrlPort = 8014}
      appClientEnv = mkClientEnv appManager appBaseUrl
      tbeClientEnv = mkClientEnv tbeManager transporterBaseUrl
      loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True,
            T._logFilePath = "/tmp/app-backend-confirmride-test",
            T._isAsync = False
          }
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Testing App Backend APIs" $
      it "Testing API flow for App confirm ride" $
        \flowRt ->
          hspec $
            around_ withBecknServers $
              it "API flow should succeed for Transporter -> Accepts & App consumer -> Confirms" $
                do
                  -- Do an App Search
                  transactionId <- UUID.nextUUID
                  sreq <- buildSearchReq $ UUID.toText $ fromJust transactionId
                  ackResult <- runClient appClientEnv (searchServices appRegistrationToken sreq)
                  ackResult `shouldSatisfy` isRight
                  -- If we reach here, the 'Right' pattern match will always succeed
                  let Right ackResponse = ackResult
                      appCaseid = (Ack._message . Common._message) ackResponse
                  -- Do a List Leads and retrieve transporter case id
                  caseReqResult <- runClient tbeClientEnv buildListLeads
                  caseReqResult `shouldSatisfy` isRight
                  -- If we reach here, the 'Right' pattern match will always succeed
                  let Right caseListRes = caseReqResult
                      caseList = filter (\caseRes -> (Case._shortId . TbeCase._case) caseRes == appCaseid) caseListRes
                      transporterCurrCaseid = (_getCaseId . Case._id . TbeCase._case . head) caseList
                  -- Transporter accepts the ride
                  accDecRideResult <- runClient tbeClientEnv (acceptOrDeclineRide appRegistrationToken transporterCurrCaseid buildUpdateCaseReq)
                  accDecRideResult `shouldSatisfy` isRight

-- Do a Case Status request for getting case product to confirm ride
-- on app side next
-- statusResResult <- runClient appClientEnv (buildCaseStatusRes appCaseid)
-- statusResResult `shouldSatisfy` isRight
-- let Right statusRes = statusResResult
--     caseProductId = (_getProductsId . Products._id . head . AppCase._product) statusRes
-- -- Confirm ride from app backend
-- confirmResult <- runClient appClientEnv (appConfirmRide appRegistrationToken $ buildAppConfirmReq appCaseid caseProductId)
-- confirmResult `shouldSatisfy` isRight
