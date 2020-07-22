module AppCancelRide where

import Beckn.Types.App
import Beckn.Types.Common as Common
import Beckn.Types.Core.Ack as Ack
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import qualified EulerHS.Types as T
import Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import qualified Types.API.Cancel as CancelAPI
import qualified "app-backend" Types.API.Case as AppCase
import qualified "beckn-transport" Types.API.Case as TbeCase
import qualified "beckn-transport" Types.API.ProductInstance as TbePI

spec :: Spec
spec = do
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
            T._logFilePath = "/tmp/app-backend-appcancelride-test",
            T._isAsync = False
          }
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Testing App Backend APIs" $
      it "Testing API flow for ride cancelled by App" $
        \_flowRt ->
          hspec $
            it "This would pass if Consumer is able to successfully book ride and then cancel it" $
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
                statusResResult <- runClient appClientEnv (buildCaseStatusRes appCaseid)
                statusResResult `shouldSatisfy` isRight
                let Right statusRes = statusResResult
                    productInstanceId = _getProductInstanceId . AppCase._id . head . productInstances $ statusRes
                -- Confirm ride from app backend
                confirmResult <- runClient appClientEnv (appConfirmRide appRegistrationToken $ buildAppConfirmReq appCaseid productInstanceId)
                confirmResult `shouldSatisfy` isRight

                -- cancel request initiated by App
                now <- getFutureTime
                txnId <- UUID.nextUUID
                cancelResult <- runClient appClientEnv (cancelRide appRegistrationToken $ buildAppCancelReq (UUID.toText $ fromJust txnId) now productInstanceId CancelAPI.PRODUCT_INSTANCE)
                cancelResult `shouldSatisfy` isRight

                -- List all cancelled rides (type = RIDEORDER)
                piListResult <- runClient tbeClientEnv (buildOrgRideReq PI.CANCELLED Case.RIDEORDER)
                piListResult `shouldSatisfy` isRight

                -- Check if app RIDEORDER PI got updated to status INPROGRESS
                let Right piListRes = piListResult
                    tbePiList = TbePI._productInstance <$> piListRes
                    tbeOrderPI = filter (\pi -> (_getProductInstanceId <$> PI._parentId pi) == Just productInstanceId) tbePiList
                length tbeOrderPI `shouldBe` 1
  where
    productInstances :: AppCase.StatusRes -> [AppCase.ProdInstRes]
    productInstances = AppCase._productInstance
