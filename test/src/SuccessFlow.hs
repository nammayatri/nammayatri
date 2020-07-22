module SuccessFlow where

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
import qualified "app-backend" Types.API.Case as AppCase
import qualified "beckn-transport" Types.API.Case as TbeCase
import qualified "app-backend" Types.API.ProductInstance as AppPI
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
            T._logFilePath = "/tmp/app-backend-successflow-test",
            T._isAsync = False
          }
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Testing App Backend APIs" $
      it "Testing API flow for App confirm ride" $
        \_flowRt ->
          hspec $
            it "This would test happy flow, App is successfully able to book ride and Transporter complete trip" $
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

                -- List all confirmed rides (type = RIDEORDER)
                rideReqResult <- runClient tbeClientEnv (buildOrgRideReq PI.CONFIRMED Case.RIDEORDER)
                rideReqResult `shouldSatisfy` isRight

                -- Filter order productInstance
                let Right rideListRes = rideReqResult
                    tbePiList = TbePI._productInstance <$> rideListRes
                    transporterOrderPi = filter (\pi -> (_getProductInstanceId <$> PI._parentId pi) == Just productInstanceId) tbePiList
                    transporterOrderPiId = (PI._id . head) transporterOrderPi

                -- Assign Driver and Vehicle
                assignDriverVehicleResult <- runClient tbeClientEnv (rideUpdate appRegistrationToken transporterOrderPiId buildUpdatePIReq)
                assignDriverVehicleResult `shouldSatisfy` isRight

                -- Update RIDEORDER PI to INPROGRESS once driver starts his trip
                inProgressStatusResult <- runClient tbeClientEnv (rideUpdate appRegistrationToken transporterOrderPiId (buildUpdateStatusReq PI.INPROGRESS))
                inProgressStatusResult `shouldSatisfy` isRight

                piListResult <- runClient appClientEnv (buildListPIs PI.INPROGRESS)
                piListResult `shouldSatisfy` isRight

                -- Check if app RIDEORDER PI got updated to status INPROGRESS
                checkPiInResult piListResult productInstanceId

                -- Update RIDEORDER PI to COMPLETED once driver ends his trip
                completedStatusResult <- runClient tbeClientEnv (rideUpdate appRegistrationToken transporterOrderPiId (buildUpdateStatusReq PI.COMPLETED))
                completedStatusResult `shouldSatisfy` isRight

                piListResult <- runClient appClientEnv (buildListPIs PI.COMPLETED)
                piListResult `shouldSatisfy` isRight

                -- Check if app RIDEORDER PI got updated to status COMPLETED
                checkPiInResult piListResult productInstanceId
  where
    productInstances :: AppCase.StatusRes -> [AppCase.ProdInstRes]
    productInstances = AppCase._productInstance

    checkPiInResult :: Either ClientError [AppPI.ProductInstanceRes] -> Text -> Expectation
    checkPiInResult piListResult productInstanceId =
      let Right piListRes = piListResult
          appPiList = AppPI._productInstance <$> piListRes
          appOrderPI = filter (\pi -> (_getProductInstanceId <$> PI._parentId pi) == Just productInstanceId) appPiList
       in length appOrderPI `shouldBe` 1
