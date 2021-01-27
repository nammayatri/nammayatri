{-# LANGUAGE OverloadedLabels #-}

module Mobility.SuccessFlow where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as PI
import Data.Text (isSuffixOf)
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import EulerHS.Prelude
import Mobility.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import qualified "app-backend" Types.API.Case as AppCase
import qualified "beckn-transport" Types.API.Case as TbeCase
import qualified "app-backend" Types.API.Common as AppCommon
import qualified "app-backend" Types.API.ProductInstance as AppPI
import qualified "beckn-transport" Types.API.ProductInstance as TbePI
import Utils

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl = getAppBaseUrl
      transporterBaseUrl = getTransporterBaseUrl
      appClientEnv = mkClientEnv appManager appBaseUrl
      tbeClientEnv = mkClientEnv appManager transporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for successful booking and completion of ride" do
      -- Do an App Search
      transactionId <- UUID.nextUUID
      sreq <- buildSearchReq $ UUID.toText $ fromJust transactionId
      ackResult <- runClient appClientEnv (searchServices appRegistrationToken sreq)
      ackResult `shouldSatisfy` isRight
      -- If we reach here, the 'Right' pattern match will always succeed
      let Right ackResponse = ackResult
          appCaseid = AppCommon._message $ ackResponse ^. #message
      theCase :| [] <- poll $ do
        -- Do a List Leads and retrieve transporter case id
        caseReqResult <- runClient tbeClientEnv buildListLeads
        caseReqResult `shouldSatisfy` isRight
        -- If we reach here, the 'Right' pattern match will always succeed
        let Right caseListRes = caseReqResult
        return . nonEmpty $
          filter
            (\caseRes -> appCaseid `isSuffixOf` Case._shortId (TbeCase._case caseRes))
            caseListRes
      let transporterCurrCaseid = _getCaseId . Case._id $ TbeCase._case theCase
      -- Transporter accepts the ride
      accDecRideResult <-
        runClient
          tbeClientEnv
          (acceptOrDeclineRide appRegistrationToken transporterCurrCaseid buildUpdateCaseReq)
      accDecRideResult `shouldSatisfy` isRight

      productInstance :| [] <- poll $ do
        -- Do a Case Status request for getting case product to confirm ride
        -- on app side next
        statusResResult <- runClient appClientEnv (buildCaseStatusRes appCaseid)
        statusResResult `shouldSatisfy` isRight
        let Right statusRes = statusResResult
        return . nonEmpty $ productInstances statusRes
      let productInstanceId = _getProductInstanceId $ AppCase._id productInstance

      -- check if calculated price is greater than 0
      let prodPrice = productInstance ^. #_price
      prodPrice `shouldBe` 1372.47075

      -- Confirm ride from app backend
      confirmResult <-
        runClient
          appClientEnv
          (appConfirmRide appRegistrationToken $ buildAppConfirmReq appCaseid productInstanceId)
      confirmResult `shouldSatisfy` isRight

      transporterOrderPi :| [] <- poll $ do
        -- List all confirmed rides (type = RIDEORDER)
        rideReqResult <- runClient tbeClientEnv (buildOrgRideReq PI.CONFIRMED Case.RIDEORDER)
        rideReqResult `shouldSatisfy` isRight

        -- Filter order productInstance
        let Right rideListRes = rideReqResult
            tbePiList = TbePI._productInstance <$> rideListRes
            transporterOrdersPi = filter (\pI -> (_getProductInstanceId <$> PI._parentId pI) == Just productInstanceId) tbePiList
        return $ nonEmpty transporterOrdersPi
      let transporterOrderPiId = PI._id transporterOrderPi

      -- Assign Driver and Vehicle
      assignDriverVehicleResult <-
        runClient
          tbeClientEnv
          (rideUpdate appRegistrationToken transporterOrderPiId buildUpdatePIReq)
      assignDriverVehicleResult `shouldSatisfy` isRight

      -- Update RIDEORDER PI to INPROGRESS once driver starts his trip
      inProgressStatusResult <-
        runClient
          tbeClientEnv
          (rideUpdate appRegistrationToken transporterOrderPiId (buildUpdateStatusReq PI.INPROGRESS (transporterOrderPi ^. #_udf4)))
      inProgressStatusResult `shouldSatisfy` isRight

      inprogressPiListResult <- runClient appClientEnv (buildListPIs PI.INPROGRESS)
      inprogressPiListResult `shouldSatisfy` isRight

      -- Check if app RIDEORDER PI got updated to status INPROGRESS
      checkPiInResult inprogressPiListResult productInstanceId

      -- Update RIDEORDER PI to COMPLETED once driver ends his trip
      completedStatusResult <-
        runClient
          tbeClientEnv
          (rideUpdate appRegistrationToken transporterOrderPiId (buildUpdateStatusReq PI.COMPLETED Nothing))
      completedStatusResult `shouldSatisfy` isRight

      completedPiListResult <- runClient appClientEnv (buildListPIs PI.COMPLETED)
      completedPiListResult `shouldSatisfy` isRight

      -- Check if app RIDEORDER PI got updated to status COMPLETED
      checkPiInResult completedPiListResult productInstanceId
  where
    -- initiate exotel call (uncomment this for call tests)
    -- callTranId <- UUID.nextUUID
    -- cReq <- buildCallReq (UUID.toText $ fromJust callTranId) "4bf94783-cce4-4692-9a3d-605d279015ee"
    -- callResult <- runClient appClientEnv (appCallToProvider appRegistrationToken cReq)
    -- callResult `shouldSatisfy` isRight

    productInstances :: AppCase.StatusRes -> [AppCase.ProdInstRes]
    productInstances = AppCase._productInstance

    checkPiInResult :: Either ClientError [AppPI.ProductInstanceRes] -> Text -> Expectation
    checkPiInResult piListResult productInstanceId =
      let Right piListRes = piListResult
          appPiList = AppPI._productInstance <$> piListRes
          appOrderPI = filter (\pI -> (_getProductInstanceId <$> PI._parentId pI) == Just productInstanceId) appPiList
       in length appOrderPI `shouldBe` 1
