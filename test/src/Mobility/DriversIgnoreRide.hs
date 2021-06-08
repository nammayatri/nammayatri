{-# LANGUAGE OverloadedLabels #-}

module Mobility.DriversIgnoreRide where

import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import EulerHS.Prelude
import Mobility.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import qualified "app-backend" Types.API.Case as AppCase
import qualified "app-backend" Types.API.ProductInstance as AppPI
import qualified "beckn-transport" Types.API.ProductInstance as TbePI
import qualified "beckn-transport" Types.API.Ride as RideAPI
import Utils

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl = getAppBaseUrl
      transporterBaseUrl = getTransporterBaseUrl
      appClientEnv = mkClientEnv appManager appBaseUrl
      tbeClientEnv = mkClientEnv appManager transporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by Driver" do
      -- Driver sets online
      setDriverOnlineResponse <-
        runClient tbeClientEnv $ setDriverOnline appRegistrationToken True
      setDriverOnlineResponse `shouldSatisfy` isRight

      -- Do an App Search
      transactionId <- UUID.nextUUID
      sreq <- buildSearchReq $ UUID.toText $ fromJust transactionId
      searchResult <- runClient appClientEnv (searchServices appRegistrationToken sreq)
      searchResult `shouldSatisfy` isRight
      -- If we reach here, the 'Right' pattern match will always succeed
      let Right searchResponse = searchResult
          appCaseid = searchResponse ^. #caseId

      productInstance :| [] <- poll $ do
        -- Do a Case Status request for getting case product to confirm ride
        -- on app side next
        statusResResult <- runClient appClientEnv (buildCaseStatusRes appCaseid)
        statusResResult `shouldSatisfy` isRight
        let Right statusRes = statusResResult
        return . nonEmpty . filter (\p -> p ^. #organizationId == Id bppTransporterOrgId) $ productInstances statusRes
      let productInstanceId = getId $ AppCase.id productInstance
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
            tbePiList = TbePI.productInstance <$> rideListRes
            transporterOrdersPi = filter (\pI -> (getId <$> PI.parentId pI) == Just productInstanceId) tbePiList
        return $ nonEmpty transporterOrdersPi
      let transporterOrderPiId = PI.id transporterOrderPi

      -- Driver Rejects a ride
      let respondBody = RideAPI.SetDriverAcceptanceReq transporterOrderPiId RideAPI.REJECT
      driverAcceptRideRequestResult <-
        runClient
          tbeClientEnv
          $ rideRespond driverToken respondBody
      driverAcceptRideRequestResult `shouldSatisfy` isRight

      piListResult <- runClient appClientEnv (buildListPIs PI.CANCELLED)
      piListResult `shouldSatisfy` isRight

      -- Check if app RIDEORDER PI is not CANCELLED. Only Customer can cancel the order.
      checkPiInResult piListResult productInstanceId
  where
    productInstances :: AppCase.GetStatusRes -> [AppCase.ProdInstRes]
    productInstances = AppCase.productInstance

    checkPiInResult :: Either ClientError [AppPI.ProductInstanceRes] -> Text -> Expectation
    checkPiInResult piListResult productInstanceId =
      let Right piListRes = piListResult
          appPiList = AppPI.productInstance <$> piListRes
          appOrderPI = filter (\pI -> (getId <$> PI.parentId pI) == Just productInstanceId) appPiList
       in length appOrderPI `shouldBe` 0
