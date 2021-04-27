{-# LANGUAGE OverloadedLabels #-}

module Mobility.SuccessFlow where

import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as PI
-- import Data.Text (isSuffixOf)
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import EulerHS.Prelude
import Mobility.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import qualified "app-backend" Types.API.Case as AppCase
-- import qualified "beckn-transport" Types.API.Case as TbeCase
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
    it "Testing API flow for successful booking and completion of ride" do
      -- Driver sets online
      setDriverOnlineResponse <-
        runClient tbeClientEnv $ setDriverOnline appRegistrationToken True
      setDriverOnlineResponse `shouldSatisfy` isRight

      -- Do an App Search
      transactionId <- UUID.nextUUID
      sreq <- buildSearchReq $ UUID.toText $ fromJust transactionId
      ackResult <- runClient appClientEnv (searchServices appRegistrationToken sreq)
      ackResult `shouldSatisfy` isRight
      -- If we reach here, the 'Right' pattern match will always succeed
      let Right ackResponse = ackResult
          appCaseid = ackResponse ^. #caseId

      -- All rides are accepted by default and has fare calculated

      productInstance :| [] <- poll $ do
        -- Do a Case Status request for getting case product to confirm ride
        -- on app side next
        statusResResult <- runClient appClientEnv (buildCaseStatusRes appCaseid)
        statusResResult `shouldSatisfy` isRight
        let Right statusRes = statusResResult
        -- since all BPP can give quote for now we filter by orgId
        return $ nonEmpty . filter (\p -> p ^. #_organizationId == bppTransporterOrgId) $ productInstances statusRes
      let productInstanceId = getId $ AppCase._id productInstance

      -- check if calculated price is greater than 0
      let prodPrice = productInstance ^. #_price
      prodPrice `shouldSatisfy` (> 100) -- should at least be more than 100

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
            transporterOrdersPi = filter (\pI -> (getId <$> PI._parentId pI) == Just productInstanceId) tbePiList
        return $ nonEmpty transporterOrdersPi
      let transporterOrderPiId = PI._id transporterOrderPi

      rideInfo <- poll $ do
        res <-
          runClient
            tbeClientEnv
            $ getNotificationInfo driverToken (Just $ cast transporterOrderPiId)
        pure $ either (const Nothing) (^. #rideRequest) res
      rideInfo ^. #productInstanceId `shouldBe` transporterOrderPiId

      -- Driver Accepts a ride
      let respondBody = RideAPI.SetDriverAcceptanceReq transporterOrderPiId RideAPI.ACCEPT
      driverAcceptRideRequestResult <-
        runClient
          tbeClientEnv
          $ rideRespond driverToken respondBody
      driverAcceptRideRequestResult `shouldSatisfy` isRight

      tripAssignedPI :| [] <- poll $ do
        -- List all confirmed rides (type = RIDEORDER)
        rideReqRes <- runClient tbeClientEnv (buildOrgRideReq PI.TRIP_ASSIGNED Case.RIDEORDER)
        rideReqRes `shouldSatisfy` isRight

        -- Filter order productInstance
        let Right rideListRes = rideReqRes
            tbePiList = TbePI._productInstance <$> rideListRes
            transporterOrdersPi = filter (\pI -> (getId <$> PI._parentId pI) == Just productInstanceId) tbePiList
        return $ nonEmpty transporterOrdersPi
      tripAssignedPI ^. #_status `shouldBe` PI.TRIP_ASSIGNED

      -- Update RIDEORDER PI to INPROGRESS once driver starts his trip
      inProgressStatusResult <-
        runClient
          tbeClientEnv
          (rideStart driverToken transporterOrderPiId (buildStartRideReq . fromMaybe "OTP is not present" $ transporterOrderPi ^. #_udf4))
      inProgressStatusResult `shouldSatisfy` isRight

      inprogressPiListResult <- runClient appClientEnv (buildListPIs PI.INPROGRESS)
      inprogressPiListResult `shouldSatisfy` isRight

      -- Check if app RIDEORDER PI got updated to status INPROGRESS
      checkPiInResult inprogressPiListResult productInstanceId

      -- Update RIDEORDER PI to COMPLETED once driver ends his trip
      completedStatusResult <-
        runClient
          tbeClientEnv
          (rideEnd driverToken transporterOrderPiId)
      completedStatusResult `shouldSatisfy` isRight
  where
    productInstances :: AppCase.StatusRes -> [AppCase.ProdInstRes]
    productInstances = AppCase._productInstance

    checkPiInResult :: Either ClientError [AppPI.ProductInstanceRes] -> Text -> Expectation
    checkPiInResult piListResult productInstanceId =
      let Right piListRes = piListResult
          appPiList = AppPI._productInstance <$> piListRes
          appOrderPI = filter (\pI -> (getId <$> PI._parentId pI) == Just productInstanceId) appPiList
       in length appOrderPI `shouldBe` 1
