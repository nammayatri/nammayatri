module Mobility.SuccessFlow where

import Beckn.Types.Id
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
import qualified "beckn-transport" Types.Storage.Case as TCase
import qualified "app-backend" Types.Storage.ProductInstance as BPI
import qualified "beckn-transport" Types.Storage.ProductInstance as TPI
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
      searchResult <- runClient appClientEnv (searchServices appRegistrationToken sreq)
      searchResult `shouldSatisfy` isRight
      -- If we reach here, the 'Right' pattern match will always succeed
      let Right searchResponse = searchResult
          appCaseid = searchResponse.caseId

      -- All rides are accepted by default and has fare calculated

      productInstance :| [] <- poll $ do
        -- Do a Case Status request for getting case product to confirm ride
        -- on app side next
        statusResResult <- runClient appClientEnv (buildCaseStatusRes appCaseid)
        statusResResult `shouldSatisfy` isRight
        let Right statusRes = statusResResult
        -- since all BPP can give quote for now we filter by orgId
        return $ nonEmpty . filter (\p -> p.organizationId == Id bppTransporterOrgId) $ productInstances statusRes
      let productInstanceId = getId $ AppCase.id productInstance

      -- check if calculated price is greater than 0
      let (Just prodPrice) = productInstance.price
      prodPrice `shouldSatisfy` (> 100) -- should at least be more than 100

      -- Confirm ride from app backend
      confirmResult <-
        runClient
          appClientEnv
          (appConfirmRide appRegistrationToken $ buildAppConfirmReq appCaseid productInstanceId)
      confirmResult `shouldSatisfy` isRight

      transporterOrderPi :| [] <- poll $ do
        -- List all confirmed rides (type = RIDEORDER)
        rideReqResult <- runClient tbeClientEnv (buildOrgRideReq TPI.CONFIRMED TCase.RIDEORDER)
        rideReqResult `shouldSatisfy` isRight

        -- Filter order productInstance
        let Right rideListRes = rideReqResult
            tbePiList = TbePI.productInstance <$> rideListRes
            transporterOrdersPi = filter (\pI -> (getId <$> TPI.parentId pI) == Just productInstanceId) tbePiList
        return $ nonEmpty transporterOrdersPi
      let transporterOrderPiId = TPI.id transporterOrderPi

      rideInfo <- poll $ do
        res <-
          runClient
            tbeClientEnv
            $ getNotificationInfo driverToken (Just $ cast transporterOrderPiId)
        pure $ either (const Nothing) (.rideRequest) res
      rideInfo.productInstanceId `shouldBe` transporterOrderPiId

      -- Driver Accepts a ride
      let respondBody = RideAPI.SetDriverAcceptanceReq transporterOrderPiId RideAPI.ACCEPT
      driverAcceptRideRequestResult <-
        runClient
          tbeClientEnv
          $ rideRespond driverToken respondBody
      driverAcceptRideRequestResult `shouldSatisfy` isRight

      tripAssignedPI :| [] <- poll $ do
        -- List all confirmed rides (type = RIDEORDER)
        rideReqRes <- runClient tbeClientEnv (buildOrgRideReq TPI.TRIP_ASSIGNED TCase.RIDEORDER)
        rideReqRes `shouldSatisfy` isRight

        -- Filter order productInstance
        let Right rideListRes = rideReqRes
            tbePiList = TbePI.productInstance <$> rideListRes
            transporterOrdersPi = filter (\pI -> (getId <$> TPI.parentId pI) == Just productInstanceId) tbePiList
        return $ nonEmpty transporterOrdersPi
      tripAssignedPI.status `shouldBe` TPI.TRIP_ASSIGNED

      -- Update RIDEORDER PI to INPROGRESS once driver starts his trip
      inProgressStatusResult <-
        runClient
          tbeClientEnv
          (rideStart driverToken transporterOrderPiId (buildStartRideReq . fromMaybe "OTP is not present" $ transporterOrderPi.udf4))
      inProgressStatusResult `shouldSatisfy` isRight

      inprogressPiListResult <- runClient appClientEnv (buildListPIs BPI.INPROGRESS)
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
    productInstances :: AppCase.GetStatusRes -> [AppCase.ProdInstRes]
    productInstances = AppCase.productInstance

    checkPiInResult :: Either ClientError [AppPI.ProductInstanceRes] -> Text -> Expectation
    checkPiInResult piListResult productInstanceId =
      let Right piListRes = piListResult
          appPiList = AppPI.productInstance <$> piListRes
          appOrderPI = filter (\pI -> (getId <$> BPI.parentId pI) == Just productInstanceId) appPiList
       in length appOrderPI `shouldBe` 1
