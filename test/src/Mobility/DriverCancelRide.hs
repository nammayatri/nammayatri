{-# LANGUAGE OverloadedLabels #-}

module Mobility.DriverCancelRide where

import Beckn.Types.Id
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
import qualified "beckn-transport" Types.Storage.CancellationReason as SCR
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
    it "Testing API flow for ride cancelled by Driver" do
      -- Do an App Search
      transactionId <- UUID.nextUUID
      sreq <- buildSearchReq $ UUID.toText $ fromJust transactionId
      searchResult <- runClient appClientEnv (searchServices appRegistrationToken sreq)
      searchResult `shouldSatisfy` isRight
      -- If we reach here, the 'Right' pattern match will always succeed
      let Right searchResponse = searchResult
          appCaseid = searchResponse.caseId

      productInstance :| [] <- poll $ do
        -- Do a Case Status request for getting case product to confirm ride
        -- on app side next
        statusResResult <- runClient appClientEnv (buildCaseStatusRes appCaseid)
        statusResResult `shouldSatisfy` isRight
        let Right statusRes = statusResResult
        return . nonEmpty . filter (\p -> p.organizationId == Id bppTransporterOrgId) $ productInstances statusRes
      let productInstanceId = AppCase.id productInstance
      -- Confirm ride from app backend
      confirmResult <-
        runClient
          appClientEnv
          (appConfirmRide appRegistrationToken $ buildAppConfirmReq appCaseid $ getId productInstanceId)
      confirmResult `shouldSatisfy` isRight

      transporterOrderPi :| [] <- poll $ do
        -- List all confirmed rides (type = RIDEORDER)
        rideReqResult <- runClient tbeClientEnv (buildOrgRideReq TPI.CONFIRMED TCase.RIDEORDER)
        rideReqResult `shouldSatisfy` isRight

        -- Filter order productInstance
        let Right rideListRes = rideReqResult
            tbePiList = TbePI.productInstance <$> rideListRes
            transporterOrdersPi = filter (\pI -> (getId <$> TPI.parentId pI) == Just (getId productInstanceId)) tbePiList
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
        rideRequestResponse <- runClient tbeClientEnv $ buildOrgRideReq TPI.TRIP_ASSIGNED TCase.RIDEORDER
        rideRequestResponse `shouldSatisfy` isRight
        let Right rideResponse = rideRequestResponse
        let orders =
              rideResponse ^.. traverse . #productInstance
                & filter \p -> p.parentId == Just (cast productInstanceId)
        return $ nonEmpty orders
      tripAssignedPI.status `shouldBe` TPI.TRIP_ASSIGNED

      -- Driver updates RIDEORDER PI to TRIP_REASSIGNMENT
      let cancelRideReq = RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing
      cancelStatusResult <-
        runClient
          tbeClientEnv
          (rideCancel appRegistrationToken transporterOrderPiId cancelRideReq)
      cancelStatusResult `shouldSatisfy` isRight

      piCancelled :| [] <- poll $ do
        res <- runClient appClientEnv (buildListPIs BPI.CANCELLED)
        let Right piListRes = res
        let appPiList = AppPI.productInstance <$> piListRes
        let appOrderPI = filter (\pI -> (getId <$> BPI.parentId pI) == Just (getId productInstanceId)) appPiList
        pure $ nonEmpty appOrderPI
      piCancelled.status `shouldBe` BPI.CANCELLED
  where
    productInstances :: AppCase.GetStatusRes -> [AppCase.ProdInstRes]
    productInstances = AppCase.productInstance
