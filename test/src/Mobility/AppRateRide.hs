{-# LANGUAGE OverloadedLabels #-}

module Mobility.AppRateRide where

import Beckn.Types.Id
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import EulerHS.Prelude hiding (pi)
import qualified Mobility.Fixtures as F
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
  ( BaseUrl
      ( BaseUrl,
        baseUrlHost,
        baseUrlPath,
        baseUrlPort,
        baseUrlScheme
      ),
    Scheme (Http),
    mkClientEnv,
  )
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldSatisfy)
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "beckn-transport" Types.Storage.Case as TCase
import qualified "app-backend" Types.Storage.ProductInstance as BPI
import qualified "beckn-transport" Types.Storage.ProductInstance as TPI
import Utils (poll, runClient)

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl =
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = "localhost",
            baseUrlPort = 8013,
            baseUrlPath = "/v1"
          }
  let transporterBaseUrl = appBaseUrl & #baseUrlPort .~ 8014
  let appClient = mkClientEnv appManager appBaseUrl
  let transporterClient = mkClientEnv appManager transporterBaseUrl
  describe "Testing App Feedback APIs" $
    it "Should send feedback for completed order" $ do
      txnId <- UUID.nextUUID
      searchRequest <- F.buildSearchReq . UUID.toText . fromJust $ txnId
      searchACK <- runClient appClient $ F.searchServices F.appRegistrationToken searchRequest
      searchACK `shouldSatisfy` isRight

      let Right searchResponse = searchACK
      let appCaseId = Id $ searchResponse.caseId

      productInstance :| [] <- poll $ do
        statusResult <- runClient appClient $ F.buildCaseStatusRes (getId appCaseId)
        statusResult `shouldSatisfy` isRight
        let Right statusResponse = statusResult
        pure . nonEmpty . filter (\p -> p.organizationId == Id F.bppTransporterOrgId) $ statusResponse.productInstance
      let appProductInstanceId = cast productInstance.id
      confirmResult <-
        runClient appClient
          . F.appConfirmRide F.appRegistrationToken
          $ F.buildAppConfirmReq (getId appCaseId) (getId appProductInstanceId)
      confirmResult `shouldSatisfy` isRight

      transporterOrder :| [] <- poll $ do
        rideRequestResponse <- runClient transporterClient $ F.buildOrgRideReq TPI.CONFIRMED TCase.RIDEORDER
        rideRequestResponse `shouldSatisfy` isRight
        let Right rideResponse = rideRequestResponse
        let orders =
              rideResponse ^.. traverse . #productInstance
                & filter \pi -> pi.parentId == Just appProductInstanceId
        pure $ nonEmpty orders

      let transporterOrderId = transporterOrder.id
      rideInfo <- poll $ do
        res <-
          runClient
            transporterClient
            $ F.getNotificationInfo F.driverToken (Just $ cast transporterOrderId)
        pure $ either (const Nothing) (.rideRequest) res
      rideInfo.productInstanceId `shouldBe` transporterOrderId

      -- Driver Accepts a ride
      let respondBody = RideAPI.SetDriverAcceptanceReq transporterOrderId RideAPI.ACCEPT
      driverAcceptRideRequestResult <-
        runClient
          transporterClient
          $ F.rideRespond F.driverToken respondBody
      driverAcceptRideRequestResult `shouldSatisfy` isRight

      tripAssignedPI :| [] <- poll $ do
        rideRequestRes <- runClient transporterClient $ F.buildOrgRideReq TPI.TRIP_ASSIGNED TCase.RIDEORDER
        rideRequestRes `shouldSatisfy` isRight
        let Right rideResponse = rideRequestRes
        let orders =
              rideResponse ^.. traverse . #productInstance
                & filter \pi -> pi.parentId == Just appProductInstanceId
        return $ nonEmpty orders
      tripAssignedPI.status `shouldBe` TPI.TRIP_ASSIGNED

      -- Update RIDEORDER PI to INPROGRESS once driver starts his trip
      inProgressStatusResult <-
        runClient
          transporterClient
          $ F.rideStart F.driverToken transporterOrderId (F.buildStartRideReq . fromMaybe "OTP is not present" $ transporterOrder.udf4)
      inProgressStatusResult `shouldSatisfy` isRight

      inprogressPiListResult <- runClient appClient (F.buildListPIs BPI.INPROGRESS)
      inprogressPiListResult `shouldSatisfy` isRight

      completeStatusResult <-
        runClient
          transporterClient
          $ F.rideEnd F.driverToken transporterOrderId
      completeStatusResult `shouldSatisfy` isRight

      appPiListResult <- runClient appClient $ F.buildListPIs BPI.COMPLETED
      appPiListResult `shouldSatisfy` isRight

      appFeedbackResult <-
        runClient appClient $
          F.callAppFeedback 5 appProductInstanceId appCaseId
      appFeedbackResult `shouldSatisfy` isRight

      driverInfoResult <-
        runClient transporterClient $ F.getDriverInfo F.driverToken
      (driverInfoResult >> return ("" :: Text)) `shouldSatisfy` isRight
      let Right driverInfoResponse = driverInfoResult
      let driverRating = driverInfoResponse.person.rating
      driverRating `shouldSatisfy` isJust
