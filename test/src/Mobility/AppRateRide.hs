{-# LANGUAGE OverloadedLabels #-}

module Mobility.AppRateRide where

import Beckn.Types.App
  ( ProductInstanceId (_getProductInstanceId),
  )
import Beckn.Types.ID
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as PI
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
      let appCaseId = ID $ searchResponse ^. #message . #_message

      productInstance :| [] <- poll $ do
        statusResult <- runClient appClient $ F.buildCaseStatusRes (getId appCaseId)
        statusResult `shouldSatisfy` isRight
        let Right statusResponse = statusResult
        pure . nonEmpty . filter (\p -> p ^. #_organizationId == F.bppTransporterOrgId) $ statusResponse ^. #_productInstance
      let appProductInstanceId = productInstance ^. #_id
      confirmResult <-
        runClient appClient
          . F.appConfirmRide F.appRegistrationToken
          $ F.buildAppConfirmReq (getId appCaseId) (_getProductInstanceId appProductInstanceId)
      confirmResult `shouldSatisfy` isRight

      transporterOrder :| [] <- poll $ do
        rideRequestResponse <- runClient transporterClient $ F.buildOrgRideReq PI.CONFIRMED Case.RIDEORDER
        rideRequestResponse `shouldSatisfy` isRight
        let Right rideResponse = rideRequestResponse
        let orders =
              rideResponse ^.. traverse . #_productInstance
                & filter \pi -> pi ^. #_parentId == Just appProductInstanceId
        pure $ nonEmpty orders

      let transporterOrderId = transporterOrder ^. #_id
      rideInfo <- poll $ do
        res <-
          runClient
            transporterClient
            $ F.getNotificationInfo F.driverToken (Just transporterOrderId)
        pure $ either (const Nothing) (^. #rideRequest) res
      rideInfo ^. #productInstanceId `shouldBe` transporterOrderId

      -- Driver Accepts a ride
      let respondBody = RideAPI.SetDriverAcceptanceReq transporterOrderId RideAPI.ACCEPT
      driverAcceptRideRequestResult <-
        runClient
          transporterClient
          $ F.rideRespond F.driverToken respondBody
      driverAcceptRideRequestResult `shouldSatisfy` isRight

      tripAssignedPI :| [] <- poll $ do
        rideRequestRes <- runClient transporterClient $ F.buildOrgRideReq PI.TRIP_ASSIGNED Case.RIDEORDER
        rideRequestRes `shouldSatisfy` isRight
        let Right rideResponse = rideRequestRes
        let orders =
              rideResponse ^.. traverse . #_productInstance
                & filter \pi -> pi ^. #_parentId == Just appProductInstanceId
        return $ nonEmpty orders
      tripAssignedPI ^. #_status `shouldBe` PI.TRIP_ASSIGNED

      inProgressStatusResult <-
        runClient
          transporterClient
          $ F.rideUpdate F.appRegistrationToken transporterOrderId (F.buildUpdateStatusReq PI.INPROGRESS (transporterOrder ^. #_udf4))
      inProgressStatusResult `shouldSatisfy` isRight

      completeStatusResult <-
        runClient
          transporterClient
          $ F.rideUpdate F.appRegistrationToken transporterOrderId (F.buildUpdateStatusReq PI.COMPLETED Nothing)
      completeStatusResult `shouldSatisfy` isRight

      appPiListResult <- runClient appClient $ F.buildListPIs PI.COMPLETED
      appPiListResult `shouldSatisfy` isRight

      appFeedbackResult <-
        runClient appClient $
          F.callAppFeedback 5 appProductInstanceId appCaseId
      appFeedbackResult `shouldSatisfy` isRight
      let Right appFeedbackResponse = appFeedbackResult
      appFeedbackResponse ^. #_error `shouldSatisfy` isNothing

      driverInfoResult <-
        runClient transporterClient $ F.callGetPerson (ID F.testDriverId)
      driverInfoResult `shouldSatisfy` isRight
      let Right driverInfoResponse = driverInfoResult
      let driverRating = driverInfoResponse ^. #_rating
      driverRating `shouldSatisfy` isJust
