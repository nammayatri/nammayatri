{-# LANGUAGE OverloadedLabels #-}

module Mobility.AppRateRide where

import Beckn.Types.App
  ( CaseId (CaseId, _getCaseId),
    ProductInstanceId (ProductInstanceId, _getProductInstanceId),
  )
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Data.Text as Text
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
import Test.Hspec (Spec, describe, it, runIO, shouldSatisfy)
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
      let appCaseId = searchResponse ^. #message . #_message

      theCase :| [] <- poll $ do
        caseRequestResult <- runClient transporterClient F.buildListLeads
        caseRequestResult `shouldSatisfy` isRight
        let Right caseListResponse = caseRequestResult
        let caseList =
              caseListResponse
                & filter \response -> appCaseId `elem` Text.splitOn "_" (response ^. #_case . #_shortId)
        pure $ nonEmpty caseList

      let transporterCaseId = _getCaseId $ theCase ^. #_case . #_id
      acceptedRideResult <-
        runClient transporterClient $
          F.acceptOrDeclineRide F.appRegistrationToken transporterCaseId F.buildUpdateCaseReq
      acceptedRideResult `shouldSatisfy` isRight

      productInstance :| [] <- poll $ do
        statusResult <- runClient appClient $ F.buildCaseStatusRes appCaseId
        statusResult `shouldSatisfy` isRight
        let Right statusResponse = statusResult
        pure . nonEmpty $ statusResponse ^. #_productInstance

      let productInstanceId = _getProductInstanceId $ productInstance ^. #_id
      confirmResult <- runClient appClient $ F.appConfirmRide F.appRegistrationToken (F.buildAppConfirmReq appCaseId productInstanceId)
      confirmResult `shouldSatisfy` isRight

      -- assign driver
      transporterOrder :| [] <- poll $ do
        rideRequestResponse <- runClient transporterClient $ F.buildOrgRideReq PI.CONFIRMED Case.RIDEORDER
        rideRequestResponse `shouldSatisfy` isRight
        let Right rideResponse = rideRequestResponse
        let orders =
              rideResponse ^.. traverse . #_productInstance
                & filter
                  \pi -> pi ^. #_parentId == Just (ProductInstanceId productInstanceId)
        pure $ nonEmpty orders

      let transporterOrderId = transporterOrder ^. #_id
      assignDriverResult <-
        runClient
          transporterClient
          $ F.rideUpdate F.appRegistrationToken transporterOrderId F.buildUpdatePIReq
      assignDriverResult `shouldSatisfy` isRight

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
          F.callAppFeedback 5 (ProductInstanceId productInstanceId) (CaseId appCaseId)
      appFeedbackResult `shouldSatisfy` isRight
