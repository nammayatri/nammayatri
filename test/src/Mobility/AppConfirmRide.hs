{-# LANGUAGE OverloadedLabels #-}

module Mobility.AppConfirmRide where

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
import Utils

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
      -- transporterBaseUrl = appBaseUrl {baseUrlPort = 8014}
      appClientEnv = mkClientEnv appManager appBaseUrl
  -- tbeClientEnv = mkClientEnv appManager transporterBaseUrl
  describe "Testing App Backend APIs" $
    it "Testing API flow for App confirm ride" $
      hspec $
        it "API flow should succeed for Transporter -> Accepts & App consumer -> Confirms" do
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
            return . nonEmpty . filter (\p -> p ^. #organizationId == bppTransporterOrgId) $ productInstances statusRes
          let productInstanceId = getId $ AppCase.id productInstance
          -- Confirm ride from app backend
          confirmResult <- runClient appClientEnv (appConfirmRide appRegistrationToken $ buildAppConfirmReq appCaseid productInstanceId)
          confirmResult `shouldSatisfy` isRight
  where
    -- initiate exotel call (uncomment this for call tests)
    -- callTranId <- UUID.nextUUID
    -- cReq <- buildCallReq (UUID.toText $ fromJust callTranId) "4bf94783-cce4-4692-9a3d-605d279015ee"
    -- callResult <- runClient appClientEnv (appCallToProvider appRegistrationToken cReq)
    -- callResult `shouldSatisfy` isRight

    productInstances :: AppCase.GetStatusRes -> [AppCase.ProdInstRes]
    productInstances = AppCase.productInstance
