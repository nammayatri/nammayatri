{-# LANGUAGE OverloadedLabels #-}

module Mobility.AppCancelRide where

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
import qualified Types.API.Cancel as CancelAPI
import qualified "app-backend" Types.API.Case as AppCase
import qualified "app-backend" Types.API.Common as AppCommon
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
    it "Testing API flow for ride cancelled by App" do
      -- Do an App Search
      transactionId <- UUID.nextUUID
      sreq <- buildSearchReq $ UUID.toText $ fromJust transactionId
      ackResult <- runClient appClientEnv (searchServices appRegistrationToken sreq)
      ackResult `shouldSatisfy` isRight
      -- If we reach here, the 'Right' pattern match will always succeed
      let Right ackResponse = ackResult
          appCaseid = AppCommon._message $ ackResponse ^. #message

      productInstance :| [] <- poll $ do
        -- Do a Case Status request for getting case product to confirm ride
        -- on app side next
        statusResResult <- runClient appClientEnv (buildCaseStatusRes appCaseid)
        statusResResult `shouldSatisfy` isRight
        let Right statusRes = statusResResult
        return . nonEmpty . filter (\p -> p ^. #_organizationId == bppTransporterOrgId) $ productInstances statusRes
      let productInstanceId = getId $ AppCase._id productInstance
      -- Confirm ride from app backend
      confirmResult <-
        runClient
          appClientEnv
          (appConfirmRide appRegistrationToken $ buildAppConfirmReq appCaseid productInstanceId)
      confirmResult `shouldSatisfy` isRight

      -- cancel request initiated by App
      txnId <- UUID.nextUUID
      cancelResult <-
        runClient
          appClientEnv
          ( cancelRide appRegistrationToken $
              buildAppCancelReq (UUID.toText $ fromJust txnId) productInstanceId CancelAPI.PRODUCT_INSTANCE
          )
      cancelResult `shouldSatisfy` isRight

      -- List all cancelled rides (type = RIDEORDER)
      orderPI :| [] <- poll $ do
        res <- runClient tbeClientEnv (buildOrgRideReq PI.CANCELLED Case.RIDEORDER)
        let Right piListRes = res
        let tbePiList = TbePI._productInstance <$> piListRes
        let tbeOrderPI = filter (\pI -> (getId <$> PI._parentId pI) == Just productInstanceId) tbePiList
        pure $ nonEmpty tbeOrderPI
      orderPI ^. #_status `shouldBe` PI.CANCELLED
  where
    productInstances :: AppCase.StatusRes -> [AppCase.ProdInstRes]
    productInstances = AppCase._productInstance
