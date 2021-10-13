module Mobility.AppCancelRide where

import Beckn.Types.Id
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
import qualified Types.API.Cancel as CancelAPI
import qualified "beckn-transport" Types.Storage.Case as Case
import qualified "beckn-transport" Types.Storage.ProductInstance as PI
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by App" $ withBecknClients clients do
      (productInstanceId, _) <- doAnAppSearch
      void $
        callBAP
          ( cancelRide appRegistrationToken $
              buildAppCancelReq productInstanceId CancelAPI.PRODUCT_INSTANCE
          )
      orderPI <- pollBPPForOrgOrderPi (Id productInstanceId) PI.CANCELLED Case.RIDEORDER
      orderPI.status `shouldBe` PI.CANCELLED
