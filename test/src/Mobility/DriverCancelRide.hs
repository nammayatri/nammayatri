module Mobility.DriverCancelRide where

import Beckn.Types.Id
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "beckn-transport" Types.Storage.CancellationReason as SCR
import qualified "app-backend" Types.Storage.ProductInstance as BPI
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by Driver" $ withBecknClients clients do
      (productInstanceId, transporterOrderPi) <- doAnAppSearch
      let transporterOrderPiId = transporterOrderPi.id

      -- Driver Accepts a ride
      void . callBPP $
        rideRespond driverToken $
          RideAPI.SetDriverAcceptanceReq transporterOrderPiId RideAPI.ACCEPT

      void . callBPP $
        rideCancel appRegistrationToken transporterOrderPiId $
          RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

      piCancelled <-
        expectSingletonNE <=< poll $ do
          callBAP (buildListPIs BPI.CANCELLED)
            <&> map (.productInstance)
            <&> filter (\pI -> pI.parentId == Just (Id productInstanceId))
            <&> nonEmpty

      piCancelled.status `shouldBe` BPI.CANCELLED
