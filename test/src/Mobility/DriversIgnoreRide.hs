module Mobility.DriversIgnoreRide where

import Beckn.Types.Id
import Control.Arrow ((>>>))
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "app-backend" Types.Storage.ProductInstance as BPI
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by Driver" $ withBecknClients clients do
      (productInstanceId, transporterOrderPi) <- doAnAppSearch
      let transporterOrderPiId = transporterOrderPi.id

      -- Driver Rejects a ride
      void . callBPP $
        rideRespond driverToken $
          RideAPI.SetDriverAcceptanceReq transporterOrderPiId RideAPI.REJECT

      -- Only Customer can cancel the order.
      void $
        callBAP (buildListPIs BPI.CANCELLED)
          <&> map (.productInstance)
          <&> filter (\pI -> pI.parentId == Just (Id productInstanceId))
          >>= (length >>> (`shouldBe` 0))
