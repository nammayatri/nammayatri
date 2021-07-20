module Mobility.DriversIgnoreRide where

import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
import qualified "app-backend" Types.Storage.RideBooking as AppRB
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by Driver" $ withBecknClients clients do
      (_, bRideBookingId, tRide) <- doAnAppSearch

      -- Driver Rejects a ride
      void . callBPP $
        rideRespond tRide.id driverToken $
          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.REJECT

      void . poll $
        callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just