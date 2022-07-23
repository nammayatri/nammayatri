module Mobility.Transporter.DriversRejectRide where

import Common (getAppBaseUrl)
import qualified "app-backend" Domain.Types.Booking as AppRB
import qualified "beckn-transport" Domain.Types.Booking as TRB
import EulerHS.Prelude
import HSpec
import Mobility.AppBackend.APICalls
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Transporter
import Mobility.Transporter.SuccessFlow
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride rejected by Driver" $ withBecknClients clients do
      bBookingId <- doAnAppSearch

      tBooking <- poll $ do
        trb <- getBPPBooking bBookingId
        trb.status `shouldBe` TRB.CONFIRMED
        return $ Just trb

      -- Driver Rejects a ride
      void . callBPP $
-- <<<<<<< HEAD:test/src/Mobility/DriversRejectRide.hs
        rideRespond tBooking.id driverToken1 $
          TbeBookingAPI.SetDriverAcceptanceReq TbeBookingAPI.REJECT
-- =======
--        rideRespond tRideBooking.id driverToken1 $
--          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.REJECT
-- >>>>>>> slight refactoring of tests structure, readable local testing data for ARDU flow, healthcheck test for ARDU:test/src/Mobility/Transporter/DriversRejectRide.hs

      void . poll $
        callBAP (appBookingStatus bBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just

      void . callBPP $ setDriverOnline driverToken1 False
