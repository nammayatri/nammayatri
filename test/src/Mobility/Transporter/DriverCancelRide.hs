module Mobility.Transporter.DriverCancelRide where

import Common (getAppBaseUrl)
import qualified "app-backend" Domain.Types.Booking as AppRB
import qualified "beckn-transport" Domain.Types.Booking as TRB
import qualified "beckn-transport" Domain.Types.CancellationReason as SCR
import qualified "beckn-transport" Domain.Types.Ride as TRide
import EulerHS.Prelude
import HSpec
import Mobility.AppBackend.APICalls
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Transporter
import Mobility.Transporter.SuccessFlow
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by Driver" $ withBecknClients clients do
      void . callBPP $ setDriverOnline driverToken2 True
      bBookingId <- doAnAppSearch

      tBooking <- poll $ do
        trb <- getBPPBooking bBookingId
        trb.status `shouldBe` TRB.CONFIRMED
        return $ Just trb

      rideInfo <-
        poll . callBPP $
          getNotificationInfo tBooking.id driverToken1
            <&> (.rideRequest)
      rideInfo.bookingId `shouldBe` tBooking.id

      -- Driver1 Accepts a ride
      void . callBPP $
-- <<<<<<< HEAD:test/src/Mobility/DriverCancelRide.hs
        rideRespond tBooking.id driverToken1 $
          TbeBookingAPI.SetDriverAcceptanceReq TbeBookingAPI.ACCEPT
-- =======
--        rideRespond tRideBooking.id driverToken1 $
--          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.ACCEPT
-- >>>>>>> slight refactoring of tests structure, readable local testing data for ARDU flow, healthcheck test for ARDU:test/src/Mobility/Transporter/DriverCancelRide.hs

      tRide1 <- poll $ do
        tRide <- getBPPRide tBooking.id
        tRide.status `shouldBe` TRide.NEW
        return $ Just tRide

      void . callBPP $
        rideCancel driverToken1 tRide1.id $
          RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

      void . poll $
        callBAP (appBookingStatus bBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.AWAITING_REASSIGNMENT)
          <&> Just

      rideInfo2 <-
        poll . callBPP $
          getNotificationInfo tBooking.id driverToken2
            <&> (.rideRequest)
      rideInfo2.bookingId `shouldBe` tBooking.id

      -- Driver2 Accepts a ride
      void . callBPP $
-- <<<<<<< HEAD:test/src/Mobility/DriverCancelRide.hs
        rideRespond tBooking.id driverToken2 $
          TbeBookingAPI.SetDriverAcceptanceReq TbeBookingAPI.ACCEPT
-- =======
--        rideRespond tRideBooking.id driverToken2 $
--          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.ACCEPT
-- >>>>>>> slight refactoring of tests structure, readable local testing data for ARDU flow, healthcheck test for ARDU:test/src/Mobility/Transporter/DriverCancelRide.hs

      tRide2 <- poll $ do
        tRide <- getBPPRide tBooking.id
        tRide.status `shouldBe` TRide.NEW
        return $ Just tRide

      void . callBPP $
        rideCancel driverToken2 tRide2.id $
          RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

      void . poll $
        callBAP (appBookingStatus bBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just

      void . callBPP $ setDriverOnline driverToken1 False
      void . callBPP $ setDriverOnline driverToken2 False
