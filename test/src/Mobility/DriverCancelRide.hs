module Mobility.DriverCancelRide where

import Common (getAppBaseUrl)
import qualified "beckn-transport" Domain.Types.CancellationReason as SCR
import qualified "beckn-transport" Domain.Types.Ride as TRide
import qualified "app-backend" Domain.Types.RideBooking as AppRB
import qualified "beckn-transport" Domain.Types.RideBooking as TRB
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by Driver" $ withBecknClients clients do
      void . callBPP $ setDriverOnline driverToken2 True
      (bapQuoteId, bRideBookingId) <- doAnAppSearch

      tRideBooking <- poll $ do
        tQuoteId <- getBPPQuoteId bapQuoteId
        trb <- getBPPRideBooking tQuoteId
        trb.status `shouldBe` TRB.CONFIRMED
        return $ Just trb

      rideInfo <-
        poll . callBPP $
          getNotificationInfo tRideBooking.id driverToken1
            <&> (.rideRequest)
      rideInfo.bookingId `shouldBe` tRideBooking.id

      -- Driver1 Accepts a ride
      void . callBPP $
        rideRespond tRideBooking.id driverToken1 $
          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.ACCEPT

      tRide1 <- poll $ do
        tRide <- getBPPRide tRideBooking.id
        tRide.status `shouldBe` TRide.NEW
        return $ Just tRide

      void . callBPP $
        rideCancel driverToken1 tRide1.id $
          RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

      void . poll $
        callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.AWAITING_REASSIGNMENT)
          <&> Just

      rideInfo2 <-
        poll . callBPP $
          getNotificationInfo tRideBooking.id driverToken2
            <&> (.rideRequest)
      rideInfo2.bookingId `shouldBe` tRideBooking.id

      -- Driver2 Accepts a ride
      void . callBPP $
        rideRespond tRideBooking.id driverToken2 $
          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.ACCEPT

      tRide2 <- poll $ do
        tRide <- getBPPRide tRideBooking.id
        tRide.status `shouldBe` TRide.NEW
        return $ Just tRide

      void . callBPP $
        rideCancel driverToken2 tRide2.id $
          RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

      void . poll $
        callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just

      void . callBPP $ setDriverOnline driverToken1 False
      void . callBPP $ setDriverOnline driverToken2 False
