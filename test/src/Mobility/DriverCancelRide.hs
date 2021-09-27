module Mobility.DriverCancelRide where

import Beckn.Types.Id
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
import qualified "beckn-transport" Types.Storage.CancellationReason as SCR
import qualified "beckn-transport" Types.Storage.Ride as TRide
import qualified "app-backend" Types.Storage.RideBooking as AppRB
import qualified "beckn-transport" Types.Storage.RideBooking as TRB
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by Driver" $ withBecknClients clients do
      (quoteId, bRideBookingId) <- doAnAppSearch

      tRideBooking <- poll $ do
        trb <- getBPPRideBooking (cast quoteId)
        trb.status `shouldBe` TRB.CONFIRMED
        return $ Just trb

      -- Driver Accepts a ride
      void . callBPP $
        rideRespond tRideBooking.id driverToken $
          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.ACCEPT

      tRide <- poll $ do
        tRide <- getBPPRide (cast quoteId)
        tRide.status `shouldBe` TRide.NEW
        return $ Just tRide

      void . callBPP $
        rideCancel appRegistrationToken tRide.id $
          RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

      void . poll $
        callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just