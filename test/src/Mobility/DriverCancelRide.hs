module Mobility.DriverCancelRide where

import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "beckn-transport" Types.Storage.CancellationReason as SCR
import qualified "app-backend" Types.Storage.RideBooking as AppRB
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by Driver" $ withBecknClients clients do
      (_, bRideBookingId, transporterOrderPi) <- doAnAppSearch
      let transporterOrderPiId = transporterOrderPi.id

      -- Driver Accepts a ride
      void . callBPP $
        rideRespond driverToken $
          RideAPI.SetDriverAcceptanceReq transporterOrderPiId RideAPI.ACCEPT

      void . callBPP $
        rideCancel appRegistrationToken transporterOrderPiId $
          RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

      void . poll $
        callBAP (rideBookingStatus bRideBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just