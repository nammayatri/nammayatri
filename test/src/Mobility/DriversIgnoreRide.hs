module Mobility.DriversIgnoreRide where

import Beckn.Types.Id
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
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

      -- Driver Rejects a ride
      void . callBPP $
        rideRespond tRideBooking.id driverToken $
          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.REJECT

      void . poll $
        callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just
