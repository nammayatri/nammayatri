module Mobility.DriversRejectRide where

import qualified "app-backend" Domain.Types.RideBooking as AppRB
import qualified "beckn-transport" Domain.Types.RideBooking as TRB
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride rejected by Driver" $ withBecknClients clients do
      (bapQuoteId, bRideBookingId) <- doAnAppSearch

      tRideBooking <- poll $ do
        tQuoteId <- getBPPQuoteId bapQuoteId
        trb <- getBPPRideBooking tQuoteId
        trb.status `shouldBe` TRB.CONFIRMED
        return $ Just trb

      -- Driver Rejects a ride
      void . callBPP $
        rideRespond tRideBooking.id driverToken1 $
          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.REJECT

      void . poll $
        callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just

      void . callBPP $ setDriverOnline driverToken1 False
