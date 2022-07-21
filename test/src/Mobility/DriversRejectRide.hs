module Mobility.DriversRejectRide where

import qualified "beckn-transport" API.UI.Booking as TbeBookingAPI
import Common (getAppBaseUrl)
import qualified "app-backend" Domain.Types.Booking as AppRB
import qualified "beckn-transport" Domain.Types.Booking as TRB
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
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
        rideRespond tBooking.id driverToken1 $
          TbeBookingAPI.SetDriverAcceptanceReq TbeBookingAPI.REJECT

      void . poll $
        callBAP (appBookingStatus bBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just

      void . callBPP $ setDriverOnline driverToken1 False
