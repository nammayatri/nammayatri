module Mobility.Transporter.DriverCancelRide where

import Common (getAppBaseUrl)
import qualified "app-backend" Domain.Types.Booking as AppRB
import EulerHS.Prelude
import HSpec
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes (route1SearchRequest)
import qualified Mobility.Transporter.APICalls as API
import Mobility.Transporter.Fixtures
import qualified Mobility.Transporter.Utils as Utils
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by Driver" $ withBecknClients clients do
      let (origin, _destination, searchReq_) = route1SearchRequest
      Utils.setupDriver transporterDriver1 origin
      Utils.setupDriver transporterDriver2 origin

      scRes <- Utils.search'Confirm appRegistrationToken transporterDriver1 searchReq_
      let bBookingId = scRes.bapBookingId
          tBooking = scRes.bppBooking

      -- Driver1 Accepts a ride
      tRide1 <- Utils.acceptRide transporterDriver1 tBooking

      Utils.cancelRideByDriver transporterDriver1 tRide1

      Utils.checkRideBapStatus appRegistrationToken bBookingId AppRB.AWAITING_REASSIGNMENT

      void $ Utils.getRideInfo transporterDriver2 tBooking.id

      -- Driver2 Accepts a ride
      tRide2 <- Utils.acceptRide transporterDriver2 tBooking

      Utils.cancelRideByDriver transporterDriver2 tRide2
      Utils.checkRideBapStatus appRegistrationToken bBookingId AppRB.CANCELLED

      mapM_ (liftIO . Utils.resetDriver) [transporterDriver1, transporterDriver2]
