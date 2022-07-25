module Mobility.Transporter.DriversRejectRide where

import Common (getAppBaseUrl)
import EulerHS.Prelude
import HSpec
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import qualified Mobility.Transporter.APICalls as API
import Mobility.Transporter.Fixtures
import qualified Mobility.Transporter.Utils as Utils
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride rejected by Driver" $ withBecknClients clients do
      let (origin, _destination, searchReq_) = route1SearchRequest
      Utils.setupDriver transporterDriver1 origin
      scRes <- Utils.search'Confirm appRegistrationToken transporterDriver1 searchReq_
      let bBookingId = scRes.bapBookingId
          tBooking = scRes.bppBooking

      -- Driver Rejects a ride
      Utils.rejectRide appRegistrationToken transporterDriver1 tBooking bBookingId

      liftIO $ Utils.resetDriver transporterDriver1
