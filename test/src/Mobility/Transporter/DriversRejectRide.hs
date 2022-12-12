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
  beforeAndAfter_
    ( do
        Utils.resetDriver transporterDriver1
        Utils.resetDriver transporterDriver2
        Utils.resetCustomer appRegistrationToken
    )
    $ it "Testing API flow for ride rejected by Driver" $ withBecknClients clients do
      let (origin, _destination, searchReq_) = route1SearchRequest
      Utils.setupDriver transporterDriver1 origin
      Utils.setupDriver transporterDriver2 origin
      Utils.search'Confirm appRegistrationToken searchReq_ \scRes -> do
        let tBooking = scRes.bppBooking
        let bBookingId = scRes.bapBookingId

        void $ Utils.getRideInfo transporterDriver1 tBooking.id
        void $ Utils.getRideInfo transporterDriver2 tBooking.id

        -- Driver Rejects a ride
        Utils.rejectRide transporterDriver1 tBooking

        -- now this is empty
        Utils.checkEmptyRideInfo transporterDriver1 tBooking.id
        -- and this is not
        void $ Utils.getRideInfo transporterDriver2 tBooking.id
        Utils.cancelRideByApp appRegistrationToken bBookingId
