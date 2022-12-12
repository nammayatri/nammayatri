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
  beforeAndAfter_
    ( do
        mapM_ Utils.resetDriver [transporterDriver1, transporterDriver2]
        Utils.resetCustomer appRegistrationToken
    )
    $ it "Testing API flow for ride cancelled by Driver" $
      withBecknClients clients do
        let (origin, _destination, searchReq_) = route1SearchRequest
        Utils.setupDriver transporterDriver1 origin
        Utils.setupDriver transporterDriver2 origin

        Utils.search'Confirm appRegistrationToken searchReq_ \scRes -> do
          let bBookingId = scRes.bapBookingId
              tBooking = scRes.bppBooking
          -- Driver1 Accepts a ride
          void $ Utils.getRideInfo transporterDriver1 tBooking.id
          tRide1 <- Utils.acceptRide transporterDriver1 tBooking
          Utils.cancelRideByDriver transporterDriver1 tRide1
          Utils.checkBookingBapStatus appRegistrationToken bBookingId AppRB.AWAITING_REASSIGNMENT
          void $ Utils.getRideInfo transporterDriver2 tBooking.id

          -- Driver2 Accepts a ride
          void $ Utils.getRideInfo transporterDriver2 tBooking.id
          tRide2 <- Utils.acceptRide transporterDriver2 tBooking
          Utils.cancelRideByDriver transporterDriver2 tRide2
          Utils.checkBookingBapStatus appRegistrationToken bBookingId AppRB.AWAITING_REASSIGNMENT
          void $ Utils.getRideInfo transporterDriver2 tBooking.id

          -- Driver2 Accepts a ride
          tRide3 <- Utils.acceptRide transporterDriver2 tBooking
          Utils.cancelRideByDriver transporterDriver2 tRide3
          Utils.checkBookingBapStatus appRegistrationToken bBookingId AppRB.AWAITING_REASSIGNMENT
          void $ Utils.getRideInfo transporterDriver2 tBooking.id

          -- Driver2 Accepts a ride
          tRide4 <- Utils.acceptRide transporterDriver2 tBooking
          Utils.cancelRideByDriver transporterDriver2 tRide4
          Utils.checkBookingBapStatus appRegistrationToken bBookingId AppRB.AWAITING_REASSIGNMENT
          void $ Utils.getRideInfo transporterDriver2 tBooking.id

          -- Driver2 Accepts a ride
          tRide5 <- Utils.acceptRide transporterDriver2 tBooking
          Utils.cancelRideByDriver transporterDriver2 tRide5
          Utils.checkBookingBapStatus appRegistrationToken bBookingId AppRB.CANCELLED
