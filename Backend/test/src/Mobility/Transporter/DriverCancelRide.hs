 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.Transporter.DriverCancelRide where

import Common (getAppBaseUrl)
import qualified "rider-app" Domain.Types.Booking as AppRB
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
