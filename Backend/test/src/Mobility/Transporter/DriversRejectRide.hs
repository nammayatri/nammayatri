 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
