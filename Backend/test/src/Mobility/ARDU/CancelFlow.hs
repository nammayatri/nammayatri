{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.CancelFlow where

import Common (getAppBaseUrl)
import EulerHS.Prelude
import HSpec
import Mobility.ARDU.APICalls (getDriverOfferBppBaseUrl)
import Mobility.ARDU.Fixtures (arduDriver1)
import Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getDriverOfferBppBaseUrl
  beforeAndAfter_
    ( do
        Utils.resetDriver arduDriver1
        Utils.resetCustomer appRegistrationToken
    )
    $ do
      it "Testing API flow for ride cancelled by Driver" $
        withBecknClients clients cancelFlow
      it "Testing API flow for ride cancelled by app" $
        withBecknClients clients appCancelFlow

cancelFlow :: ClientsM ()
cancelFlow = do
  let (origin, _, searchReq') = karnatakaSearchReq
  Utils.setupDriver arduDriver1 origin
  scRes <- search'Confirm appRegistrationToken arduDriver1 searchReq'
  cancelRideByDriver arduDriver1 scRes.bapBookingId scRes.ride

appCancelFlow :: ClientsM ()
appCancelFlow = do
  let (origin, _, searchReq') = karnatakaSearchReq
  Utils.setupDriver arduDriver1 origin
  scRes <- search'Confirm appRegistrationToken arduDriver1 searchReq'
  cancelRideByApp appRegistrationToken arduDriver1 scRes.bapBookingId
