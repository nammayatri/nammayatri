{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TripDetailsScreen.ScreenData where

import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Screens.Types (TripDetailsScreenState, PaymentMode(..), TripDetailsGoBackType(..))
import ConfigProvider
import Data.Maybe (Maybe(..))

initData :: TripDetailsScreenState
initData =
  { data:
      { message: ""
      , driverName: ""
      , date: ""
      , time: ""
      , source: ""
      , destination: ""
      , totalAmount: ""
      , paymentMode: CASH
      , rating: 0
      , tripId: ""
      , selectedItem: dummyIndividualCard
      , vehicleVariant: Nothing
      , config: getAppConfig appConfig
      }
  , props:
      { reportIssue: true
      , issueReported: false
      , activateSubmit: false
      , fromMyRides: Home
      , showConfirmationPopUp: false
      , canConnectWithDriver: true
      }
  }
