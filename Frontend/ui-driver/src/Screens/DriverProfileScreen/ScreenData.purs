{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.ScreenData where

import Data.Maybe

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Language.Types (STR(..))
import Screens.Types (DriverProfileScreenState, BottomNavBarState)
import Prelude (class Eq, unit, (<>), (==), (||), (/=))
import Common.Types.App (LazyCheck(..))

initData :: DriverProfileScreenState
initData = {
  data:  {
    driverName : "",
    driverVehicleType : "",
    driverRating : Just 2,
    base64Image : "",
    drivingLicenseNo : "",
    driverMobile : Just "",
    vehicleRegNumber : "",
    vehicleModelName : "",
    vehicleColor : "",
    driverAlternateNumber : Nothing,

    capacity : 0,
    vehicleSelected: [],
    downgradeOptions : []
    },

  props: {
    logoutModalView: false,
    showLiveDashboard : false
   }
}

data MenuOptions = DRIVER_PRESONAL_DETAILS |DRIVER_BANK_DETAILS | DRIVER_VEHICLE_DETAILS | ABOUT_APP | MULTI_LANGUAGE | HELP_AND_FAQS | DRIVER_LOGOUT | DRIVER_BOOKING_OPTIONS | REFER | APP_INFO_SETTINGS | LIVE_STATS_DASHBOARD
derive instance genericMenuoptions :: Generic MenuOptions _
instance eqMenuoptions :: Eq MenuOptions where eq = genericEq

type Listtype =
    { icon :: String,
      menuOptions :: MenuOptions
    }

