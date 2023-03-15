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
import Data.Generic.Rep.Eq (genericEq)
import Language.Types (STR(..))
import Prelude (class Eq)
import Screens.Types (DriverProfileScreenState, BottomNavBarState)

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
    vehicleColor : ""
    },

  props: {
    logoutModalView: false
   }
}

data MenuOptions = DRIVER_PRESONAL_DETAILS | DRIVER_VEHICLE_DETAILS | ABOUT_APP | MULTI_LANGUAGE | HELP_AND_FAQS | DRIVER_LOGOUT | DRIVER_BANK_DETAILS | REFER | APP_INFO_SETTINGS
derive instance genericMenuoptions :: Generic MenuOptions _
instance eqMenuoptions :: Eq MenuOptions where eq = genericEq

type Listtype =
    { icon :: String,
      menuOptions :: MenuOptions
    }

optionList :: String -> Array Listtype
optionList dummy = 
    [
      {menuOptions: DRIVER_PRESONAL_DETAILS , icon:"ny_ic_profile,https://assets.juspay.in/nammayatri/images/driver/ny_ic_profile.png"},
      {menuOptions: DRIVER_VEHICLE_DETAILS , icon:"ny_ic_car_profile,https://assets.juspay.in/nammayatri/images/driver/ny_ic_car_profile.png"},
      {menuOptions: APP_INFO_SETTINGS , icon:"ny_ic_app_info,https://assets.juspay.in/nammayatri/images/driver/ny_ic_app_info.png"},
      {menuOptions: DRIVER_BANK_DETAILS , icon:"ny_ic_bank_profile,https://assets.juspay.in/nammayatri/images/driver/ny_ic_bank_profile.png"},
      {menuOptions: MULTI_LANGUAGE , icon:"ny_ic_language,https://assets.juspay.in/nammayatri/images/driver/ny_ic_language.png"},
      {menuOptions: HELP_AND_FAQS , icon:"ny_ic_head_phones,https://assets.juspay.in/nammayatri/images/driver/ny_ic_head_phones.png"},
      {menuOptions: ABOUT_APP , icon:"ny_ic_about,https://assets.juspay.in/nammayatri/images/driver/ny_ic_about.png"},
      {menuOptions: REFER , icon:"ic_add_person,https://assets.juspay.in/nammayatri/images/driver/ic_add_person.png"},
      {menuOptions: DRIVER_LOGOUT , icon:"ny_ic_logout_grey,https://assets.juspay.in/nammayatri/images/driver/ny_ic_logout_grey.png"}
    ]


navData :: BottomNavBarState
navData = {
   activeIndex: 3,
   screenName: "DriverProfileScreen",
   navButton: [
    {
      defaultIcon: "ny_ic_home_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_home_inactive.png",
      activeIcon: "ny_ic_home_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_home_inactive.png",
      text: "Home"
    },
    {
      defaultIcon: "ny_ic_cab_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_cab_inactive.png",
      activeIcon: "ny_ic_cab_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_cab_inactive.png",
      text: "Rides"
    },
    {
      defaultIcon: "ny_ic_account_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_account_active.png",
      activeIcon: "ny_ic_account_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_account_active.png",
      text: "Profile"
    }
  ]
}
