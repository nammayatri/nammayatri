{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.ComponentConfig where

import Language.Strings
import PrestoDOM
import Common.Types.App (LazyCheck)
import Components.PopUpModal as PopUpModal
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (unit, (/=), (<>), (==))
import Screens.DriverProfileScreen.ScreenData (MenuOptions(..), Listtype)
import Screens.Types as ST

logoutPopUp :: ST.DriverProfileScreenState -> PopUpModal.Config
logoutPopUp  state = let 
  config' = PopUpModal.config
  popUpConfig' = config' {
    primaryText {text = (getString LOGOUT)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT)},
    option1 {text = (getString GO_BACK)},
    option2 {text = (getString LOGOUT)}
  }
  in popUpConfig'

optionList :: LazyCheck -> Array Listtype
optionList lazy =
    [
      {menuOptions: DRIVER_PRESONAL_DETAILS , icon:"ny_ic_profile,https://assets.juspay.in/nammayatri/images/driver/ny_ic_profile.png"},
      {menuOptions: DRIVER_VEHICLE_DETAILS , icon:"ny_ic_car_profile,https://assets.juspay.in/nammayatri/images/driver/ny_ic_car_profile.png"}
    ]
    <> (if (getMerchant lazy /= NAMMAYATRI)  then [{menuOptions: DRIVER_BOOKING_OPTIONS , icon:"ic_booking_options,https://assets.juspay.in/nammayatri/images/driver/ic_booking_options.png"}] else []) <>
    [
      {menuOptions: APP_INFO_SETTINGS , icon:"ny_ic_app_info,https://assets.juspay.in/nammayatri/images/driver/ny_ic_app_info.png"},
      {menuOptions: MULTI_LANGUAGE , icon:"ny_ic_language,https://assets.juspay.in/nammayatri/images/driver/ny_ic_language.png"},
      {menuOptions: HELP_AND_FAQS , icon:"ny_ic_head_phones,https://assets.juspay.in/nammayatri/images/driver/ny_ic_head_phones.png"}
    ]
    <> (if (getMerchant lazy == NAMMAYATRI) then [{menuOptions: LIVE_STATS_DASHBOARD , icon:"ic_graph_black,https://assets.juspay.in/nammayatri/images/common/ic_graph_black.png"}] else []) <>
    [ 
      {menuOptions: ABOUT_APP , icon:"ny_ic_about,https://assets.juspay.in/nammayatri/images/driver/ny_ic_about.png"},
      {menuOptions: DRIVER_LOGOUT , icon:"ny_ic_logout_grey,https://assets.juspay.in/nammayatri/images/driver/ny_ic_logout_grey.png"}
    ]