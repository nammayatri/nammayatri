{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.BottomNavBar.Controller where

import Screens.Types (BottomNavBarState)
import Storage (getValueToLocalNativeStore, KeyStore(..))
import Prelude((==))

data Action = OnNavigate String 

navData :: Int -> BottomNavBarState
navData activeIndex = {
   activeIndex: activeIndex ,
   screenName : "driver homeScreen screen",
   navButton: [
    {
      activeIcon: "ny_ic_home_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_home_active.png",
      defaultIcon: "ny_ic_home_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_home_inactive.png",
      text: "Home"
    },
    {
      activeIcon: "ny_ic_cab_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_cab_active.png",
      defaultIcon: "ny_ic_cab_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_cab_inactive.png",
      text: "Rides"
    },
    {
      activeIcon: "ny_ic_contest_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_contest_active.png",
      defaultIcon: if (getValueToLocalNativeStore REFERRAL_ACTIVATED) == "true" then  "ny_ic_contest_alert,https://assets.juspay.in/nammayatri/images/driver/ny_ic_contest_alert.png" else "ny_ic_contest_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_contest_alert.png",
      text: "Contest"
    },
    {
      activeIcon: "",
      defaultIcon: if (getValueToLocalNativeStore ALERT_RECEIVED) == "true" then "ny_ic_notification,https://assets.juspay.in/nammayatri/images/driver/ny_ic_notification.png" else "ny_ic_no_notification,https://assets.juspay.in/nammayatri/images/driver/ny_ic_no_notification.png",
      text: "Alert"
    },
    {
      activeIcon: "ny_ic_account_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_account_active.png",
      defaultIcon: "ny_ic_account_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_account_inactive.png",
      text: "Profile"
    }
  ]
}