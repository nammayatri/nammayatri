{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.BottomNavBar.Controller where

import Data.Maybe as Maybe
import Helpers.Utils (getMerchant, Merchant(..))
import Prelude (unit, (<>), (==))
import Screens.Types (BottomNavBarState)
import Storage (getValueToLocalNativeStore, KeyStore(..))

data Action = OnNavigate String 

navData :: Int -> BottomNavBarState
navData activeIndex = {
   activeIndex: activeIndex ,
   navButton: [
    {
      activeIcon: "ic_home_active,https://assets.juspay.in/nammayatri/images/driver/ic_home_active.png",
      defaultIcon: "ic_home_inactive,https://assets.juspay.in/nammayatri/images/driver/ic_home_inactive.png",
      text: "Home"
    },
    {
      activeIcon: "ny_ic_rides_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_rides_active.png",
      defaultIcon: "ny_ic_rides_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_rides_inactive.png",
      text: "Rides"
    }] <> 
    (if (getMerchant unit == NAMMAYATRIPARTNER) then [{
      activeIcon: "ic_referral_active,https://assets.juspay.in/nammayatri/images/driver/ic_referral_active.png",
      defaultIcon: if (getValueToLocalNativeStore REFERRAL_ACTIVATED) == "true" then  "ny_ic_contest_alert,https://assets.juspay.in/nammayatri/images/driver/ny_ic_contest_alert.png" else "ic_referral_inactive,https://assets.juspay.in/nammayatri/images/driver/ic_referral_inactive.png",
      text: "Contest"
    }] else []) <> 
    [{
      activeIcon: "ny_ic_alerts_active",
      defaultIcon: "ny_ic_alerts_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_alerts_inactive.png",
      text: "Alert"
    }
    -- , -- TODO::- DEPRECATE THE DESIGN
    -- {
    --   activeIcon: "ic_profile_active,https://assets.juspay.in/nammayatri/images/driver/ic_profile_active.png",
    --   defaultIcon: "ic_profile_inactive,https://assets.juspay.in/nammayatri/images/driver/ic_profile_inactive.png",
    --   text: "Profile"
    -- }
  ]
}