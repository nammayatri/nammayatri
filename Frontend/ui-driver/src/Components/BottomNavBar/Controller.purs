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