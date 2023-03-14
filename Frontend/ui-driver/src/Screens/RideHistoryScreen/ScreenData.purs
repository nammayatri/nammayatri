module Screens.RideHistoryScreen.ScreenData where

import Screens.Types (AnimationState(..), RideHistoryScreenState, BottomNavBarState)
import PrestoDOM.Types.Core (toPropValue)

initData :: RideHistoryScreenState
initData = {
  shimmerLoader : AnimatingIn,
  recievedResponse: false,
  prestoListArrayItems : [],
  rideList: [],
  currentTab: "COMPLETED",
  loadMoreDisabled: false,
  selectedItem: {
    date : "",
    time : "",
    total_amount : 0,
    card_visibility : "",
    shimmer_visibility : "",
    rideDistance : "",
    status : "",
    vehicleModel : "",
    shortRideId : "",
    vehicleNumber : "",
    driverName : "",
    driverSelectedFare : 0,
    vehicleColor : "",
    id : "",
    updatedAt : "",
    source : "",
    destination : ""
  },
  offsetValue: 0,
  loaderButtonVisibility: false
}
navbarData :: BottomNavBarState
navbarData = {
   activeIndex: 1,
   screenName: "RideHistoryScreen",
   navButton: [
    {
      defaultIcon: "ny_ic_home_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_home_inactive.png",
      activeIcon: "ny_ic_home_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_home_active.png",
      text: "Home"
    },
    {
      defaultIcon: "ny_ic_cab_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_cab_inactive.png",
      activeIcon: "ny_ic_cab_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_cab_active.png",
      text: "Rides"
    },
    {
      defaultIcon: "ny_ic_account_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_account_inactive.png",
      activeIcon: "ny_ic_account_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_account_active.png",
      text: "Profile"
    }
  ]
}