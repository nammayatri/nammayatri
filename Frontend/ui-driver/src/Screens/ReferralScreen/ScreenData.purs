module Screens.ReferralScreen.ScreenData where

import Screens.Types (ReferralScreenState, BottomNavBarState, ReferralType(..))
import PrestoDOM.Types.Core (toPropValue)
import Data.Maybe (Maybe(..))

initData :: ReferralScreenState
initData =  {
  data:  { 
    referralCode : "" 
  , confirmReferralCode : ""
  , password : ""
  , driverInfo : { 
      driverName : "",
      driverMobile : Just "",
      vehicleRegNumber : "",
      referralCode : Nothing
    }
  , driverPerformance : {
      referrals : {
        totalActivatedCustomers : 0,
        totalReferredCustomers : 0
    }
}
  }
,  props: {
    primarybtnActive :false,
    confirmBtnActive : false,
    passwordPopUpVisible : false,
    callSupportPopUpVisible : false,
    enableReferralFlowCount : 0,
    stage : ComingSoonScreen,
    seconds : 4,
    id : "SuccessScreenTimerId",
    firstTime : false
  }
}
