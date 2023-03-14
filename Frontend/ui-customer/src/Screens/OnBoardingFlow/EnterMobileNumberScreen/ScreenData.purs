module Screens.EnterMobileNumberScreen.ScreenData where

import Screens.Types (EnterMobileNumberScreenState)

initData :: EnterMobileNumberScreenState
initData = {
    data: {
      mobileNumber: ""
    , tokenId : ""
    , attempts : 0
    , otp : ""
    , timer : ""
    , timerID : ""
    },
    props: {
        enterOTP : false,
        btnActiveMobileNuber : false,
        btnActiveOTP :false,
        isValidMobileNumber : true,
        wrongOTP : false,
        resendEnable : true,
        capturedOtp : "",
        isReadingOTP : true,
        letterSpacing : 1.0
    }
}