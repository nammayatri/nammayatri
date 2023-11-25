{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterOTPScreen.ScreenData where

import Screens.Types (EnterOTPScreenState)
import Engineering.Helpers.Commons as EHC
import MerchantConfig.DefaultConfig as DC

initData :: EnterOTPScreenState
initData =  {
  data:  {
    otp : "",
    tokenId : "",
    attemptCount : 3,
    mobileNo : "",
    timer: "10s",
    capturedOtp : "",
    focusedIndex : 1,
    editTextId : (EHC.getNewIDWithTag "EnterOTPNumberEditText"),
    config : DC.config
    },
  props: {
    btnActive :false,
    isValid : false,
    resendEnabled : false,
    otpTmp :false
    }
}