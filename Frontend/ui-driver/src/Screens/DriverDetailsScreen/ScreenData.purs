{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverDetailsScreen.ScreenData where

import Screens.Types(DriverDetailsScreenState, KeyboardModalType(..))
import Data.Maybe

initData :: DriverDetailsScreenState
initData = {
  data:  {
    driverName : "",
    driverVehicleType : "",
    driverRating : Nothing,
    base64Image : "",
    drivingLicenseNo : "",
    driverMobile : Just "",
    driverAlternateMobile : Nothing,
    driverEditAlternateMobile : Nothing,
    genderSelectionModal : {
          selectionOptions : [],
          activeIndex : Nothing,
          selectedReasonCode : "",
          selectedReasonDescription : "",
          isSelectButtonActive : false
        },
    driverGender : Nothing,
    otpLimit : 5,
    otpBackAlternateNumber : Nothing
    },
  props: {
    keyboardModalType : NONE,
    checkAlternateNumber : true,
    otpAttemptsExceeded: false,
    enterOtpFocusIndex : 0,
    genderSelectionModalShow : false,
    otpIncorrect : false,
    alternateMobileOtp : "",
    removeNumberPopup : false,
    isEditAlternateMobile : false,
    numberExistError : false

  }
}



