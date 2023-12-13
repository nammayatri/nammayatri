{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverDetailsScreen.ScreenData where

import Screens.Types (DriverDetailsScreenState, KeyboardModalType(..))
import Data.Maybe

initData :: DriverDetailsScreenState
initData =
  { data:
      { driverName: ""
      , driverVehicleType: ""
      , driverRating: Just 2.0
      , base64Image: "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"
      , drivingLicenseNo: ""
      , driverMobile: Just ""
      , driverAlternateMobile: Nothing
      , driverEditAlternateMobile: Nothing
      , genderSelectionModal:
          { selectionOptions: []
          , activeIndex: Nothing
          , selectedReasonCode: ""
          , selectedReasonDescription: ""
          , isSelectButtonActive: false
          }
      , driverGender: Nothing
      , otpLimit: 5
      , otpBackAlternateNumber: Nothing
      }
  , props:
      { keyboardModalType: NONE
      , checkAlternateNumber: true
      , otpAttemptsExceeded: false
      , enterOtpFocusIndex: 0
      , genderSelectionModalShow: false
      , otpIncorrect: false
      , alternateMobileOtp: ""
      , removeNumberPopup: false
      , isEditAlternateMobile: false
      , numberExistError: false
      }
  }
