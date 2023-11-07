{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddVehicleDetailsScreen.ScreenData where
import Data.Maybe
import Screens.Types
import Foreign.Object (empty)

initData :: AddVehicleDetailsScreenState
initData = {
    data: {
      vehicle_type : "",
      vehicle_model_name : "",
      vehicle_color : "",
      vehicle_registration_number : "",
      reEnterVehicleRegistrationNumber : "",
      rc_base64 : "",
      vehicle_rc_number : "",
      referral_mobile_number : "",
      rcImageID : "",
      errorMessage : "",
      dateOfRegistration : Nothing,
      dateOfRegistrationView : "",
      logField : empty,
      driverMobileNumber : ""
    },
    props: {
      rcAvailable : false,
      vehicleTypes : [Sedan, Hatchback, SUV, Auto],
      openSelectVehicleTypeModal : false,
      openRegistrationModal : false,
      rc_name : "",
      input_data : "",
      enable_upload : true,
      openRCManual : false,
      openReferralMobileNumber : false,
      isValidState : false,
      btnActive : false,
      referralViewstatus : false,
      isEdit : false,
      isValid : false,
      limitExceedModal : false,
      errorVisibility : false,
      openRegistrationDateManual : false,
      addRcFromProfile : false,
      isDateClickable : true,
      openHowToUploadManual : false,
      logoutModalView : false,
      validateProfilePicturePopUp : false,
      imageCaptureLayoutView : false,
      fileCameraOption : false,
      fileCameraPopupModal : false,
      validating : false,
      successfulValidation : false
    }
}