{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.UploadDrivingLicenseScreen.ScreenData where

import Data.Maybe

import Engineering.Helpers.Commons as EHC
import Foreign.Object (empty)
import Screens.RegistrationScreen.ScreenData (dummyCityConfig)
import Screens.Types (UploadDrivingLicenseState)
import MerchantConfig.DefaultConfig as DC

initData :: UploadDrivingLicenseState
initData = {
      data: {
        imageFront : ""
      , imageBack : ""
      , imageNameFront : "imagefront.jpg"
      , imageNameBack : "imageback.jpg"
      , driver_license_number : ""
      , reEnterDriverLicenseNumber : ""
      , dob : ""
      , dobView : ""
      , imageIDFront : ""
      , imageIDBack : ""
      , rcVerificationStatus : ""
      , errorMessage : ""
      , dateOfIssue : Nothing
      , dateOfIssueView : ""
      , imageFrontUrl : ""
      , logField : empty
      , mobileNumber : ""
      , cityConfig : dummyCityConfig
      , config : DC.config
      },
      props: {
        openRegistrationModal : false
      , openLicenseManual : false    
      , input_data : ""   
      , clickedButtonType : "" 
      , openGenericMessageModal : false
      , errorVisibility : false
      , openDateOfIssueManual: false
      , isDateClickable : true
      , openHowToUploadManual : false
      , logoutPopupModal : false
      , validateProfilePicturePopUp : false
      , imageCaptureLayoutView : false
      , fileCameraPopupModal : false
      , fileCameraOption : false
      , validating : false
      , successfulValidation : false
      }
    }