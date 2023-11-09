{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.UploadDrivingLicenseScreen.ComponentConfig
  where

import Language.Strings
import Prelude
import PrestoDOM

import Common.Types.App as Common
import Components.PopUpModal as PopUpModal
import Components.PopUpModal.Controller as PopUpModalConfig
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.StepsHeaderModal as StepsHeaderModel
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resource.Constants as Constant
import Screens.Types as ST
import Styles.Colors as Color

------------------------------ primaryButtonConfig --------------------------------
primaryButtonConfig :: ST.UploadDrivingLicenseState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = if isJust state.data.dateOfIssue then getString CONFIRM 
                           else if state.props.openHowToUploadManual then getString UPLOAD_PHOTO
                           else getString UPLOAD_DRIVING_LICENSE
      -- , textSize = FontSize.a_16
      }
      , width = MATCH_PARENT
      , background = Color.black900
      , margin = Margin 15 0 15 30
      , cornerRadius = 6.0
      , height = V 50
      , isClickable =  state.data.dob /= "" && DS.length state.data.driver_license_number >= 9 && (DS.toLower(state.data.driver_license_number) == DS.toLower(state.data.reEnterDriverLicenseNumber)) && state.data.dateOfIssue /= Just ""
      , alpha = if (state.data.dob /= "" && DS.length state.data.driver_license_number >= 9) && (DS.toLower(state.data.driver_license_number) == DS.toLower(state.data.reEnterDriverLicenseNumber)) && state.data.dateOfIssue /= Just "" then 1.0 else 0.8
      }
  in primaryButtonConfig'

------------------------------ primaryEditTextConfig --------------------------------
primaryEditTextConfig :: ST.UploadDrivingLicenseState -> PrimaryEditText.Config
primaryEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[A-Z0-9/-]*,25"
          , placeholder = getString ENTER_DL_NUMBER
          , capsLock = true
        }
      , topLabel
        { 
        text = getString DRIVING_LICENSE_NUMBER
        , color = Color.greyTextColor
        }
      , type = "password"
      , margin = MarginBottom 15
      , background = Color.white900
      , id = EHC.getNewIDWithTag "EnterDrivingLicenseEditText"
      }
    in primaryEditTextConfig'

------------------------------ primaryEditTextConfigReEnterDl --------------------------------
primaryEditTextConfigReEnterDl :: ST.UploadDrivingLicenseState -> PrimaryEditText.Config
primaryEditTextConfigReEnterDl state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[A-Z0-9/-]*,25"
          -- , fontStyle = FontStyle.bold LanguageStyle
          -- , textSize = FontSize.a_16
          , placeholder = getString ENTER_DL_NUMBER
          , capsLock = true
          , color = Color.black800
        }
      , stroke = if (DS.toLower(state.data.driver_license_number) /= DS.toLower(state.data.reEnterDriverLicenseNumber) && state.data.reEnterDriverLicenseNumber /= "") then ("1," <> Color.red) else ("1," <> Color.borderColorLight)
      , topLabel
        { --textSize = FontSize.a_12
        -- , 
        text = getString RE_ENTER_DRIVING_LICENSE_NUMBER
        , color = Color.greyTextColor
        }
      , margin = MarginBottom 15
      , background = Color.white900
      , id = EHC.getNewIDWithTag "ReEnterDrivingLicenseEditText"
      }
    in primaryEditTextConfig'

stepsHeaderModelConfig :: ST.UploadDrivingLicenseState ->Int -> StepsHeaderModel.Config
stepsHeaderModelConfig state headerValue = let
    config = StepsHeaderModel.config headerValue
    stepsHeaderConfig' = config 
     {
      stepsViewVisibility = false,
      profileIconVisibility = true,
      driverNumberVisibility = true,
      logoutVisibility = true,
      driverTextArray = Constant.driverTextArray Common.FunctionCall,
      rightButtonText = getString LOGOUT,
      customerTextArray = [],
      driverMobileNumber = Just state.data.mobileNumber
     }
  in stepsHeaderConfig'

fileCameraLayoutConfig:: ST.UploadDrivingLicenseState -> PopUpModalConfig.Config
fileCameraLayoutConfig state = let
    config = PopUpModalConfig.config
    popUpConf' = config {
      cornerRadius = Corners 15.0 true true true true,
      margin = Margin 16 16 16 16 ,
      gravity = CENTER,
      optionButtonOrientation = "VERTICAL",
      padding = Padding 16 16 16 16,
      buttonLayoutMargin = Margin 0 0 0 0,

     primaryText {
          text = getString UPLOAD_PHOTO
        , margin = Margin 16 0 16 0
        , visibility = VISIBLE
        , gravity = LEFT
      },
      secondaryText {
        visibility = GONE
      },
      option1 {
        text = "Take a Photo"
      , color = Color.black900
      , strokeColor = Color.white900
      , padding = Padding 15 10 15 10
      , visibility = true
      , margin = MarginTop 0
      , background = Color.white900
      , width = MATCH_PARENT
      , gravity = LEFT
      },
      option2 {
        text = "Gallery"
      , color = Color.black900
      , strokeColor = Color.white900
      , padding = Padding 15 10 15 10
      , margin = MarginTop 0
      , width = MATCH_PARENT
      , background = Color.white900
      , gravity = LEFT
      }
    }
  in popUpConf'