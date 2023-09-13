{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddVehicleDetailsScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Common.Types.App
import Data.Maybe
import Data.String
import Font.Size as FontSize
import Language.Strings
import Language.Types (STR(..))
import Prelude
import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color
import Components.ReferralMobileNumber as ReferralMobileNumber
import Components.StepsHeaderModel as StepsHeaderModel
import Components.PopUpModal as PopUpModal
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Style as FontStyle

primaryButtonConfig :: ST.AddVehicleDetailsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = if (state.props.openHowToUploadManual) then ("Take Photo") else ("Upload Registration Certificate (RC)")  
      , textSize = FontSize.a_16}
      , width = MATCH_PARENT
      , cornerRadius = 6.0
      , height = (V 50)
      , background = Color.black900 
     -- , alpha = if ((state.props.isValidState) && (toLower(state.data.vehicle_registration_number) == toLower(state.data.reEnterVehicleRegistrationNumber)) && (state.data.dateOfRegistration /= Just "") )  then 1.0 else 0.8
      --, isClickable = ((state.props.isValidState) && (toLower(state.data.vehicle_registration_number) == toLower(state.data.reEnterVehicleRegistrationNumber)) && (state.data.dateOfRegistration /= Just "") )
      , margin = (Margin 15 0 15 30)
      }
  in primaryButtonConfig'


referalNumberConfig :: ST.AddVehicleDetailsScreenState -> ReferralMobileNumber.Config
referalNumberConfig state = let 
  config' = ReferralMobileNumber.config
  referalNumberConfig' = config'{
   isApplyButtonActive = state.props.btnActive, 
   referralNumber = if state.props.isEdit then state.data.referral_mobile_number else ""
  }
  in referalNumberConfig'
  
stepsHeaderModelConfig ::  ST.AddVehicleDetailsScreenState -> StepsHeaderModel.Config
stepsHeaderModelConfig state = let
    config = StepsHeaderModel.config if state.props.openHowToUploadManual then 6 else 5
    stepsHeaderConfig' = config 
     {
      stepsViewVisibility = false
    , driverMobileNumber = Just state.data.driverMobileNumber
     }
  in stepsHeaderConfig'

logoutPopUp :: ST.AddVehicleDetailsScreenState -> PopUpModal.Config
logoutPopUp  state = let 
  config' = PopUpModal.config
  popUpConfig' = config' {
    primaryText {text = (getString LOGOUT)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT)},
    option1 {text = (getString LOGOUT)},
    option2 {text = (getString CANCEL)},
    onBoardingButtonVisibility = true
  }
  in popUpConfig'


fileCameraLayoutConfig:: ST.AddVehicleDetailsScreenState -> PopUpModal.Config
fileCameraLayoutConfig state = let
    config = PopUpModal.config
    popUpConf' = config {
      cornerRadius = (Corners 15.0 true true true true),
      margin = (Margin 16 290 16 200) ,
      gravity = CENTER,
      onBoardingButtonVisibility = true
    ,primaryText {
        text =  ("Upload Photo")
      , margin = (Margin 24 24 24 12)
      , visibility = VISIBLE
     },
      secondaryText {
        text = (getString OTP_LIMIT_EXCEEDED_MESSAGE)
      , color = Color.black600
      , margin = (Margin 24 0 24 32)
      , visibility = GONE
        },
      option1 {
        text =  ("Take a Photo")
      , fontSize = FontSize.a_16
      , color = Color.white900
      , strokeColor = Color.black700
      , fontStyle = FontStyle.semiBold LanguageStyle
      , visibility = true

      },
      option2 {text = ("Gallery")
      , color = Color.yellow900
      , strokeColor = Color.white900
      , fontSize = FontSize.a_16
      , margin = (Margin 16 0 16 0 )
      , fontStyle = FontStyle.semiBold LanguageStyle
      , width = (V 50)

      , background = Color.black900
      }
    }
  in popUpConf'
