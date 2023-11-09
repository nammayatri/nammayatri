{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddVehicleDetailsScreen.ComponentConfig where

import Data.Maybe
import Data.String
import Language.Strings
import Prelude
import PrestoDOM

import Common.Types.App as Common
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.ReferralMobileNumber as ReferralMobileNumber
import Components.StepsHeaderModal as StepsHeaderModel
import Data.String as DS
import Font.Size as FontSize
import Language.Types (STR(..))
import MerchantConfig.Utils (getValueFromConfig)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resource.Constants as Constant
import Screens.AddVehicleDetailsScreen.Controller (validateRegistrationNumber)
import Screens.Types (StageStatus(..))
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: ST.AddVehicleDetailsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    imageUploadCondition = true--getValueFromConfig "imageUploadOptional" || state.props.isValidState
    activate = ((toLower(state.data.vehicle_registration_number) == toLower(state.data.reEnterVehicleRegistrationNumber)) && 
                -- (state.data.dateOfRegistration /= Just "") && 
                state.data.vehicle_registration_number /= "" &&
                ((DS.length state.data.vehicle_registration_number >= 2) && validateRegistrationNumber (DS.take 2 state.data.vehicle_registration_number)) &&
                imageUploadCondition)
    primaryButtonConfig' = config 
      { textConfig{ text = if isJust state.data.dateOfRegistration then getString CONFIRM 
                           else if state.props.openHowToUploadManual then getString UPLOAD_PHOTO
                           else getString UPLOAD_RC}
      , width = MATCH_PARENT
      , cornerRadius = 0.0
      , height = (V 60)
      , background = Color.black900 
      , alpha = if activate then 1.0 else 0.8
      , isClickable = activate
      , margin = (Margin 0 0 0 0)
      , id = "AddVehiclePrimaryButton"
      }
  in primaryButtonConfig'


referalNumberConfig :: ST.AddVehicleDetailsScreenState -> ReferralMobileNumber.Config
referalNumberConfig state = let 
  config' = ReferralMobileNumber.config
  referalNumberConfig' = config'{
    isApplyButtonActive = state.props.btnActive, 
    referralNumber = if state.props.isEdit then state.data.referral_mobile_number else "",
    isValid = state.props.isValid,
    errorText = getString if state.props.isValid then INVALID_REFERRAL_NUMBER else INVALID_REFERRAL_CODE
  }
  in referalNumberConfig'

stepsHeaderModelConfig :: ST.AddVehicleDetailsScreenState -> StepsHeaderModel.Config
stepsHeaderModelConfig state = let
    config = StepsHeaderModel.config if state.props.openHowToUploadManual then 6 else 5
    stepsHeaderConfig' = config 
     {
      stepsViewVisibility = false,
      profileIconVisibility = true,
      driverNumberVisibility = true,
      customerTextArray = [],
      driverTextArray = Constant.driverTextArray Common.FunctionCall,
      driverMobileNumber = (Just state.data.driverMobileNumber),
      rightButtonText = getString LOGOUT,
      logoutVisibility = true
     }
  in stepsHeaderConfig'

fileCameraLayoutConfig:: ST.AddVehicleDetailsScreenState -> PopUpModal.Config
fileCameraLayoutConfig state = let
    config = PopUpModal.config
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

activateRcButtonConfig :: ST.AddVehicleDetailsScreenState -> PrimaryButton.Config
activateRcButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = getString case state.props.multipleRCstatus of
                                        COMPLETED -> ACTIVATE_RC
                                        _ -> OKAY}
      , width = MATCH_PARENT
      , cornerRadius = 8.0
      , height = V 48
      , margin = MarginHorizontal 16 16 
      , id = "AddRCPrimaryButton"
      }
  in primaryButtonConfig'