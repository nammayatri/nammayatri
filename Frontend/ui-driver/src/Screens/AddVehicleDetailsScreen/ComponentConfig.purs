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
import Components.GenericHeader as GenericHeader
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Data.String as DS
import Font.Size as FontSize
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resource.Constants as Constant
import Screens.AddVehicleDetailsScreen.Controller (validateRegistrationNumber)
import Screens.Types (StageStatus(..))
import Screens.Types as ST
import Styles.Colors as Color
import Helpers.Utils as HU
import Storage (KeyStore(..), getValueToLocalStore)
import Font.Style as FontStyle
import ConfigProvider
import Mobility.Prelude

primaryButtonConfig :: ST.AddVehicleDetailsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    feature = (getAppConfig appConfig).feature
    imageUploadCondition = state.props.openHowToUploadManual && not state.data.cityConfig.uploadRCandDL
    rcMatch = caseInsensitiveCompare state.data.vehicle_registration_number state.data.reEnterVehicleRegistrationNumber
    activate = (( rcMatch || (not state.data.cityConfig.uploadRCandDL)) && 
                -- (state.data.dateOfRegistration /= Just "") && 
                state.data.vehicle_registration_number /= "" &&
                ((DS.length state.data.vehicle_registration_number >= 2) && validateRegistrationNumber (DS.take 2 state.data.vehicle_registration_number)))
    primaryButtonConfig' = config 
      { textConfig{ text = if isJust state.data.dateOfRegistration then getString CONFIRM 
                           else if state.props.openHowToUploadManual then getString UPLOAD_PHOTO
                           else getString UPLOAD_REGISTRATION_CERTIFICATE}
      , width = MATCH_PARENT
      , height = (V 50)
      , background = Color.black900 
      , margin = if imageUploadCondition then Margin 15 0 15 10 else Margin 15 0 15 30
      , cornerRadius = 6.0
      , alpha = if activate then 1.0 else 0.8
      , isClickable = activate
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
        text = getString TAKE_A_PHOTO
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
        text = getString GALLERY
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

genericHeaderConfig :: ST.AddVehicleDetailsScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , background = state.data.config.primaryBackground
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = HU.fetchImage HU.FF_ASSET "ic_new_avatar"
      , height = (V 25)
      , width = (V 25)
      , margin = (Margin 12 5 5 5)
      }
    , padding = (PaddingVertical 5 5)
    , textConfig {
        text = (getValueToLocalStore MOBILE_NUMBER_KEY)
      , color = Color.white900
      , margin = MarginHorizontal 5 5 
      , textStyle = FontStyle.Body1
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

appOnboardingNavBarConfig :: ST.AddVehicleDetailsScreenState -> AppOnboardingNavBar.Config
appOnboardingNavBarConfig state = 
  AppOnboardingNavBar.config
  { genericHeaderConfig = genericHeaderConfig state,
    appConfig = state.data.config,
    headerTextConfig = AppOnboardingNavBar.config.headerTextConfig
      { text = if state.props.openHowToUploadManual
                then getString UPLOAD_REGISTRATION_CERTIFICATE_STR 
                else getString VEHICLE_REGISTRATION_DETAILS
      }
  }