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
import Components.GenericHeader as GenericHeader
import Components.AppOnboardingNavBar as AppOnboardingNavBar
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
import Helpers.Utils as HU
import Data.Array as DA
import Storage ( getValueToLocalStore , KeyStore(..))
import ConfigProvider
import Mobility.Prelude
import Components.OptionsMenu as OptionsMenuConfig
import Components.BottomDrawerList as BottomDrawerList

------------------------------ primaryButtonConfig --------------------------------
primaryButtonConfig :: ST.UploadDrivingLicenseState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    imageUploadCondition = state.props.openHowToUploadManual && not state.data.cityConfig.uploadRCandDL
    dobNotEmpty = not $ DS.null state.data.dob
    id = "UploadDrivingLicenseButton"
    driverLicenseLengthValid = DS.length state.data.driver_license_number >= 9
    dateOfIssueNotEmpty = state.data.dateOfIssue /= Just ""
    isDriverInfoValid = dobNotEmpty 
                        && driverLicenseLengthValid
                        && dateOfIssueNotEmpty
    primaryButtonConfig' = config 
      { textConfig{ text = if isJust state.data.dateOfIssue then getString CONFIRM 
                           else if state.props.openHowToUploadManual then getString UPLOAD_PHOTO
                           else getString UPLOAD_DRIVING_LICENSE
      }
      , width = MATCH_PARENT
      , margin = if imageUploadCondition then Margin 15 0 15 10 else Margin 15 0 15 30
      , cornerRadius = 6.0
      , height = V 50
      , isClickable = isDriverInfoValid
      , alpha = if isDriverInfoValid then 1.0 else 0.8
      }
  in primaryButtonConfig'

------------------------------ primaryEditTextConfig --------------------------------
primaryEditTextConfig :: ST.UploadDrivingLicenseState -> PrimaryEditText.Config
primaryEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[A-Za-z0-9/-]*,25"
          , placeholder = getString ENTER_DL_NUMBER
          , capsLock = true
        }
      , topLabel
        { 
        text = getString DRIVING_LICENSE_NUMBER
        , color = Color.greyTextColor
        }
      , margin = MarginBottom 15
      , background = state.data.config.themeColors.radioInactiveBackground
      , id = EHC.getNewIDWithTag "EnterDrivingLicenseEditText"
      , stroke = ("1," <> state.data.config.themeColors.editTextNormalStroke)
      }
    in primaryEditTextConfig'

------------------------------ primaryEditTextConfigReEnterDl --------------------------------
primaryEditTextConfigReEnterDl :: ST.UploadDrivingLicenseState -> PrimaryEditText.Config
primaryEditTextConfigReEnterDl state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[A-Za-z0-9/-]*,25"
          , placeholder = getString ENTER_DL_NUMBER
          , capsLock = true
          , color = Color.black800
        }
      , stroke = if (DS.toLower(state.data.driver_license_number) /= DS.toLower(state.data.reEnterDriverLicenseNumber) && state.data.reEnterDriverLicenseNumber /= "") then ("1," <> Color.red) else ("1," <> state.data.config.themeColors.editTextNormalStroke
)
      , topLabel
        { text = getString RE_ENTER_DRIVING_LICENSE_NUMBER
        , color = Color.greyTextColor
        }
      , margin = MarginBottom 15
      , background = state.data.config.themeColors.radioInactiveBackground
      , id = EHC.getNewIDWithTag "ReEnterDrivingLicenseEditText"
      }
    in primaryEditTextConfig'


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
        , margin = MarginHorizontal 16 16
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

appOnboardingNavBarConfig :: ST.UploadDrivingLicenseState -> AppOnboardingNavBar.Config
appOnboardingNavBarConfig state = 
  AppOnboardingNavBar.config
  { genericHeaderConfig = genericHeaderConfig state,
    appConfig = state.data.config,
    headerTextConfig = AppOnboardingNavBar.config.headerTextConfig
              { color = state.data.config.themeColors.onboardingHeaderTextColor,
                text = getString DRIVING_LICENSE
              },
    rightButton = AppOnboardingNavBar.config.rightButton{
      text = getString HELP_FAQ,
      color = state.data.config.themeColors.onboardingHeaderTextColor
      },
    navBarOpen = state.props.menuOptions,
    prefixImageConfig = AppOnboardingNavBar.config.prefixImageConfig{ image = state.data.config.themeColors.defaultBackButton }
  }

genericHeaderConfig :: ST.UploadDrivingLicenseState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , background = Color.transparent
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = HU.fetchImage HU.FF_ASSET "ic_new_avatar"
      , height = (V 25)
      , width = (V 25)
      , margin = (Margin 12 5 5 5)
      }
    , padding = (PaddingVertical 5 5)
    , textConfig {
        text = if DA.any (_ == getValueToLocalStore DRIVER_NAME) ["", "__failed"] then getValueToLocalStore MOBILE_NUMBER_KEY else getValueToLocalStore DRIVER_NAME
      , color = state.data.config.themeColors.onboardingHeaderTextColor
      , margin = MarginHorizontal 5 5 
      , textStyle = FontStyle.Body1
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

optionsMenuConfig :: ST.UploadDrivingLicenseState -> OptionsMenuConfig.Config
optionsMenuConfig state = OptionsMenuConfig.config {
  menuItems = [
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_getting_started_and_faq", textdata : "FAQs", action : "faqs", isVisible : true, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_phone_unfilled", textdata : getString CONTACT_SUPPORT, action : "contact_support", isVisible : true, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_language", textdata : getString CHANGE_LANGUAGE_STR, action : "change_language", isVisible : true, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_parallel_arrows_horizontal", textdata : getString CHANGE_VEHICLE, action : "change_vehicle", isVisible : state.data.config.enableChangeVehicleType, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_logout_grey", textdata : getString LOGOUT, action : "logout", isVisible :  true, color : Color.black800}
  ],
  backgroundColor = Color.blackLessTrans,
  menuBackgroundColor = Color.white900,
  gravity = RIGHT,
  menuExpanded = true,
  width = WRAP_CONTENT,
  marginRight = 16,
  itemHeight = V 50,
  itemPadding = Padding 16 16 16 16,
  cornerRadius = 4.0,
  enableAnim = true
}

bottomDrawerListConfig :: ST.UploadDrivingLicenseState -> BottomDrawerList.Config
bottomDrawerListConfig state = BottomDrawerList.config {
  animState = state.props.contactSupportModal,
  titleText = getString CONTACT_SUPPORT_VIA,
  itemList = [
    {prefixImg : "ny_ic_whatsapp_black", title : "Whatsapp", desc : getString YOU_CAN_SHARE_SCREENSHOT , postFixImg : "ny_ic_chevron_right", visibility : state.data.cityConfig.registration.whatsappSupport, identifier : "whatsapp"},
    {prefixImg : "ny_ic_direct_call", title : getString CALL, desc : getString PLACE_A_CALL, postFixImg : "ny_ic_chevron_right", visibility : state.data.cityConfig.registration.callSupport, identifier : "call"}
  ]
}