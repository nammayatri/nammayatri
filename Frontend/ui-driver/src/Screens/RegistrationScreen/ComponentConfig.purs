{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.ComponentConfig where

import Language.Strings
import PrestoDOM

import Common.Types.App as Common
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader as GenericHeader
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Data.Maybe (Maybe(..), isJust)
import Font.Style as FontStyle
import Language.Types (STR(..))
import Resource.Constants as Constant
import Helpers.Utils as HU
import Screens.Types as ST
import Styles.Colors as Color
import Storage ( getValueToLocalStore , KeyStore(..))
import Components.InAppKeyboardModal as InAppKeyboardModal
import Prelude ((<), not, ($), (&&))
import Data.String as DS
import Mobility.Prelude
import Prelude ((==))
import Components.OptionsMenu as OptionsMenuConfig
import PrestoDOM.Types.DomAttributes as PTD
import Components.BottomDrawerList as BottomDrawerList

primaryButtonConfig :: ST.RegistrationScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { text = getString if state.props.manageVehicle then ADD_VEHICLE else COMPLETE_REGISTRATION }
      , width = MATCH_PARENT
      , height = V 48
      , id = "RegistrationScreenButton"
      }
  in primaryButtonConfig'

appOnboardingNavBarConfig :: ST.RegistrationScreenState -> AppOnboardingNavBar.Config
appOnboardingNavBarConfig state = 
  AppOnboardingNavBar.config
  { prefixImageConfig = AppOnboardingNavBar.config.prefixImageConfig{visibility = GONE},
    genericHeaderConfig = genericHeaderConfig state,
    appConfig = state.data.config,
    headerTextConfig = AppOnboardingNavBar.config.headerTextConfig{
      color = state.data.config.themeColors.onboardingHeaderTextColor,
      text = case state.data.vehicleCategory of
              _ | state.props.manageVehicle -> getString ADD_VEHICLE
              Just ST.CarCategory -> getString REGISTER_YOUR_CAR
              Just ST.AutoCategory -> getString REGISTER_YOUR_AUTO
              Just ST.BikeCategory -> getString REGISTER_YOUR_BIKE
              Just ST.AmbulanceCategory -> getString REGISTER_YOUR_AMBULANCE
              Just ST.TruckCategory -> getString REGISTER_YOUR_TRUCK
              Just ST.BusCategory ->  "Register your Bus"
              Just ST.UnKnown -> getString REGISTRATION
              Nothing -> getString REGISTRATION
      },
    rightButton = AppOnboardingNavBar.config.rightButton{
      text = getString HELP_FAQ,
      color = state.data.config.themeColors.onboardingHeaderTextColor
      },
    navBarOpen = state.props.menuOptions
  }

changeVehicleConfig :: Common.LazyCheck -> PopUpModal.Config
changeVehicleConfig _ = let 
  config' = PopUpModal.config
  popUpConfig' = config' {
    primaryText {text = getString DO_YOU_WANT_TO_CHANGE_VT , margin = MarginBottom 20},
    secondaryText {visibility = GONE},
    buttonLayoutMargin = (MarginBottom 40),
    padding = (Padding 16 16 16 0),
    backgroundClickable = true,
    dismissPopup = true,
    option1 {
      text = getString YES_CHANGE_VEHICLE ,
      color = Color.black700,
      textStyle = FontStyle.SubHeading1,
      strokeColor = Color.white900,
      width = MATCH_PARENT,
      height = WRAP_CONTENT,
      background = Color.blue600,
      margin = (MarginBottom 12),
      padding = (PaddingVertical 16 16),
      enableRipple = true
      },
    option2 {
      text = (getString CANCEL),
      color = Color.black700,
      textStyle = FontStyle.SubHeading1,
      height = WRAP_CONTENT,
      strokeColor = Color.white900,
      width = MATCH_PARENT,
      padding = PaddingVertical 16 16,
      margin = (MarginBottom 0),
      background = Color.blue600,
      enableRipple = true
      },
    optionButtonOrientation = "VERTICAL"
  }
  in popUpConfig'


vehicleMismatchConfig :: ST.RegistrationScreenState -> PopUpModal.Config
vehicleMismatchConfig state = PopUpModal.config {
    gravity = CENTER,
    backgroundClickable = false,
    optionButtonOrientation = "VERTICAL",
    buttonLayoutMargin = Margin 16 0 16 20,
    margin = MarginHorizontal 25 25, 
    primaryText {
      text = getString VEHICLE_TYPE_MISMATCH
    , textStyle = FontStyle.Heading2
    , margin = Margin 16 0 16 10},
    secondaryText{
      text = getString UPLOADED_DOC_DOESNT_MATCH
    , textStyle = FontStyle.Body5
    , margin = Margin 16 0 16 15 },
    option1 {
      text = getString CHANGE_VEHICLE_TYPE
    , color = Color.yellow900
    , background = Color.black900
    , strokeColor = Color.transparent
    , textStyle = FontStyle.SubHeading1
    , width = MATCH_PARENT
    },
    option2 {
    text = getString UPLOAD_DIFFERENT_RC,
    margin = MarginHorizontal 16 16,
    color = Color.black650,
    background = Color.white900,
    strokeColor = Color.white900,
    width = MATCH_PARENT
  },
    cornerRadius = PTD.Corners 15.0 true true true true,
    coverImageConfig {
      imageUrl = HU.fetchImage HU.FF_ASSET if state.data.vehicleCategory == Just ST.CarCategory then "ny_ic_car_warning" else "ny_ic_auto_warning"
    , visibility = VISIBLE
    , margin = Margin 16 20 16 24
    , width = V 296
    , height = V 190
    }
  }

logoutPopUp :: Common.LazyCheck -> PopUpModal.Config
logoutPopUp  dummy = let 
  config' = PopUpModal.config
  popUpConfig' = config' {
    primaryText {text = (getString LOGOUT)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT)},
    buttonLayoutMargin = (MarginBottom 40),
    padding = (Padding 16 16 16 0),
    backgroundClickable = true,
    dismissPopup = true,
    option1 {
      text = (getString LOGOUT),
      color = Color.black700,
      textStyle = FontStyle.SubHeading1,
      strokeColor = Color.white900,
      width = MATCH_PARENT,
      height = WRAP_CONTENT,
      background = Color.blue600,
      margin = (MarginBottom 12),
      padding = (PaddingVertical 16 16),
      enableRipple = true
      },
    option2 {
      text = (getString CANCEL),
      color = Color.black700,
      textStyle = FontStyle.SubHeading1,
      height = WRAP_CONTENT,
      strokeColor = Color.white900,
      width = MATCH_PARENT,
      padding = PaddingVertical 16 16,
      margin = (MarginBottom 0),
      background = Color.blue600,
      enableRipple = true
      },
    optionButtonOrientation = "VERTICAL"
  }
  in popUpConfig'

genericHeaderConfig :: ST.RegistrationScreenState -> GenericHeader.Config
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
      , margin = (Margin 0 5 5 5)
      }
    , padding = (PaddingVertical 5 5)
    , textConfig {
        text = (getValueToLocalStore MOBILE_NUMBER_KEY)
      , color = state.data.config.themeColors.onboardingHeaderTextColor
      , margin = MarginHorizontal 5 5 
      , textStyle = FontStyle.Body1
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

enterReferralStateConfig :: ST.RegistrationScreenState -> InAppKeyboardModal.InAppKeyboardModalState
enterReferralStateConfig state = InAppKeyboardModal.config{
      otpIncorrect = not state.props.isValidReferralCode,
      inputTextConfig {
        text = state.data.referralCode,
        focusIndex = state.props.enterOtpFocusIndex
        , textStyle = FontStyle.Heading1
      },
      headingConfig {
        text = getString ENTER_REFERRAL_CODE
      },
      errorConfig {
        text = if state.props.isValidReferralCode then "" else getString INVALID_REFERRAL_CODE,
        visibility = boolToVisibility $ not state.props.isValidReferralCode
      },
      imageConfig {
        alpha = if(DS.length state.data.referralCode < 6) then 0.3 else 1.0
      },
      textBoxConfig{
        textBoxesArray = [1,2,3,4,5,6],
        width = V 36,
        height = V 44
      },
      modalType = ST.OTP
    }

continueButtonConfig :: ST.RegistrationScreenState -> PrimaryButton.Config
continueButtonConfig state = 
  let isEnabled = isJust state.props.selectedVehicleIndex
  in
  PrimaryButton.config
  { textConfig{ text = getString CONTINUE}
  , width = MATCH_PARENT
  , margin = Margin 16 16 16 16
  , height = V 48
  , alpha = if isEnabled then 1.0 else 0.5
  , isClickable = if isEnabled then true else false
  , id = "RegistrationContinueButton"
  }

optionsMenuConfig :: ST.RegistrationScreenState -> OptionsMenuConfig.Config
optionsMenuConfig state = OptionsMenuConfig.config {
  menuItems = [
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_getting_started_and_faq", textdata : "FAQs", action : "faqs", isVisible : true, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_phone_unfilled", textdata : getString CONTACT_SUPPORT, action : "contact_support", isVisible : true, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_language", textdata : getString CHANGE_LANGUAGE_STR, action : "change_language", isVisible : not state.props.manageVehicle, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_parallel_arrows_horizontal", textdata : getString CHANGE_VEHICLE, action : "change_vehicle", isVisible : (isJust state.data.vehicleCategory) && not state.props.manageVehicle && state.data.config.enableChangeVehicleType, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_logout_grey", textdata : getString LOGOUT, action : "logout", isVisible :  not state.props.manageVehicle, color : Color.black800}
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

bottomDrawerListConfig :: ST.RegistrationScreenState -> BottomDrawerList.Config
bottomDrawerListConfig state = BottomDrawerList.config {
  animState = state.props.contactSupportModal,
  titleText = getString CONTACT_SUPPORT_VIA,
  itemList = [
    {prefixImg : "ny_ic_whatsapp_black", title : "Whatsapp", desc : getString YOU_CAN_SHARE_SCREENSHOT , postFixImg : "ny_ic_chevron_right", visibility : state.data.cityConfig.registration.whatsappSupport, identifier : "whatsapp"},
    {prefixImg : "ny_ic_direct_call", title : getString CALL, desc : getString PLACE_A_CALL, postFixImg : "ny_ic_chevron_right", visibility : state.data.cityConfig.registration.callSupport, identifier : "call"}
  ]
}