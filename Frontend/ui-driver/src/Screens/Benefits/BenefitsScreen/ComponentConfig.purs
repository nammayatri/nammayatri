{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Benefits.BenefitsScreen.ComponentConfig where

import Common.Types.App
import Data.Array as DA
import Data.Maybe
import Data.String
import Helpers.Utils
import Language.Strings
import Prelude
import PrestoDOM
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.Views as PrimaryEditText
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.ReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import JBridge as JB
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Helpers.Utils as HU
import Components.OptionsMenu as OptionsMenuConfig
import Common.Types.App as Common
import Components.BottomDrawerList as BottomDrawerList

--------------------------------------------------- genericHeaderConfig -----------------------------------------------------
genericHeaderConfig :: ST.BenefitsScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
  bothReferralNotEnabled = not (cityConfig.showDriverReferral || state.data.config.enableDriverReferral || cityConfig.showCustomerReferral || state.data.config.enableCustomerReferral)
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left"
      , margin = Margin 12 12 12 12
      , visibility = GONE
      }
    , padding = Padding 16 16 0 16
    , textConfig {
        text = case bothReferralNotEnabled of
          true -> getString RIDE_LEADERBOARD
          false -> getString REFERRAL_BONUS
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

trainingsHeaderConfig :: ST.BenefitsScreenState -> GenericHeader.Config
trainingsHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , background = Color.transparent
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = HU.fetchImage HU.FF_ASSET "ic_new_avatar"
      , height = V 25
      , width = V 25
      , margin = Margin 12 5 5 5
      }
    , padding = PaddingVertical 5 5
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

primaryButtonConfig :: ST.BenefitsScreenState -> PrimaryButton.Config 
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
      { text = getString GO_BACK
      , color = state.data.config.primaryTextColor
      }
      , margin = Margin 10 10 10 10
      , background = Color.black900
      , height = V 48
      , width = MATCH_PARENT
      , id = "BenefitsScreenPrimaryButton"
      }
  in primaryButtonConfig'

appOnboardingNavBarConfig :: ST.BenefitsScreenState -> AppOnboardingNavBar.Config
appOnboardingNavBarConfig state = 
  AppOnboardingNavBar.config
  { prefixImageConfig = AppOnboardingNavBar.config.prefixImageConfig{ 
        image = state.data.config.themeColors.defaultBackButton,
        visibility = VISIBLE,
        clickable = not $ state.props.menuOptions
    },
    genericHeaderConfig = trainingsHeaderConfig state,
    appConfig = state.data.config,
    headerTextConfig = AppOnboardingNavBar.config.headerTextConfig{
      color = state.data.config.themeColors.onboardingHeaderTextColor,
      text = getString TRAININGS
      },
    rightButton = AppOnboardingNavBar.config.rightButton{
      text = getString HELP_FAQ,
      color = state.data.config.themeColors.onboardingHeaderTextColor
      },
    navBarOpen = state.props.menuOptions
  }


optionsMenuConfig :: ST.BenefitsScreenState -> OptionsMenuConfig.Config
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

bottomDrawerListConfig :: ST.BenefitsScreenState -> BottomDrawerList.Config
bottomDrawerListConfig state = BottomDrawerList.config {
  animState = state.props.contactSupportModal,
  titleText = getString CONTACT_SUPPORT_VIA,
  itemList = [
    {prefixImg : "ny_ic_whatsapp_black", title : "Whatsapp", desc : getString YOU_CAN_SHARE_SCREENSHOT , postFixImg : "ny_ic_chevron_right", visibility : true, identifier : "whatsapp"},
    {prefixImg : "ny_ic_direct_call", title : getString CALL, desc : getString PLACE_A_CALL, postFixImg : "ny_ic_chevron_right", visibility : true, identifier : "call"}
  ]
}

continueButtonConfig :: ST.BenefitsScreenState -> PrimaryButton.Config
continueButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
        { text = getString CONTINUE } 
        , margin = Margin 16 16 16 16
        , id = "BenefitsScreenPrimaryButton"
        , isClickable = DA.length state.data.moduleList.remaining == 0
      }
  in primaryButtonConfig'