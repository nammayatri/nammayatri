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
import Data.Maybe (Maybe(..))
import Font.Style as FontStyle
import Language.Types (STR(..))
import Resource.Constants as Constant
import Helpers.Utils as HU
import Screens.Types as ST
import Styles.Colors as Color
import Storage ( getValueToLocalStore , KeyStore(..))
import Components.InAppKeyboardModal as InAppKeyboardModal
import Prelude ((<), not, ($))
import Data.String as DS
import Mobility.Prelude

primaryButtonConfig :: ST.RegistrationScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString COMPLETE_REGISTRATION)}
      , width = MATCH_PARENT
      , background = Color.black900
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      , height = (V 60)
      , id = "RegistrationScreenButton"
      }
  in primaryButtonConfig'

appOnboardingNavBarConfig :: ST.RegistrationScreenState -> AppOnboardingNavBar.Config
appOnboardingNavBarConfig state = 
  AppOnboardingNavBar.config
  { prefixImageConfig = AppOnboardingNavBar.config.prefixImageConfig{visibility = GONE},
    genericHeaderConfig = genericHeaderConfig state,
    appConfig = state.data.config,
    headerTextConfig = AppOnboardingNavBar.config.headerTextConfig{text = (getString REGISTRATION)}
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
    , background = state.data.config.primaryBackground
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
      , color = Color.white900
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