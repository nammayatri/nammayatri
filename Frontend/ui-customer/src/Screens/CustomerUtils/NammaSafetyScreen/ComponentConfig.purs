{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB 
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (not, (<>), ($), (==))
import PrestoDOM (Length(..), Margin(..), Visibility(..), Padding(..), Gravity(..), textFromHtml)
import Screens.Types(NammaSafetyScreenState, Stage(..))
import Styles.Colors as Color
import Common.Types.App

-- mobileNumberButtonConfig :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
-- mobileNumberButtonConfig state = let 
--     config = PrimaryButton.config
--     primaryButtonConfig' = config 
--       { textConfig{ text = (getString CONTINUE) }
--       , id = "PrimaryButtonMobileNumber"
--       , isClickable = state.props.btnActiveMobileNumber
--       , alpha = if state.props.btnActiveMobileNumber then 1.0 else 0.4
--       , margin = (Margin 0 0 0 0 )
--       , enableLoader = (JB.getBtnLoader "PrimaryButtonMobileNumber")
--       }
--   in primaryButtonConfig'

startNSOnboardingButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
startNSOnboardingButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = (getButtonString state.props.currentStage) }
    , id = "PrimaryButtonOTP"
    , isClickable = true
    , visibility = if state.props.showOnboarding == true then VISIBLE else GONE
    -- , alpha = if state.props.btnActiveOTP then 1.0 else 0.4
    , margin = (Margin 16 0 16 0 )
    -- , enableLoader = (JB.getBtnLoader "PrimaryButtonOTP")
    -- , layoutGravity = BOTTOM
    }
  
skipNSOnboardingButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
skipNSOnboardingButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = (getString SKIP) , color = Color.black700}
    , id = "PrimaryButtonOTP"
    , isClickable = true
    -- , alpha = if state.props.btnActiveOTP then 1.0 else 0.4
    , margin = (Margin 16 8 16 0 )
    -- , enableLoader = (JB.getBtnLoader "PrimaryButtonOTP")
    -- , gravity = CENTER_BOTTOM
    , background = Color.white900
    , stroke = ("1," <> Color.black700)
    }

editEmergencyContactsBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
editEmergencyContactsBtnConfig state = 
  PrimaryButton.config
    { textConfig{ text = (getString EDIT) , color = Color.blue900
    -- , textSize = FontSize.a_14, fontStyle = FontStyle.semiBold LanguageStyle
    }
    , id = "PrimaryButtonOTP"
    , isClickable = true
    , height = MATCH_PARENT
    , width = WRAP_CONTENT
    -- , alpha = if state.props.btnActiveOTP then 1.0 else 0.4
    , margin = (MarginLeft 9)
    -- , enableLoader = (JB.getBtnLoader "PrimaryButtonOTP")
    , gravity = CENTER
    , background = Color.white900
    }

cancelSOSBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
cancelSOSBtnConfig state = 
  PrimaryButton.config
    { textConfig{ text = "Cancel SOS" , color = Color.black900
    -- , textSize = FontSize.a_16, fontStyle = FontStyle.bold LanguageStyle
    }
    , id = "PrimaryButtonOTP"
    , isClickable = true
    , height = WRAP_CONTENT
    , width = MATCH_PARENT
    -- , alpha = if state.props.btnActiveOTP then 1.0 else 0.4
    -- , margin = (MarginLeft 9)
    -- , enableLoader = (JB.getBtnLoader "PrimaryButtonOTP")
    , gravity = CENTER
    , background = Color.white900
    }



getButtonString :: Stage -> String
getButtonString stage = case stage of
  SetTriggerCustomerSupport ->  "Add Emergency Contacts"
  SetPersonalSafetySettings ->  "Finish Setup"
  _ ->  "Enable Now"

-- mobileNumberEditTextConfig :: ST.EnterMobileNumberScreenState -> PrimaryEditText.Config
-- mobileNumberEditTextConfig state = let 
--     config = PrimaryEditText.config
--     primaryEditTextConfig' = config
--       { editText
--         {
--             color = Color.black800
--           , singleLine = true
--           , pattern = Just "[0-9]*,10"
--           , fontStyle = FontStyle.bold LanguageStyle
--           , textSize = FontSize.a_16
--           , margin = MarginHorizontal 10 10
--           , focused = state.props.mNumberEdtFocused
--           , text = state.props.editTextVal
--         }
--       , background = Color.white900
--       , topLabel
--         { textSize = FontSize.a_12
--         , text = (getString ENTER_YOUR_MOBILE_NUMBER)
--         , color = Color.black800
--         , fontStyle = FontStyle.semiBold LanguageStyle
--         , alpha = 0.8
--         }
--       , id = (EHC.getNewIDWithTag "EnterMobileNumberEditText")
--       , type = "number"
--       , height = V 54
--       , margin = MarginTop 30
--       , showErrorLabel = (not state.props.isValidMobileNumber)
--       , errorLabel
--         { text = (getString INVALID_MOBILE_NUMBER)
--         }
--       , showConstantField = true
--       , constantField { 
--           color = if state.props.mNumberEdtFocused then Color.black800 else Color.grey900 
--         , textSize = FontSize.a_16
--         , padding = PaddingBottom 1
--         }
--       }
--     in primaryEditTextConfig'

-- otpEditTextConfig :: ST.EnterMobileNumberScreenState -> PrimaryEditText.Config
-- otpEditTextConfig state = let 
--     config = PrimaryEditText.config
--     primaryEditTextConfig' = config
--       { editText
--         { color = Color.black800
--         , placeholder = (getString ENTER_4_DIGIT_OTP)
--         , singleLine = true
--         , pattern = Just "[0-9]*,4"
--         , margin = MarginHorizontal 10 10
--         , fontStyle = FontStyle.bold LanguageStyle
--         , textSize = FontSize.a_16
--         , letterSpacing = state.props.letterSpacing
--         , text = ""
--         , focused = state.props.otpEdtFocused
--         , gravity = LEFT
--         }
--       , background = Color.white900
--       , margin = (Margin 0 30 0 20)
--       , topLabel
--         { textSize = FontSize.a_12
--         , text = (getString LOGIN_USING_THE_OTP_SENT_TO) <> " +91 " <> state.data.mobileNumber
--         , color = Color.black800
--         , fontStyle = FontStyle.regular LanguageStyle
--         , alpha = 0.8
--         } 
--       , id = (EHC.getNewIDWithTag "EnterOTPNumberEditText")
--       , type = "number"
--       , height = V 54
--       , errorLabel {
--             text = (getString WRONG_OTP),
--             visibility = VISIBLE,
--             textSize = FontSize.a_12,
--             fontStyle = FontStyle.bold LanguageStyle,
--             alpha = 0.8
--       },
--       width = MATCH_PARENT,
--       showErrorLabel = state.props.wrongOTP, 
--       stroke = if state.props.wrongOTP then ("1," <> Color.warningRed) else ("1," <> Color.borderColorLight)
--       }
--     in primaryEditTextConfig'

genericHeaderConfig :: String -> NammaSafetyScreenState -> GenericHeader.Config 
genericHeaderConfig title state = 
  GenericHeader.config
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , background = Color.white900
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = (Margin 12 14 12 12)
      } 
    , textConfig {
        text = title
      -- , textSize = FontSize.a_18
      , color = Color.black800
      -- , fontStyle = FontStyle.semiBold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 5)
    }
  









  
