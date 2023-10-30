{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Screens.Types as ST
import Components.PrimaryEditText as PrimaryEditText
import Components.StepsHeaderModal as StepsHeaderModal
import Styles.Colors as Color
import Font.Style as FontStyle
import Data.Maybe (Maybe(..))
import JBridge as JB
import Common.Types.App
import Font.Size as FontSize
import Engineering.Helpers.Commons as EHC
import Prelude (not)

primaryButtonViewConfig :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
primaryButtonViewConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString CONTINUE) }
      , id = "PrimaryButtonMobileNumber"
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.6
      , height = (V 60)
      , cornerRadius = 0.0
      , margin = (Margin 0 0 0 0)
      }
  in primaryButtonConfig'

stepsHeaderModalConfig :: ST.EnterMobileNumberScreenState -> StepsHeaderModal.Config
stepsHeaderModalConfig state = let
    config = StepsHeaderModal.config 0
    stepsHeaderConfig' = config 
     {
      stepsViewVisibility = false,
      profileIconVisibility = false,
      driverNumberVisibility = false,
      logoutVisibility = false,
      activeIndex = 0
     }
  in stepsHeaderConfig'

mobileNumberButtonConfig :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
mobileNumberButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString CONTINUE) }
      , id = "PrimaryButtonMobileNumber"
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.4
      , margin = (Margin 0 0 0 0 )
      , enableLoader = (JB.getBtnLoader "PrimaryButtonMobileNumber")
      }
  in primaryButtonConfig'

mobileNumberEditTextConfig :: ST.EnterMobileNumberScreenState -> PrimaryEditText.Config
mobileNumberEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        {
            color = Color.black800
          , singleLine = true
          , pattern = Just "[0-9]*,10"
          -- , fontStyle = FontStyle.bold LanguageStyle
          -- , textSize = FontSize.a_16
          , margin = MarginHorizontal 10 10
          , focused = state.props.mobileNumberEditFocused
          --, text = state.data.mobileNumber
        }
      , background = Color.white900
      , topLabel
        { 
          -- textSize = FontSize.a_14
          text = "Enter your Mobile number"
        , color = Color.black800
        -- , fontStyle = FontStyle.semiBold LanguageStyle
        , alpha = 0.8
        }
      , id = (EHC.getNewIDWithTag "EnterMobileNumberEditText")
      , type = "number"
      , height = V 54
      , margin = MarginTop 30
      , showErrorLabel = (not state.props.isValid)
      , errorLabel
        { text = (getString INVALID_MOBILE_NUMBER)
        }
      , showConstantField = true
      , constantField { 
          --color = if state.props.mNumberEdtFocused then Color.black800 else Color.grey900 
        --  textSize = FontSize.a_16
         padding = PaddingBottom 1
        }
      }
    in primaryEditTextConfig'