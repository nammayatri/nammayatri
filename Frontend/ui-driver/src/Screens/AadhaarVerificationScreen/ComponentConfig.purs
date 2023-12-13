{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.AadhaarVerificationScreen.ComponentConfig where

import Language.Strings
import PrestoDOM
import Common.Types.App (LazyCheck(..))
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Array (any)
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import Prelude (not, (/=), (==))
import Screens.Types (AadhaarStage(..))
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonViewConfig :: ST.AadhaarVerificationScreenState -> PrimaryButton.Config
primaryButtonViewConfig state =
  PrimaryButton.config
    { textConfig { text = if state.props.currentStage == AadhaarDetails then (getString SUBMIT) else (getString NEXT) }
    , id = "PrimaryButtonMobileNumber"
    , isClickable =
      case state.props.currentStage of
        AadhaarDetails -> not any (_ == "") [ state.data.driverName, state.data.driverGender, state.data.driverDob ]
        _ -> state.props.btnActive
    , alpha =
      case state.props.currentStage of
        AadhaarDetails -> if (not any (_ == "") [ state.data.driverName, state.data.driverGender, state.data.driverDob ]) then 1.0 else 0.6
        _ -> if state.props.btnActive then 1.0 else 0.6
    , height = (V 60)
    , cornerRadius = 0.0
    , margin = (Margin 0 0 0 0)
    }

aadhaarNumberEditText :: ST.AadhaarVerificationScreenState -> PrimaryEditText.Config
aadhaarNumberEditText state =
  PrimaryEditText.config
    { id = getNewIDWithTag "EnterAadhaarNumberEditText"
    , editText
      { placeholder = "XXXX  XXXX  XXXX"
      , singleLine = true
      , gravity = CENTER_VERTICAL
      , pattern = Just "[0-9]*,14"
      , textStyle = FontStyle.SubHeading1
      , separator = " "
      , separatorRepeat = "4"
      }
    , topLabel
      { visibility = GONE
      }
    , showErrorLabel = state.props.showErrorAadhaar
    , errorLabel
      { text = "Not Valid"
      }
    , margin = Margin 0 0 0 0
    , type = "number"
    }

aadhaarNameEditText :: ST.AadhaarVerificationScreenState -> PrimaryEditText.Config
aadhaarNameEditText state =
  PrimaryEditText.config
    { id = getNewIDWithTag "EnterAadhaarNameEditText"
    , editText
      { placeholder = getString ENTER_AADHAAR_DETAILS
      , singleLine = true
      , gravity = CENTER_VERTICAL
      , pattern = Just "[a-zA-Z ]*,30"
      , textStyle = FontStyle.SubHeading1
      }
    , topLabel
      { visibility = VISIBLE
      , text = getString NAME
      , textStyle = FontStyle.Body3
      , color = Color.black900
      }
    , margin = MarginBottom 15
    }

aadhaarGenderEditText :: ST.AadhaarVerificationScreenState -> PrimaryEditText.Config
aadhaarGenderEditText state =
  PrimaryEditText.config
    { id = getNewIDWithTag "EnterAadhaarGenderEditText"
    , editText
      { placeholder = getString GENDER
      , singleLine = true
      , gravity = CENTER_VERTICAL
      , pattern = Just "[a-zA-Z ]*,30"
      , textStyle = FontStyle.SubHeading1
      }
    , topLabel
      { visibility = VISIBLE
      , text = getString GENDER
      , textStyle = FontStyle.Body3
      , color = Color.black900
      }
    , margin = MarginBottom 15
    }

aadhaarOTPEditText :: ST.AadhaarVerificationScreenState -> PrimaryEditText.Config
aadhaarOTPEditText state =
  PrimaryEditText.config
    { id = getNewIDWithTag "EnterAadhaarOTPEditText"
    , editText
      { placeholder = getString ENTER_SIX_DIGIT_OTP
      , singleLine = true
      , gravity = CENTER_VERTICAL
      , pattern = Just "[0-9]*,6"
      , textStyle = FontStyle.SubHeading1
      , letterSpacing = if state.data.otp == "" then PX 0.0 else PX 4.0
      }
    , topLabel
      { visibility = VISIBLE
      , text = getString OTP_SENT_TO_AADHAAR_NUMBER
      , textStyle = FontStyle.Body3
      , color = Color.black900
      }
    , margin = Margin 0 0 0 0
    , type = "number"
    }

logOutPopUpModelConfig :: ST.AadhaarVerificationScreenState -> PopUpModal.Config
logOutPopUpModelConfig state =
  PopUpModal.config
    { primaryText { text = (getString LOGOUT) }
    , secondaryText { text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT) }
    , option1 { text = (getString GO_BACK) }
    , option2 { text = (getString LOGOUT) }
    }
