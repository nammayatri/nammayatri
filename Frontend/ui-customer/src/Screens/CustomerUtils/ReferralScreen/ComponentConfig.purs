{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReferralScreen.ComponentConfig where

import Common.Types.App
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color

continueButtonConfig :: ST.ReferralScreenState -> PrimaryButton.Config
continueButtonConfig state =
  PrimaryButton.config
    { textConfig { text = (getString CONTINUE) }
    , isClickable = state.btnActive
    , alpha = if state.btnActive then 1.0 else 0.4
    , id = "ReferralCodeModelContinue"
    , margin = (MarginTop 16)
    }

goToHomeButtonConfig :: ST.ReferralScreenState -> PrimaryButton.Config
goToHomeButtonConfig state =
  PrimaryButton.config
    { textConfig { text = (getString GO_TO_HOME__) }
    , id = "GoToHomePrimaryButton"
    , margin = (Margin 0 0 0 0)
    }

primaryEditTextConfig :: ST.ReferralScreenState -> PrimaryEditText.Config
primaryEditTextConfig state =
  let
    config = PrimaryEditText.config

    primaryEditTextConfig' =
      config
        { editText
          { color = Color.black800
          , placeholder = (getString SIX_DIGIT_REFERRAL_CODE)
          , singleLine = true
          , fontStyle = FontStyle.bold LanguageStyle
          , textSize = FontSize.a_16
          , pattern = Just "[0-9]*,6"
          , placeholderColor = Color.black600
          }
        , background = Color.white900
        , type = "number"
        , topLabel
          { visibility = GONE
          }
        , margin = (Margin 0 0 0 0)
        , cornerRadius = 8.0
        , showErrorLabel = state.isInvalidCode
        , errorLabel
          { text = (getString INVALID_CODE_PLEASE_RE_ENTER)
          , fontStyle = FontStyle.medium LanguageStyle
          , textSize = FontSize.a_14
          , color = Color.red
          }
        }
  in
    primaryEditTextConfig'

genericHeaderConfig :: ST.ReferralScreenState -> GenericHeader.Config
genericHeaderConfig _ =
  GenericHeader.config
    { height = WRAP_CONTENT
    , prefixImageConfig
      { height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = (Margin 12 12 12 12)
      }
    , textConfig
      { text = (getString HAVE_REFERRAL_CODE)
      , textSize = FontSize.a_18
      , color = Color.black900
      , fontStyle = FontStyle.semiBold LanguageStyle
      }
    , suffixImageConfig
      { visibility = GONE
      }
    , padding = (Padding 0 5 0 5)
    }
