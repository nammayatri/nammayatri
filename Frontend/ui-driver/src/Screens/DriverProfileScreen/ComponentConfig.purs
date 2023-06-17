{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.ComponentConfig where

import Components.PopUpModal as PopUpModal
import Components.GenericHeader as GenericHeader
import Components.PrimaryEditText as PrimaryEditText
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Screens.Types as ST
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Data.Maybe(Maybe(..))

logoutPopUp :: ST.DriverProfileScreenState -> PopUpModal.Config
logoutPopUp  state = let 
  config' = PopUpModal.config
  popUpConfig' = config' {
    primaryText {text = (getString LOGOUT)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT)},
    option1 {text = (getString GO_BACK)},
    option2 {text = (getString LOGOUT)}
  }
  in popUpConfig'

genericHeaderConfig :: ST.DriverProfileScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , height = (V 25)
      , width = (V 25)
      , margin = (Margin 16 16 16 16)
      } 
    , padding = (Padding 0 5 0 5)
    , textConfig {
        text = "Settings"
      , textSize = FontSize.a_18
      , color = Color.darkDescriptionText
      , fontStyle = FontStyle.bold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

primaryEditTextConfig :: ST.DriverProfileScreenState -> PrimaryEditText.Config
primaryEditTextConfig state = let 
  config = PrimaryEditText.config
  primaryEditTextConfig' = config
    { editText
      { singleLine = true
        , pattern = Just "[0-9]*,10"
        , fontStyle = FontStyle.medium LanguageStyle
        , textSize = FontSize.a_16
        , text = ""
        , placeholder =""
      }
    , topLabel
      { textSize = FontSize.a_14
      , text = ""
      , color = Color.greyTextColor
      }
    }
  in primaryEditTextConfig'