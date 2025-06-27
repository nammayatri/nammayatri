{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectLanguageScreen.ComponentConfig where

import Language.Strings
import PrestoDOM

import Components.PrimaryButton as PrimaryButton
import Font.Size as FontSize
import Language.Types (STR(..))
import Screens.Types as ST
import Styles.Colors as Color
import Components.SelectMenuButton as MenuButton
import MerchantConfig.Types as MT
import Prelude ((==))

primaryButtonConfig :: ST.SelectLanguageScreenState -> PrimaryButton.Config
primaryButtonConfig state = PrimaryButton.config { textConfig
      { text = if state.props.onlyGetTheSelectedLanguage then (getString CONFIRM_LANGUAGE) else (getString UPDATE)}
      , margin = (Margin 16 16 16 16)
      , cornerRadius = 8.0
      , id = "SelectLanguageScreenPrimaryButton"
      , enableRipple = true
      }
  


menuButtonConfig :: ST.SelectLanguageScreenState -> MT.Language -> Int -> MenuButton.State
menuButtonConfig state language index = MenuButton.config
    { text =
        { name : language.name
        , value : language.value
        , subtitle : language.subtitle
        }
    , isSelected = (state.props.selectedLanguage == language.value)
    , index = index
    , lineVisibility = false
    , activeStrokeColor = state.data.config.themeColors.radioActiveStroke 
    , activeBgColor =  state.data.config.themeColors.radioActiveBackground 
    , inactiveStrokeColor =  Color.grey100 
    , inactiveBgColor = state.data.config.themeColors.radioInactiveBackground
    , radioSelectedImage = state.data.config.themeColors.radioSelectedImage
    }