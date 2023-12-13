{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.ChooseLanguageScreen.ComponentConfig where

import PrestoDOM (Margin(..), Padding(..), Length(..))
import Components.SelectMenuButton as MenuButton
import Components.PrimaryButton as PrimaryButton
import Screens.Types as ST
import MerchantConfig.Types as MT
import Prelude ((==))
import Styles.Colors as Color

primaryButtonViewConfig :: ST.ChooseLanguageScreenState -> PrimaryButton.Config
primaryButtonViewConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig { text = "Continue" }
        , id = "PrimaryButtonLanguage"
        , isClickable = true
        , height = (V 60)
        , cornerRadius = 8.0
        , margin = (Margin 16 19 16 24)
        }
  in
    primaryButtonConfig'

menuButtonConfig :: ST.ChooseLanguageScreenState -> Int -> MT.Language -> MenuButton.State
menuButtonConfig state index language =
  MenuButton.config
    { text = { name: language.name, value: language.value, subtitle: language.subtitle }
    , isSelected = state.props.selectedLanguage == language.value
    , index = index
    , lineVisibility = false
    , activeStrokeColor = Color.white900
    , activeBgColor = Color.white900
    , inactiveStrokeColor = Color.white900
    , margin = MarginBottom 16
    , padding = Padding 16 24 0 0
    }
