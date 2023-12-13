{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.OnBoardingFlow.ChooseLanguageScreen.ComponentConfig where

import Components.MenuButton as MenuButton
import Components.PrimaryButton as PrimaryButton
import JBridge as JB
import MerchantConfig.Types (Language)
import Prelude ((==))
import PrestoDOM (Margin(..))
import Screens.Types as ST

primaryButtonConfig :: ST.ChooseLanguageScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text = "Continue"
          }
        , margin = (Margin 0 0 0 0)
        , enableLoader = (JB.getBtnLoader "ChooseLanguageScreen")
        , id = "ChooseLanguageScreen"
        }
  in
    primaryButtonConfig'

menuButtonConfig :: ST.ChooseLanguageScreenState -> Language -> MenuButton.Config
menuButtonConfig state language =
  let
    config = MenuButton.config

    menuButtonConfig' =
      config
        { titleConfig
          { text = language.name
          }
        , subTitleConfig
          { text = language.subTitle
          }
        , id = language.value
        , isSelected = (language.value == state.props.selectedLanguage)
        }
  in
    menuButtonConfig'
