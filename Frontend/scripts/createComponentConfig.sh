#!/bin/bash
touch "$3/ComponentConfig.purs"

echo "{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.$1Screen.ComponentConfig where 

import Components.GenericHeader as GenericHeader 
import Components.PrimaryButton as PrimaryButton 
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST 
import Styles.Colors as Color
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude ((<>))
import Common.Types.App(LazyCheck(..))


primaryButtonConfig :: ST.$1ScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      {   textConfig
         { text = \"Button\"
         } 
        , margin = (Margin 0 0 0 0)
        , id = \"DummyButton\"
      }
  in primaryButtonConfig'

genericHeaderConfig :: ST.$1ScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  genericHeaderConfig' = GenericHeader.config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET \"ny_ic_chevron_left\" 
      } 
    , textConfig {
        text = \"Generic Header\"
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig' " > "$3/ComponentConfig.purs"

echo "ComponentConfig.purs generated Successfully ! ------------------------------------------- ✔"