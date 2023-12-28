{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.AboutUsScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM ( Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..), showTitle, isParentView)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

genericHeaderConfig :: ST.AboutUsScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = if state.appConfig.nyBrandingVisibility then GenericHeader.merchantConfig else GenericHeader.config
  btnVisibility = if isParentView FunctionCall then GONE else config.prefixImageConfig.visibility
  titleVisibility = if showTitle FunctionCall then config.visibility else GONE
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , visibility = btnVisibility
      } 
    , textConfig {
        text = (getString ABOUT)
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , visibility = titleVisibility
    }
  in genericHeaderConfig'
