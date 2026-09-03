{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.SelectFaqScreen.ComponentConfig where

import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), Gravity(..))
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Prelude
import Data.Maybe (Maybe(..))
import Data.String as DS
import Screens.SelectFaqScreen.Transformer 
import Screens.SelectFaqScreen.ScreenData
import Helpers.Utils (fetchImage, FetchImageFrom(..), isParentView, showTitle)

genericHeaderConfig :: ST.SelectFaqScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = if state.data.config.nyBrandingVisibility then GenericHeader.merchantConfig else GenericHeader.config
  btnVisibility =  config.prefixImageConfig.visibility
  titleVisibility = if showTitle FunctionCall then config.visibility else GONE
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , visibility =  btnVisibility
      , margin = Margin 8 8 8 8 
      , layoutMargin = Margin 4 4 4 4
      , enableRipple = true
      } 
    , textConfig {
        text = "FAQs"
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , visibility = titleVisibility
    }
  in genericHeaderConfig'

apiErrorModalConfig :: ST.SelectFaqScreenState -> ErrorModal.Config
apiErrorModalConfig state = let
  config = ErrorModal.config
  errorModalConfig' = config
    { imageConfig {
        imageUrl = fetchImage FF_ASSET "ny_ic_error_404"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 32)
      }
    , errorConfig {
        text = (getString ERROR_404)
      , margin = (MarginBottom 7)
      , color = Color.black800
      }
    , errorDescriptionConfig {
        text = (getString PROBLEM_AT_OUR_END)
      , color = Color.black700
      , margin = (Margin 16 0 16 0)
      }
    , buttonConfig {
        text = (getString NOTIFY_ME)
      , margin = (Margin 16 0 16 16)
      , background = state.data.config.primaryBackground
      , color = state.data.config.primaryTextColor
      }
    }
  in errorModalConfig'

