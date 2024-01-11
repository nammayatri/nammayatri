{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.MyRidesScreen.ComponentConfig where

import Common.Types.App
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color
import Storage as Storage
import Prelude (not, (&&))
import Helpers.Utils (fetchImage, FetchImageFrom(..), isParentView, showTitle)
import Common.Types.App (LazyCheck(..))

apiErrorModalConfig :: ST.MyRidesScreenState -> ErrorModal.Config 
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
      , enableRipple = true
      }
    }
  in errorModalConfig' 

errorModalConfig :: ST.MyRidesScreenState -> ErrorModal.Config 
errorModalConfig state = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_no_past_rides"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 32)
      }
    , errorConfig {
        text = (getString EMPTY_RIDES)
      , margin = (MarginBottom 7)  
      , color = Color.black800
      }
    , errorDescriptionConfig {
        text = (getString YOU_HAVENT_TAKEN_A_TRIP_YET)
      , color = Color.black700
      }
    , buttonConfig {
        text = (getString BOOK_NOW)
      , margin = (Margin 16 0 16 16)
      , background = state.data.config.primaryBackground
      , color = state.data.config.primaryTextColor
      , visibility = if (Storage.isLocalStageOn ST.HomeScreen) && not (isParentView FunctionCall) then VISIBLE else GONE
      , enableRipple = true
      }
    }
  in errorModalConfig' 

genericHeaderConfig :: ST.MyRidesScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = if state.data.config.nyBrandingVisibility then GenericHeader.merchantConfig else GenericHeader.config
  btnVisibility =  if isParentView FunctionCall then GONE else config.prefixImageConfig.visibility
  titleVisibility = if showTitle FunctionCall then config.visibility else GONE
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , visibility = btnVisibility
      , margin = Margin 8 8 8 8
      , layoutMargin = Margin 4 4 4 4
      , enableRipple = true
      } 
    , textConfig {
        text = (getString MY_RIDES)
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , visibility = titleVisibility
    }
  in genericHeaderConfig'
 