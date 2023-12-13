{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.RideSelectionScreen.ComponentConfig where

import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Length(..), Margin(..), Visibility(..))
import Styles.Colors (black700, black900, white900, yellow900) as Color
import Components.ErrorModal (config, Config) as ErrorModal
import Font.Size (a_14, a_16, a_18) as FontSize
import Font.Style (bold, medium, regular) as FontStyle
import Components.PrimaryButton as PrimaryButton
import Screens.Types (RideSelectionScreenState)

cancelButtonConfig :: RideSelectionScreenState -> PrimaryButton.Config
cancelButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { margin = (Margin 16 12 16 16)
        , textConfig
          { text = (getString I_DONT_KNOW_WHICH_RIDE)
          , color = Color.black700
          }
        , stroke = "1," <> Color.black700
        , background = Color.white900
        , height = (V 60)
        , isClickable = true
        , cornerRadius = 8.0
        , id = "RideSelectionScreenPrimaryButton"
        }
  in
    primaryButtonConfig'

errorModalConfig :: ErrorModal.Config
errorModalConfig =
  let
    config = ErrorModal.config

    errorModalConfig' =
      config
        { imageConfig
          { imageUrl = "ic_no_past_rides"
          , height = V 110
          , width = V 124
          , margin = (MarginBottom 61)
          }
        , errorConfig
          { text = (getString EMPTY_RIDES)
          , margin = (MarginBottom 7)
          , color = Color.black900
          }
        , errorDescriptionConfig
          { text = (getString YOU_HAVE_NOT_TAKEN_A_TRIP_YET)
          , color = Color.black700
          }
        , buttonConfig
          { text = (getString BOOK_NOW)
          , color = Color.yellow900
          , margin = (Margin 16 0 16 24)
          , visibility = GONE
          , background = Color.black900
          }
        }
  in
    errorModalConfig'
