{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.BookingOptionsScreen.ComponentConfig where

import Language.Strings
import PrestoDOM
import Font.Style as FontStyle
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Prelude
import Mobility.Prelude
import Prelude
import Components.PopUpModal as PopUpModal
import Screens.Types as ST
import Helpers.Utils as HU


topAcDriverPopUpConfig :: ST.BookingOptionsScreenState -> PopUpModal.Config
topAcDriverPopUpConfig state = let 
  config' = PopUpModal.config
    { gravity = CENTER,
      margin = MarginHorizontal 24 24 ,
      buttonLayoutMargin = Margin 16 0 16 20 ,
      optionButtonOrientation = "VERTICAL",
      primaryText {
        text = getString TOP_AC_DRIVER,
        margin = Margin 18 8 18 16 
    },
      secondaryText { visibility = GONE },
      option1 {
        text = getString WATCH_VIDEO,
        margin = MarginHorizontal 16 16,
        color = Color.yellow900,
        background = Color.black900,
        strokeColor = Color.white900,
        width = MATCH_PARENT
      },
        option2 {
        text = getString MAYBE_LATER,
        margin = MarginHorizontal 16 16,
        color = Color.black650,
        background = Color.white900,
        strokeColor = Color.white900,
        width = MATCH_PARENT
      },
      backgroundClickable = true,
      dismissPopup = true,
      cornerRadius = Corners 15.0 true true true true,
      coverImageConfig {
        visibility = VISIBLE,
        height = V 215,
        width = V 320,
        margin = MarginTop 16,
        imageUrl = HU.fetchImage HU.FF_ASSET "ny_ac_explanation"
      }
    }
  in config'