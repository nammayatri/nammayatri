{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.RideCompletedScreen.ComponentConfig where

import Prelude
import Screens.Types as ST
import Components.PrimaryButton as PrimaryButton
import Styles.Colors as Color
import Language.Strings (getString)
import PrestoDOM (Padding(..),  Margin(..), Gravity(..))
import Language.Types (STR(..))
import Components.PopUpModal as PopUpModal
import PrestoDOM.Types.DomAttributes as PTD

rideCompletedButtonConfig :: ST.RideCompletedScreenState -> PrimaryButton.Config
rideCompletedButtonConfig state =
  PrimaryButton.config
    {   textConfig { text = "Ride Complete", color = Color.white900 }
      , cornerRadius = 8.0
      , id = "RideCompletedButton"
      , padding = (Padding 14 16 14 16)
      , margin = (Margin 16 16 16 32)
      , background = Color.purple700
      , enableRipple = true
      , alpha = if (state.props.selectedRating /= ST.SEL_NONE) then 1.0 else 0.4
      , isClickable = state.props.selectedRating /= ST.SEL_NONE
    }


callSupportPopupConfig :: ST.RideCompletedScreenState -> PopUpModal.Config
callSupportPopupConfig state = PopUpModal.config {
  gravity = CENTER,
  cornerRadius = PTD.Corners 15.0 true true true true,
  margin = MarginHorizontal 16 16,
  padding = PaddingTop 24,
  buttonLayoutMargin = Margin 0 0 0 0,
  primaryText {
    text = getString CONTACT_SUPPORT <> "?",
    margin = MarginBottom 12
  },
  secondaryText {
    text = getString $ YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT",
    margin = MarginBottom 16
  },
  option1 {
    text = getString CANCEL
  },
  option2 {
    text = getString CALL_SUPPORT
  }
}
