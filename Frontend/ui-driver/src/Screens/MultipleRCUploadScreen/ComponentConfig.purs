{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MultipleRCUploadScreen.ComponentConfig where

import Data.Maybe

import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.ReferralMobileNumber as ReferralMobileNumber
import Engineering.Helpers.Commons (screenWidth)
import Font.Size as FontSize
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (unit, (<>), (||), (-), (/))
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, clickable, color, cornerRadius, fontStyle, gravity, height, margin, orientation, padding, text, textSize, textView, visibility, width)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: ST.MultipleRCUploadScreenState -> String -> Boolean -> PrimaryButton.Config
primaryButtonConfig state title isEnabled = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = title
      , color = Color.primaryButtonColor
      }
      , cornerRadius = 8.0
      , alpha = if isEnabled then 1.0 else 0.5
      , isClickable = if isEnabled then true else false
      , background = Color.black900
      , height = (V 48)
      , margin = (Margin 16 14 16 14)
      }
  in primaryButtonConfig'

skipButtonConfig :: ST.MultipleRCUploadScreenState -> PrimaryButton.Config
skipButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = "Skip"
      , color = Color.black800
      }
      , cornerRadius = 8.0
      , background = Color.white900
      , height = (V 48)
      , stroke = "1," <> Color.grey900
      , width =  (V ((screenWidth unit/2)-48))
      , margin = (MarginRight 12)
      }
  in primaryButtonConfig'

activateButtonConfig :: ST.MultipleRCUploadScreenState -> PrimaryButton.Config
activateButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = "Yes, Activate it"
      , color = Color.primaryButtonColor
      }
      , cornerRadius = 8.0
      , background = Color.black900
      , height = (V 48)
      , width =  (V ((screenWidth unit/2)-48)) 
      }
  in primaryButtonConfig'

