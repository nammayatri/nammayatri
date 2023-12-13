{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.VehicleDetailsScreen.ComponentConfig where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..), fromMaybe)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Styles.Colors as Color
import Screens.Types as ST

------------------------------ primaryButtonConfig ---------------------------------
primaryButtonConfig :: ST.VehicleDetailsScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString UPDATE)
          , color = Color.primaryButtonColor
          }
        , margin = (Margin 0 0 0 0)
        , cornerRadius = 0.0
        , background = Color.black900
        , height = (V 60)
        , isClickable = state.props.deleteButtonVisibility
        , alpha = if state.props.deleteButtonVisibility then 1.0 else 0.7
        , id = "VehicleDetailsScreenPrimaryButton"
        }
  in
    primaryButtonConfig'

------------------------------ primaryEditTextConfig ---------------------------------
primaryEditTextConfig :: String -> String -> PrimaryEditText.Config
primaryEditTextConfig label value =
  let
    config = PrimaryEditText.config

    primaryEditTextConfig' =
      config
        { editText
          { singleLine = true
          , pattern = Just "[0-9]*,10"
          , text = value
          , alpha = 0.9
          }
        , topLabel
          { text = label
          , color = Color.greyTextColor
          }
        }
  in
    primaryEditTextConfig'
