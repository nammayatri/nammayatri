{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SelectMenuButton.Controller where

import PrestoDOM (Margin(..), Padding(..))
import Styles.Types (Color)

data Action = OnSelection State

type State = { 
      text :: Text , 
      isSelected :: Boolean ,
      index :: Int,
      lineVisibility :: Boolean,
      activeBgColor :: Color,
      inactiveBgColor :: Color,
      activeStrokeColor :: Color,
      inactiveStrokeColor :: Color,
      margin :: Margin,
      padding :: Padding,
      radioSelectedImage :: String
      }
type Text = { 
    name :: String, 
    value :: String, 
    subtitle :: String
    }


config :: State 
config = {
  text : {
    name : "",
    value : "",
    subtitle : ""
  }, 
  isSelected : false,
  index : 0 ,
  lineVisibility : false,
  activeBgColor : "",
  inactiveBgColor : "",
  activeStrokeColor : "",
  inactiveStrokeColor : "",
  margin : MarginBottom 16,
  padding : Padding 16 16 16 16,
  radioSelectedImage : "ny_ic_radio_selected"
}