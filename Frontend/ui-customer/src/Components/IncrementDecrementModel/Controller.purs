{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.IncrementDecrementModel.Controller where

import Effect (Effect)
import Prelude (Unit)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Prop)
import Styles.Colors as Color

data Action = NoAction | OnIncrement | OnDecrement

type Config =
  { initialValue :: Int
  , minValue :: Int
  , maxValue :: Int
  , background :: String
  , heading :: String
  , orientation :: Orientation
  , buttonTextConfig :: TextConfig
  , countTextConfig :: TextConfig
  , cornerRadius :: Number
  , padding :: Padding
  , gravity :: Gravity
  , width :: Length
  , lineWidth :: Length
  , stroke :: String
  , countSuffixText :: String
  }

type TextConfig =
  { color :: String
  , cornerRadius :: Number
  , padding :: Padding
  , stroke :: String
  , background :: String
  , gravity :: Gravity
  , margin :: Margin
  , fontStyle :: Array (Prop (Effect Unit))
  }

config :: Config
config =
  { initialValue: 0
  , minValue: 0
  , maxValue: 0
  , background: Color.blue600
  , heading: ""
  , cornerRadius: 0.0
  , orientation : VERTICAL
  , buttonTextConfig : dummyTextConfig
  , countTextConfig : dummyTextConfig
  , padding : Padding 0 0 0 0
  , gravity : CENTER
  , width : V 0
  , lineWidth : V 0
  , stroke : ""
  , countSuffixText : ""
  }

dummyTextConfig :: TextConfig
dummyTextConfig =
  { color: Color.black800
  , cornerRadius: 0.0
  , padding: Padding 0 0 0 0
  , stroke: ""
  , background: ""
  , gravity: CENTER
  , margin : Margin 0 0 0 0
  , fontStyle : []
  }