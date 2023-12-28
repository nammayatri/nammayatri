{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.UpdateErrorModal.Controller where

import PrestoDOM ( Length(..), Margin(..), Visibility(..), Padding(..), Gravity(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Size as FontSize
import Font.Style (Style (..))
import Common.Styles.Colors as Color
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude ((<>))

data Action = OnCloseClick

type Config =
  { imageConfig :: ImageConfig
  , textConfig :: TextConfig
  , starterLayout :: StarterLayoutConfig
  }

type ImageConfig =
  { imageUrl :: String
  , height :: Length
  , width :: Length
  , margin :: Margin
  , alpha :: Number
  , padding :: Padding
  }

type TextConfig =
  { height :: Length
  , textStyle :: Style
  , text :: String
  , color :: String
  , padding :: Padding
  , margin :: Margin
  , visibility :: Visibility
  , gravity :: Gravity
  , weight :: Number
  }

type StarterLayoutConfig =
  { width :: Length
  , height :: Length
  , background :: String
  , cornerRadii :: Corners
  , padding :: Padding
  , margin :: Margin
  }

config :: Config 
config = 
  { imageConfig : 
    { imageUrl : fetchImage FF_COMMON_ASSET "ny_ic_close"
    , height : V 25
    , width : V 25
    , margin : (MarginRight 15)
    , padding : (Padding 0 0 0 0)
    , alpha : 0.7
    },
    textConfig :
    { height : WRAP_CONTENT
    , text : ""
    , color : Color.black800
    , padding : (Padding 10 10 10 10)
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    , gravity : CENTER
    , weight : 1.0
    , textStyle : SubHeading2
    },
    starterLayout :
    { height : MATCH_PARENT
    , width : V 5
    , background : Color.red
    , padding : (Padding 0 0 0 0)
    , cornerRadii : Corners 15.0 true false false true
    , margin : (MarginLeft 8)
    }
  }
