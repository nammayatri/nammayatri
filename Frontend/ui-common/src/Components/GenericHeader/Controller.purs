{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.GenericHeader.Controller where

import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), padding)
import Styles.Colors as Color
import Common.Types.App

data Action = SuffixImgOnClick | PrefixImgOnClick

type Config = 
  {
    height :: Length
  , width :: Length
  , margin :: Margin
  , padding :: Padding
  , background :: String
  , isClickable :: Boolean
  , gravity :: Gravity
  , prefixImageConfig :: ImageConfig
  , textConfig :: TextConfig 
  , suffixImageConfig :: ImageConfig
  }

type ImageConfig =
  {
    height :: Length
  , width :: Length
  , imageUrl :: String
  , margin :: Margin
  , padding :: Padding 
  , visibility :: Visibility
  }

type TextConfig =
  {
    text :: String
  , textSize :: Int
  , margin :: Margin
  , fontStyle :: String
  , color :: String
  }

config :: Config
config = {
    height : V 56
  , width : MATCH_PARENT
  , margin : (Margin 0 0 0 0)
  , padding : (Padding 0 0 0 0)
  , background : Color.white900
  , gravity : CENTER_VERTICAL
  , isClickable : true
  , prefixImageConfig : {
      height : V 25
    , width : V 25
    , imageUrl : ""
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    }
  , textConfig : {
      text : ""
    , textSize : FontSize.a_18
    , margin : (Margin 0 0 0 0)
    , fontStyle : FontStyle.semiBold LanguageStyle
    , color : Color.black
    }
  , suffixImageConfig : {
      height : V 25
    , width : V 25
    , imageUrl : ""
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : GONE
    }

}