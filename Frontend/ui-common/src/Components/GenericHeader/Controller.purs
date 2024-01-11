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
import Font.Style (Style(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), Orientation(..), Accessiblity(..), padding)
import Common.Styles.Colors as Color
import Common.Types.App
import Prelude ((<>))
import ConfigProvider

data Action = SuffixImgOnClick | PrefixImgOnClick

type Config =
  {
    height :: Length
  , width :: Length
  , margin :: Margin
  , padding :: Padding
  , background :: String
  , orientation :: Orientation
  , isClickable :: Boolean
  , gravity :: Gravity
  , prefixImageConfig :: ImageConfig
  , textConfig :: TextConfig
  , suffixImageConfig :: ImageConfig
  , visibility :: Visibility
  }

type ImageConfig =
  {
    height :: Length
  , width :: Length
  , imageUrl :: String
  , margin :: Margin
  , padding :: Padding
  , visibility :: Visibility
  , accessibilityHint :: String
  , accessibility :: Accessiblity
  , cornerRadius :: Number
  , layoutMargin :: Margin
  , enableRipple :: Boolean
  , rippleColor :: String
  }

type TextConfig =
  {
    text :: String
  , margin :: Margin
  , color :: String
  , textStyle :: Style
  , accessibilityHint :: String
  }

config :: Config
config = 
  let 
    config = getAppConfig appConfig
  in {
    height : V 56
  , width : MATCH_PARENT
  , margin : (Margin 0 0 0 0)
  , padding : (Padding 0 5 0 5)
  , background : Color.white900
  , gravity : CENTER_VERTICAL
  , isClickable : true
  , orientation : HORIZONTAL
  , prefixImageConfig : {
      height : V 25
    , width : V 25
    , imageUrl : config.genericHeaderConfig.backArrow
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 12 12 12 12)
    , visibility : VISIBLE
    , accessibility : ENABLE
    , accessibilityHint : "Back : Button"
    , cornerRadius : 30.0
    , layoutMargin : (Margin 0 0 0 0)
    , enableRipple : false
    , rippleColor : Color.rippleShade
    }
  , textConfig : {
      text : ""
    , textStyle : Heading3
    , margin : (Margin 0 0 0 0)
    , color : Color.black800
    , accessibilityHint : ""
    }
  , suffixImageConfig : {
      height : V 25
    , width : V 25
    , imageUrl : ""
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : GONE
    , accessibility : DISABLE 
    , accessibilityHint : ""
    , cornerRadius : 0.0
    , layoutMargin : (Margin 0 0 0 0)
    , enableRipple : false
    , rippleColor : Color.rippleShade
    }
  , visibility : VISIBLE
}

merchantConfig :: Config
merchantConfig = 
  let 
    config = getAppConfig appConfig
  in {
    height : V 56
  , width : MATCH_PARENT
  , margin : (Margin 0 0 0 0)
  , padding : (Padding 0 0 0 0)
  , background : Color.white900
  , gravity : CENTER_VERTICAL
  , isClickable : true
  , orientation : VERTICAL
  , prefixImageConfig : {
      height : V 25
    , width : V 25
    , imageUrl : config.genericHeaderConfig.backArrow
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 16 16 16 12)
    , visibility : VISIBLE
    , accessibility : ENABLE 
    , accessibilityHint : "Back : Button"
    , cornerRadius : 30.0
    , layoutMargin : (Margin 0 0 0 0)
    , enableRipple : false
    , rippleColor : Color.rippleShade
    }
  , textConfig : {
      text : ""
    , textStyle : Heading0
    , margin : (Margin 16 0 16 10)
    , color : "#101010"
    , accessibilityHint : ""
    }
  , suffixImageConfig : {
      height : V 25
    , width : V 25
    , imageUrl : ""
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : GONE
    , accessibility : DISABLE 
    , accessibilityHint : ""
    , cornerRadius : 0.0
    , layoutMargin : (Margin 0 0 0 0)
    , enableRipple : false
    , rippleColor : Color.rippleShade
    }
  , visibility : VISIBLE
}
