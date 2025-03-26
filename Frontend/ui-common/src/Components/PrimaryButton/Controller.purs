{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.PrimaryButton.Controller where

import Font.Size as FontSize
import Font.Style (Style(..))
import Prelude ((<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), Gradient(..), height, width)
import Common.Styles.Colors as Color
import Common.Types.App
import Data.Maybe (Maybe(..))
import PrestoDOM.Animation (Animation(..))
import ConfigProvider 

data Action = OnClick | NoAction


type Config =
  {
      textConfig :: TextConfig
    , width :: Length
    , height :: Length
    , cornerRadius :: Number
    , margin :: Margin
    , stroke :: String
    , alpha :: Number
    , allowAlpha :: Boolean
    , isClickable :: Boolean
    , visibility :: Visibility
    , background :: String
    , buttonInactiveBackground :: String
    , gravity :: Gravity
    , isSuffixImage :: Boolean
    , suffixImageConfig :: ImageConfig
    , isPrefixImage :: Boolean
    , prefixImageConfig :: ImageConfig
    , id :: String
    , enableLoader :: Boolean
    , isGradient :: Boolean
    , gradient :: Gradient
    , padding :: Padding
    , lottieConfig :: LottieConfig
    , weight :: Maybe Number
    , enableButtonLayoutId :: Boolean
    , underlineConfig :: UnderLineConfig 
    , enableRipple :: Boolean
    , rippleColor :: String
    , viewbackground :: String
  }

type TextConfig =
  { text :: String
  , textStyle :: Style
  , color :: String
  , buttonInactiveTextColor :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , height :: Length
  , width :: Length
  , accessibilityHint :: String
  , weight :: Maybe Number
  , textFromHtml :: Maybe String
  , id :: String
  }


type ImageConfig =
  { height :: Length
  , width :: Length
  , imageUrl :: String
  , margin :: Margin
  , padding :: Padding
  , gravity :: Gravity
  , animation :: Array Animation
  }

type LottieConfig = 
  { height :: Length
  , width :: Length
  , lottieURL :: String
  , autoDisableLoader :: Boolean
  }

type UnderLineConfig = 
  { color :: String
  , margin :: Margin
  , padding :: Padding
  , visibility :: Visibility
  , height :: Length
  }

config :: Config
config = 
  let 
    appConfigVal = getAppConfig appConfig
    btnConfig = appConfigVal.primaryButtonConfig
  in {
    textConfig  :
    { text : ""
    , textStyle : SubHeading1
    , gravity : CENTER
    , visibility : VISIBLE
    , color : appConfigVal.primaryTextColor
    , buttonInactiveTextColor : appConfigVal.buttonInactiveTextColor
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , accessibilityHint : ""
    , weight : Nothing
    , textFromHtml : Nothing
    , id : ""
    }
  , enableRipple : false
  , rippleColor : Color.rippleShade
  , width: MATCH_PARENT
  , height: V 50
  , cornerRadius: 8.0
  , margin: (Margin 10 0 10 0)
  , stroke: ("0," <> Color.black)
  , alpha: 1.0
  , allowAlpha : appConfigVal.alphaInPrimaryButtonAllowed
  , isClickable: true
  , visibility: VISIBLE
  , background : appConfigVal.primaryButtonBackground
  , buttonInactiveBackground : appConfigVal.buttonInactiveBackground
  , gravity : CENTER
  , isSuffixImage : false
  , weight : Nothing
  , enableButtonLayoutId : false
  , viewbackground : Color.white900
  , suffixImageConfig :
    {
      height : V 20
    , width : V 20
    , imageUrl : ""
    , margin : (Margin 0 0 0 0)
    , padding : (Padding 0 0 0 0)
    , gravity : LEFT
    , animation : []
    }
  , isPrefixImage : false
  , prefixImageConfig :
    { height : V 20
    , width : V 20
    , imageUrl : ""
    , margin : (Margin 0 0 0 0)
    , padding : (Padding 0 0 0 0)
    , gravity : LEFT
    , animation : []
    }
  , id : ""
  , enableLoader : false
  , isGradient : btnConfig.isGradient
  , gradient : (Linear 90.0 btnConfig.gradient)
  , padding : Padding 0 0 0 0
  , lottieConfig : {
    height : V 30
  , width : V 150
  , lottieURL : btnConfig.loaderUrl
  , autoDisableLoader : true
  }
  , underlineConfig : {
    color : Color.grey900
  , margin : Margin 0 0 0 0
  , padding : Padding 0 0 0 0
  , visibility : GONE
  , height : V 0
  }
}
