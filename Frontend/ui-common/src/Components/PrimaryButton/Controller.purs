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
import ConfigProvider (getAppConfig)
import Constants
import Debug

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
    , isClickable :: Boolean
    , visibility :: Visibility
    , background :: String
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
  }

type TextConfig =
  { text :: String
  , textStyle :: Style
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , height :: Length
  , width :: Length
  , accessibilityHint :: String
  , weight :: Maybe Number
  , textFromHtml :: Maybe String
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

config :: Config
config = 
  let 
    btnConfig = (getAppConfig appConfig).primaryButtonConfig
  in {
    textConfig  :
    { text : ""
    , textStyle : SubHeading1
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.yellow900
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , accessibilityHint : ""
    , weight : Nothing
    , textFromHtml : Nothing
    }
  , width: MATCH_PARENT
  , height: V 50
  , cornerRadius: 8.0
  , margin: (Margin 10 0 10 0)
  , stroke: ("0," <> Color.black)
  , alpha: 1.0
  , isClickable: true
  , visibility: VISIBLE
  , background : spy "hello" Color.black900
  , gravity : CENTER
  , isSuffixImage : false
  , weight : Nothing
  , enableButtonLayoutId : false
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
  }
