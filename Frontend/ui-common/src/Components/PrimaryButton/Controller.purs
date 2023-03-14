module Components.PrimaryButton.Controller where

import Font.Size as FontSize
import Font.Style as FontStyle
import Prelude ((<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), height, width)
import Styles.Colors as Color
import Common.Types.App

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
  }

type TextConfig = 
  { text :: String
  , textSize :: Int
  , fontStyle :: String
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , height :: Length
  , width :: Length
  }


type ImageConfig = 
  { height :: Length
  , width :: Length
  , imageUrl :: String
  , margin :: Margin
  , padding :: Padding
  , gravity :: Gravity
  }


config :: Config
config =   { 
    textConfig  : 
    { text : ""
    , fontStyle : FontStyle.semiBold LanguageStyle
    , textSize :  FontSize.a_16
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.yellow900
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    }
  , width: MATCH_PARENT
  , height: V 50
  , cornerRadius: 8.0
  , margin: (Margin 10 0 10 0)
  , stroke: ("0," <> Color.black)
  , alpha: 1.0
  , isClickable: true
  , visibility: VISIBLE
  , background : Color.black900
  , gravity : CENTER
  , isSuffixImage : false
  , suffixImageConfig : 
    {
      height : V 20
    , width : V 20
    , imageUrl : ""
    , margin : (Margin 0 0 0 0)
    , padding : (Padding 0 0 0 0)
    , gravity : LEFT
    }
  , isPrefixImage : false
  , prefixImageConfig :
    { height : V 20
    , width : V 20
    , imageUrl : ""
    , margin : (Margin 0 0 0 0)
    , padding : (Padding 0 0 0 0)
    , gravity : LEFT
    }
  , id : ""
  , enableLoader : false
  }