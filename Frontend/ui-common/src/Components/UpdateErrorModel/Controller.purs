module Components.UpdateErrorModal.Controller where

import PrestoDOM ( Length(..), Margin(..), Visibility(..), Padding(..), Gravity(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Common.Types.App

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
  , textSize :: Int
  , fontStyle :: String
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
    { imageUrl : "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
    , height : V 25
    , width : V 25
    , margin : (MarginRight 15)
    , padding : (Padding 0 0 0 0)
    , alpha : 0.7
    },
    textConfig :
    { height : WRAP_CONTENT
    , text : ""
    , textSize : 16
    , fontStyle : FontStyle.medium LanguageStyle
    , color : Color.black800
    , padding : (Padding 10 10 10 10)
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    , gravity : CENTER
    , weight : 1.0
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