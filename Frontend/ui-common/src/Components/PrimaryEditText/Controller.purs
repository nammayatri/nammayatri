module Components.PrimaryEditText.Controller where

import Prelude((<>))
import Font.Size as FontSize
import Styles.Colors as Color
import Font.Style as FontStyle
import Data.Maybe(Maybe(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import Common.Types.App

data Action = TextChanged String String

type Config = 
  { height :: Length
  , width :: Length 
  , editText :: EditTextConfig
  , visibility :: Visibility
  , background :: String
  , cornerRadius :: Number
  , stroke :: String
  , id ::String
  , topLabel :: TextConfig
  , showErrorLabel :: Boolean
  , errorLabel :: TextConfig
  , showErrorImage :: Boolean
  , errorImageConfig :: ImageConfig 
  , margin :: Margin
  , type :: String
  , warningStroke :: String
  }


type EditTextConfig = 
  { text :: String
  , textSize :: Int
  , fontStyle :: String
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , placeholder :: String
  , placeholderColor :: String
  , singleLine :: Boolean
  , pattern :: Maybe String
  , margin :: Margin
  , padding :: Padding
  , letterSpacing :: Number
  , alpha :: Number
  , capsLock :: Boolean
  }

type TextConfig = 
  { text :: String
  , textSize :: Int
  , fontStyle :: String
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , alpha :: Number
  , margin :: Margin
  }
type ImageConfig = 
  { height :: Length
  , width :: Length
  , imageUrl :: String
  , margin :: Margin
  , padding :: Padding
  }

config :: Config
config = {  
    height : V 54
  , width : MATCH_PARENT
  , margin : (Margin 10 0 10 15)
  , type : ""
  , editText :
    { text : ""
    , textSize : FontSize.a_12
    , fontStyle : FontStyle.regular LanguageStyle
    , color : Color.black800
    , gravity : LEFT
    , visibility : VISIBLE
    , placeholder : ""
    , placeholderColor : Color.blueGrey
    , singleLine : true
    , pattern : Nothing
    , margin : (Margin 20 0 10 0)
    , padding : (Padding 0 16 16 16)
    , letterSpacing : 0.0
    , alpha : 1.0
    , capsLock : false
    }
  , visibility : VISIBLE
  , background : Color.white900
  , cornerRadius : 5.0
  , stroke : ("1," <> Color.borderColorLight)
  , warningStroke : ("1," <> Color.lightMaroon)
  , id : ""
  , topLabel :  
    { text : ""
    , fontStyle : FontStyle.regular LanguageStyle
    , textSize :  FontSize.a_12
    , gravity : LEFT
    , visibility : VISIBLE
    , color : Color.yellow900
    , alpha : 1.0
    , margin : (Margin 0 0 0 10)
    }
  , showErrorLabel : false
  , errorLabel : { 
      text : ""
    , fontStyle : FontStyle.bold LanguageStyle
    , textSize :  FontSize.a_12
    , gravity : LEFT
    , color : Color.lightMaroon
    , visibility : GONE
    , alpha : 1.0
    , margin : (Margin 0 0 0 0)
    }
  , showErrorImage : false
  , errorImageConfig :  
  {   height : V 10
    , width : V 10
    , imageUrl : ""
    , margin : (Margin 0 0 0 0)
    , padding : (Padding 0 0 0 0)
  }

}