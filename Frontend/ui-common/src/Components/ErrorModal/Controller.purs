module Components.ErrorModal.Controller where

import PrestoDOM ( Length(..), Margin(..), Visibility(..), Padding(..), Gravity(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.PrimaryButton.Controller as PrimaryButtonController 
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Common.Types.App

data Action = PrimaryButtonActionController PrimaryButtonController.Action 

type Config =
  { 
    height :: Length
  , gravity :: Gravity
  , background :: String 
  , stroke :: String 
  , corners :: Corners
  , imageConfig :: ImageConfig
  , errorConfig :: TextConfig
  , errorDescriptionConfig :: TextConfig
  , buttonConfig :: ButtonConfig 
  }

type ImageConfig = 
  { imageUrl :: String
  , height :: Length
  , width :: Length 
  , margin :: Margin 
  , visibility :: Visibility
  }

type TextConfig =
  { text :: String
  , textSize :: Int
  , fontStyle :: String
  , color :: String 
  , padding :: Padding
  , margin :: Margin 
  , visibility :: Visibility
  }

type ButtonConfig =
  { margin :: Margin 
  , text :: String
  , fontStyle :: String
  , textSize :: Int
  , color :: String 
  , width :: Length
  , height :: Length
  , cornerRadius :: Number 
  , stroke :: String
  , background :: String 
  , visibility :: Visibility
  }

config :: Config 
config = 
  { height : MATCH_PARENT
  , gravity : CENTER 
  , background : Color.white900 
  , corners : (Corners 0.0 false false false false)
  , stroke : "0,#000000"
  , imageConfig : 
    { imageUrl : ""
    , height : V 124
    , width : V 124
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    }
  , errorConfig :
    { text : ""
    , textSize : FontSize.a_18
    , fontStyle : FontStyle.medium LanguageStyle
    , color : Color.black
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    }
  , errorDescriptionConfig :
    { text : ""
    , textSize : FontSize.a_14 
    , fontStyle : FontStyle.regular LanguageStyle
    , color : Color.black
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    }
  , buttonConfig :
    { margin : (Margin 0 0 0 0)
    , text : ""
    , fontStyle : FontStyle.bold LanguageStyle
    , textSize : FontSize.a_17
    , color : Color.yellow900
    , width : MATCH_PARENT
    , height : V 50
    , cornerRadius : 8.0
    , stroke : "0,#ffffff"
    , background : Color.black
    , visibility : VISIBLE
    }

  }