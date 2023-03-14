module Components.MenuButton.Controller where

import Prelude((<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Common.Types.App

data Action = OnClick Config

type Config = 
  {
      titleConfig :: TextConfig
    , subTitleConfig :: TextConfig    
    , id :: String
    , index :: Int  
    , width :: Length
    , height :: Length
    , cornerRadius :: Number
    , padding :: Padding
    , stroke :: String
    , isClickable :: Boolean
    , visibility :: Visibility
    , background :: String
    , isSelected :: Boolean
    , radioButtonConfig :: RadioButtonConfig
  }

type TextConfig = 
  { text :: String
  , textSize :: Int
  , fontStyle :: String
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , singleLine :: Boolean
  }


type RadioButtonConfig = 
  { height :: Length
  , width :: Length
  , imageHeight :: Length
  , imageWidth :: Length
  , imageUrl :: String
  , imageMargin :: Margin
  , imagePadding :: Padding
  , activeStroke :: String
  , inActiveStroke :: String
  , cornerRadius :: Number
  , buttonMargin :: Margin
  , buttonPadding :: Padding
  }

config :: Config
config = 
  {
      titleConfig :
          { text : ""
          , fontStyle : FontStyle.regular LanguageStyle
          , textSize :  FontSize.a_16
          , gravity : LEFT
          , visibility : VISIBLE
          , color : Color.black800
          , singleLine : true
          }
    , subTitleConfig :  
          { text : ""
          , fontStyle : FontStyle.regular LanguageStyle
          , textSize :  FontSize.a_16
          , gravity : LEFT
          , visibility : VISIBLE
          , color : Color.black700
          , singleLine : true
          }     
    , width : MATCH_PARENT
    , height : V 70
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0) 
    , stroke : ""
    , isClickable : true
    , visibility : VISIBLE
    , background : ""
    , isSelected : false
    , radioButtonConfig :
          { height : V 20
          , width : V 20
          , imageHeight : V 10
          , imageWidth : V 10
          , imageUrl : "ny_ic_radio_button,https://assets.juspay.in/nammayatri/images/common/ny_ic_radio_button.png"
          , imageMargin : (Margin 0 0 0 0)
          , imagePadding : (Padding 0 0 0 0)
          , activeStroke :("2," <> Color.black800)
          , inActiveStroke :("2," <> Color.black600)
          , cornerRadius : 10.0
          , buttonMargin : (Margin 0 10 0 10)
          , buttonPadding : (Padding 0 0 0 0)
          }
    , index : 0
    , id : ""
  }

