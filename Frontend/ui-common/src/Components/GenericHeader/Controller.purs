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