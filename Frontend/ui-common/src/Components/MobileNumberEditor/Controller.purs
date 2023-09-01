{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.MobileNumberEditor.Controller where

import Prelude((<>), negate)
import Font.Size as FontSize
import Common.Styles.Colors as Color
import Font.Style (Style(..)) 
import Data.Maybe(Maybe(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), LetterSpacing(..), Accessiblity(..), accessibilityImportance)
import Common.Types.App
import Animation.Config (AnimConfig, animConfig)
import PrestoDOM.Animation as PrestoAnim

data Action = TextChanged String String | FocusChanged Boolean | CountryCodeSelected CountryCodeObj | ShowOptions | CloseOptions 

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
  , countryCodeField :: CountryCodeFieldConfig
  , showCountryCodeField :: Boolean
  , focusedStroke :: String 
  , countryCodeOptionConfig :: CountryCodeOptionConfig
  , countryCodeOptionElementConfig :: CountryCodeOptionElementConfig
  , countryCodeCaptureConfig :: CountryCodeCaptureConfig
  }

type CountryCodeFieldConfig = {
    width :: Length
  , height :: Length
  , gravity :: Gravity
  , text :: String
  , textStyle :: Style
  , color :: String
  , padding :: Padding
  , margin :: Margin
  , countryCodeOptionExpanded :: Boolean
  , countryCode :: String
}

type EditTextConfig =
  { text :: String
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , placeholder :: String
  , placeholderColor :: String
  , singleLine :: Boolean
  , pattern :: Maybe String
  , margin :: Margin
  , padding :: Padding
  , letterSpacing :: LetterSpacing
  , alpha :: Number
  , capsLock :: Boolean
  , enabled :: Boolean
  , focused :: Boolean
  , textStyle :: Style
  , separatorRepeat :: String
  , separator :: String
  , accessibilityHint :: String
  }

type TextConfig =
  { text :: String
  , color :: String
  , gravity :: Gravity
  , visibility :: Visibility
  , alpha :: Number
  , margin :: Margin
  , textStyle :: Style
  , accessibilityImportance :: Accessiblity
  }
type ImageConfig =
  { height :: Length
  , width :: Length
  , imageUrl :: String
  , margin :: Margin
  , padding :: Padding
  }

type CountryCodeOptionConfig  = 
  { height :: Length 
  , width :: Length
  , background :: String
  , stroke :: String
  , cornerRadius :: Number 
  , margin :: Margin
  }
type CountryCodeOptionElementConfig = 
  { height :: Length
  , width :: Length
  , leftTextColor :: String 
  , rightTextColor :: String
  , leftTextMargin :: Margin
  , rightTextMargin :: Margin 
  , lineHeight :: Length 
  , lineColor :: String
  , lineMargin :: Margin 
  , lineVisibility :: Boolean
  }

type CountryCodeCaptureConfig = 
  { height :: Length
  , width :: Length
  , margin :: Margin
  , background :: String 
  , cornerRadius :: Number
  }

config :: Config
config = {
    height : V 54
  , width : MATCH_PARENT
  , margin : (Margin 10 0 10 15)
  , type : ""
  , editText :
    { text : ""
    , textStyle : SubHeading1
    , color : Color.black800
    , gravity : LEFT
    , visibility : VISIBLE
    , placeholder : ""
    , placeholderColor : Color.blueGrey
    , singleLine : true
    , pattern : Nothing
    , margin : (Margin 20 0 10 0)
    , padding : (Padding 20 16 16 16)
    , letterSpacing : PX 0.0
    , alpha : 1.0
    , capsLock : false
    , enabled : true
    , focused : false
    , separatorRepeat : ""
    , separator : ""
    , accessibilityHint : ""
    }
  , visibility : VISIBLE
  , background : Color.white900
  , cornerRadius : 5.0
  , stroke : ("1," <> Color.borderColorLight)
  , warningStroke : ("1," <> Color.lightMaroon)
  , focusedStroke : ("1," <> Color.blue800)
  , id : ""
  , topLabel :
    { text : ""
    , textStyle : Body3
    , gravity : LEFT
    , visibility : VISIBLE
    , color : Color.yellow900
    , alpha : 1.0
    , margin : (Margin 0 0 0 10)
    , accessibilityImportance : DISABLE
    }
  , showErrorLabel : false
  , errorLabel : {
      text : ""
    , gravity : LEFT
    , textStyle : Body3
    , color : Color.lightMaroon
    , visibility : GONE
    , alpha : 1.0
    , margin : (Margin 0 0 0 0)
    , accessibilityImportance : ENABLE
    }
  , showErrorImage : false
  , errorImageConfig :
  {   height : V 10
    , width : V 10
    , imageUrl : ""
    , margin : (Margin 0 0 0 0)
    , padding : (Padding 0 0 0 0)
  }
  , showCountryCodeField : false
  , countryCodeField : {
      width : WRAP_CONTENT
    , height : MATCH_PARENT
    , gravity : CENTER
    , text : "+91"
    , textStyle : ParagraphText
    , color : Color.black800
    , padding : Padding 0 0 0 0
    , margin : Margin 10 0 0 0
    , countryCodeOptionExpanded : false
    , countryCode : "+91"
  }
  , countryCodeOptionConfig : {
      height : WRAP_CONTENT
    , width : MATCH_PARENT
    , background : Color.white900
    , stroke : "1,"<>Color.grey900
    , cornerRadius : 8.0 
    , margin : MarginTop 0
  }
  , countryCodeOptionElementConfig :{ 
    height : WRAP_CONTENT
  , width : MATCH_PARENT
  , leftTextColor : Color.black900 
  , rightTextColor : Color.black900
  , leftTextMargin :  Margin 16 15 16 15
  , rightTextMargin :  Margin 16 15 16 15 
  , lineHeight : V 1
  , lineColor : Color.grey900
  , lineMargin : MarginHorizontal 16 16 
  , lineVisibility : true
  }
  , countryCodeCaptureConfig : { 
    height : V 54
  , width : V 75
  , margin : MarginRight 8
  , background : Color.white900
  , cornerRadius : 8.0
  }
}


