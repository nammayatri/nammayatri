{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.InputView.Controller where

import Effect (Effect)
import Components.SeparatorView.View as SeparatorView
import Prelude
import PrestoDOM ( Length(..), Padding(..), Margin(..), Gravity(..), Visibility(..), Prop, layoutGravity, PrestoDOM(..), Orientation(..), linearLayout, height, width, orientation, margin, padding, textView, color, background, cornerRadius, weight, text, imageView, imageWithFallback, stroke, gravity, visibility, onChange, onFocus, onClick, selectAllOnFocus, hint, hintColor, cursorColor, pattern, maxLines, singleLine, ellipsize, editText, id, afterRender, clickable, relativeLayout, frameLayout)
import Data.Maybe
import Styles.Colors as Color
import Styles.Types (Color) 

data Action = TextFieldFocusChanged String Boolean Int Boolean
            | ClearTextField String 
            | InputChanged Int String 
            | AutoCompleteCallBack String Boolean
            | BackPress
            | DateTimePickerButtonClicked
            | BackPressed
            | NoAction
            | AddStopAction
            | SetSelectedBoxId String
            | AddRemoveStopAction String Int
            | SwapLocation Int Int 

type Address =
  { area :: Maybe String
  , state :: Maybe String
  , country :: Maybe String
  , building  :: Maybe String
  , door :: Maybe String
  , street :: Maybe String
  , city :: Maybe String
  , areaCode :: Maybe String
  , ward :: Maybe String
  , placeId :: Maybe String
  }

type InputViewConfig = 
  { backIcon :: ImageConfig
  , headerText :: String 
  , suffixButton :: ButtonLayoutConfig
  , headerVisibility :: Boolean
  , inputView :: Array InputView
  , imageLayoutMargin :: Margin
  , imageLayoutWidth :: Length
  , imageLayoutVisibility :: Visibility
  , suffixButtonVisibility :: Visibility
  , backgroundColor :: Color
  , inputLayoutPading :: Padding
  }

type InputView =
  { padding :: Padding
  , height :: Length
  , gravity :: Gravity
  , canClearText :: Boolean 
  , isEditable :: Boolean  
  , isClickable :: Boolean
  , prefixImage :: ImageConfig
  , stroke :: String
  , imageSeparator :: SeparatorView.Config 
  , clearTextIcon :: ImageConfig
  , fontStyle :: forall properties. Array (Prop properties)    
  , placeAddress :: Address
  , place :: String
  , placeId :: Maybe String  
  , placeLat :: Number
  , placeLong :: Number
  , inputTextConfig :: InputTextConfig
  , inputTextViewContainerMargin :: Margin 
  , index :: Int
  , crossBtnEnabled :: Boolean
  }
  

type InputTextConfig =
  { textValue :: String
  , isFocussed :: Boolean
  , imageName :: String
  , margin :: Margin
  , placeHolder :: String
  , id :: String
  , hint :: String 
  , cornerRadius :: Number
  , textColor :: String
  , prefixImageVisibility :: Visibility
  , prefixImageConfig :: ImageConfig
  , postfixImageConfig :: ImageConfig
  , swapImageConfig :: ImageConfig
  }

type ImageConfig = 
  { imageName :: String 
  , height :: Length 
  , width :: Length 
  , padding :: Padding
  , layoutWidth :: Length
  , layoutHeight :: Length
  , layoutCornerRadius :: Number
  , layoutPadding :: Padding
  , layoutMargin :: Margin
  , layoutColor :: Color
  }

type ButtonLayoutConfig = 
  { text :: String
  , fontStyle :: Array (Prop (Effect Unit))
  , prefixImage :: String
  , suffixImage :: String
  , padding :: Padding
  , gravity :: Gravity
  }

config :: InputViewConfig
config = {
  backIcon : dummyImageConfig {
    imageName = "ny_ic_chevron_left_white,https://assets.moving.tech/beckn/mobilitypaytm/user/ny_ic_chevron_left_white.png"
    , height = V 24
    , width = V 24
    , padding = PaddingTop 16 
  },
  headerText : "",
  headerVisibility : true,
  inputView : [],
  imageLayoutMargin : MarginLeft 24,
  imageLayoutWidth : V 20,
  inputLayoutPading : PaddingLeft 8,
  imageLayoutVisibility : VISIBLE,
  suffixButtonVisibility : GONE,
  suffixButton : {
    text : "",
    fontStyle : [],
    prefixImage : "",
    suffixImage : "",
    padding : Padding 0 0 0 0,
    gravity : CENTER_VERTICAL
  },
  backgroundColor : Color.squidInkBlue
}

dummyImageConfig :: ImageConfig
dummyImageConfig = {
  imageName : "",
  height : V 0,
  width : V 0,
  padding : Padding 0 0 0 0
  , layoutWidth : V 0
  , layoutHeight : V 0
  , layoutCornerRadius : 0.0
  , layoutPadding : Padding 0 0 0 0
  , layoutMargin : Margin 0 0 0 0
  , layoutColor : ""
}

separatorConfig :: SeparatorView.Config
separatorConfig = 
  { orientation : VERTICAL
  , count : 4 
  , height : V 4
  , width : V 1
  , layoutWidth : V 12
  , layoutHeight : V 15
  , color : Color.black500
  , margin : MarginVertical 2 2
  }

defaultInputView :: InputView
defaultInputView = 
  {  padding : Padding 0 0 0 0
  , height : V 0
  , gravity : CENTER
  , canClearText : false
  , isEditable : false 
  , isClickable : false
  , prefixImage : dummyImageConfig  
  , stroke : ""
  , imageSeparator : separatorConfig 
  , clearTextIcon : dummyImageConfig
  , fontStyle : []
  , placeAddress : dummyAddress
  , place : ""
  , placeId : Nothing
  , placeLat : 0.0
  , placeLong : 0.0
  , index : 0
  , inputTextViewContainerMargin : Margin 0 0 0 0 
  , inputTextConfig : {
    textValue : ""
  , isFocussed : false
  , imageName : ""
  , margin : Margin 0 0 0 0
  , placeHolder : ""
  , id : ""
  , hint : ""
  , cornerRadius : 0.0
  , textColor : Color.blueGrey
  , prefixImageVisibility : GONE
  , prefixImageConfig : dummyImageConfig     
  , postfixImageConfig :dummyImageConfig
  , swapImageConfig : dummyImageConfig
  }
  , crossBtnEnabled : false
  }

dummyAddress :: Address
dummyAddress = 
  { "area"      : Nothing
  , "state"     : Nothing
  , "country"   : Nothing
  , "building"  : Nothing
  , "door"      : Nothing
  , "street"    : Nothing
  , "city"      : Nothing
  , "areaCode"  : Nothing
  , "ward"      : Nothing
  , "placeId"   : Nothing
  }