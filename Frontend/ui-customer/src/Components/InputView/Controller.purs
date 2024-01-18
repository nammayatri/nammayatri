module Components.InputView.Controller where

import Effect (Effect)
import Prelude
import PrestoDOM ( Length(..), Padding(..), Margin(..), Gravity(..), Visibility(..), Prop)
import Components.SeparatorView.View as SeparatorView

data Action = TextFieldFocusChanged String Boolean 
            | ClearTextField String 
            | InputChanged String 
            | AutoCompleteCallBack String Boolean
            | DateTimePickerButtonClicked
            | BackPressed

type InputViewConfig = 
  { backIcon :: ImageConfig
  , headerText :: String 
  , suffixButton :: ButtonLayoutConfig
  , headerVisibility :: Boolean
  , inputView :: Array InputView
  , imageLayoutMargin :: Margin
  , imageLayoutWidth :: Length
  , inputLayoutPading :: Padding
  , imageLayoutVisibility :: Visibility
  }

type InputView =
  { margin :: Margin 
  , padding :: Padding
  , textValue :: String 
  , textColor :: String
  , height :: Length
  , isFocussed :: Boolean 
  , id :: String 
  , placeHolder :: String 
  , canClearText :: Boolean 
  , isEditable :: Boolean 
  , prefixImage :: ImageConfig
  , stroke :: String
  , imageSeparator :: SeparatorView.Config 
  , clearTextIcon :: ImageConfig
  , cornerRadius :: Number
  , fontStyle :: Array (Prop (Effect Unit))
  , gravity :: Gravity
  }

type ImageConfig = 
  { imageName :: String 
  , height :: Length 
  , width :: Length 
  , padding :: Padding
  }

type ButtonLayoutConfig = 
  { text :: String
  , fontStyle :: Array (Prop (Effect Unit))
  , prefixImage :: String
  , suffixImage :: String
  , padding :: Padding
  , gravity :: Gravity
  , visibility :: Visibility
  }

config :: InputViewConfig
config = {
  backIcon : {
    imageName : "ny_ic_chevron_left_white,https://assets.juspay.in/beckn/mobilitypaytm/user/ny_ic_chevron_left_white.png"
    , height : V 24
    , width : V 24
    , padding : PaddingTop 16 
  },
  headerText : "",
  headerVisibility : true,
  inputView : [],
  imageLayoutMargin : MarginLeft 24,
  imageLayoutWidth : V 20,
  inputLayoutPading : PaddingLeft 8,
  imageLayoutVisibility : VISIBLE,
  suffixButton : {
    text : "",
    fontStyle : [],
    prefixImage : "",
    suffixImage : "",
    padding : Padding 0 0 0 0,
    gravity : CENTER_VERTICAL,
    visibility : GONE
  }
}