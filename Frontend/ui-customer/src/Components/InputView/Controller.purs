module Components.InputView.Controller where

import Effect (Effect)
import Components.SeparatorView.View as SeparatorView
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Prelude
import PrestoDOM ( Length(..), Padding(..), Margin(..), Gravity(..), Visibility(..), Prop)

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
  , imageLayoutVisibility :: Visibility
  , suffixButtonVisibility :: Visibility
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
  , inputTextConfig :: InputTextConfig
  }
  

type InputTextConfig =
  { textValue :: String
  , isFocussed :: Boolean
  , imageName :: String
  , margin :: Margin
  , placeHolder :: String
  , id :: String
  , cornerRadius :: Number
  , textColor :: String
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
  }

config :: InputViewConfig
config = {
  backIcon : {
      imageName : fetchImage FF_ASSET "ny_ic_chevron_left_white"
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
  suffixButtonVisibility : GONE,
  suffixButton : {
    text : "",
    fontStyle : [],
    prefixImage : "",
    suffixImage : "",
    padding : Padding 0 0 0 0,
    gravity : CENTER_VERTICAL
  }
}