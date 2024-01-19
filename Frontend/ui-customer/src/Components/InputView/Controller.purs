module Components.InputView.Controller where

import Prelude
import PrestoDOM ( Length(..), Padding(..), Margin(..))
import Components.SeparatorView.View as SeparatorView

data Action = TextFieldFocusChanged String Boolean 
            | ClearTextField String 
            | InputChanged String 
            | AutoCompleteCallBack String Boolean
            | BackPress 

type InputViewConfig = 
  { backIcon :: ImageConfig
  , headerText :: String 
  , headerVisibility :: Boolean
  , inputView :: Array InputView
  , imageLayoutMargin :: Margin
  , imageLayoutWidth :: Length
  , inputLayoutPading :: Padding
  }

type InputView =
  { margin :: Margin 
  , padding :: Padding
  , textValue :: String 
  , height :: Length
  , isFocussed :: Boolean 
  , id :: String 
  , placeHolder :: String 
  , canClearText :: Boolean 
  , isEditable :: Boolean 
  , isClickable :: Boolean
  , prefixImage :: ImageConfig
  , stroke :: String
  , imageSeparator :: SeparatorView.Config 
  , clearTextIcon :: ImageConfig
  , cornerRadius :: Number
  }

type ImageConfig = 
  { imageName :: String 
  , height :: Length 
  , width :: Length 
  , padding :: Padding
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
  inputLayoutPading : PaddingLeft 8
}