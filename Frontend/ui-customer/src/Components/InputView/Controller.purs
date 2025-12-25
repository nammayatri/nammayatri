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
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Prelude
import PrestoDOM ( Length(..), Padding(..), Margin(..), Gravity(..), Visibility(..), Prop)

data Action = TextFieldFocusChanged String Boolean Boolean
            | ClearTextField String 
            | InputChanged String 
            | AutoCompleteCallBack String Boolean
            | BackPress
            | DateTimePickerButtonClicked
            | BackPressed
            | NoAction

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
  , alpha :: Number
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
  , prefixImageVisibility :: Visibility
  , prefixImageConfig :: ImageConfig
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
  , accessibilityHint :: String
  }

config :: InputViewConfig
config = {
  backIcon : {
    imageName : "ny_ic_chevron_left_white,https://assets.moving.tech/beckn/mobilitypaytm/user/ny_ic_chevron_left_white.png"
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
    gravity : CENTER_VERTICAL,
    accessibilityHint : ""
  }
}

dummyImageConfig :: ImageConfig
dummyImageConfig = {
  imageName : "",
  height : V 0,
  width : V 0,
  padding : Padding 0 0 0 0
}