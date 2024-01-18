{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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