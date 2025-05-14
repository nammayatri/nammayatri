{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DropdownTextField.Controller where

import Prelude
import Data.Maybe as Mb
import Common.Styles.Colors as Color
import PrestoDOM (InputType(..),Gravity(..), Length(..), Orientation(..), PrestoDOM, Visibility(..), Margin(..), Padding(..), Accessiblity(..), alpha, background, color, cornerRadius, editText, fontStyle, gravity, height, hint, hintColor, imageUrl, imageView, lineHeight, letterSpacing, linearLayout, margin, relativeLayout, scrollView, onClick, onChange, onAnimationEnd, orientation, padding, pattern, singleLine, stroke, text, textSize, textView, visibility, weight, width, id, inputType, multiLineEditText, maxLines, inputTypeI, onFocus, clickable, separator, separatorRepeat,imageWithFallback, accessibility, accessibilityHint)
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Components.PrimaryEditText.Controller as PrimaryEditTextController

-- | Actions for the DropdownTextField component
data Action = 
    OnTextChange String
  | OnOptionSelect DropdownOption
  | LoadMoreOptions
  | DismissDropdown
  | FocusChanged Boolean
  | NoAction

-- | Type for dropdown options
type DropdownOption = {
  id :: String,
  name :: String,
  description :: String,
  optionEnabled :: Boolean,
  selected :: Boolean
}

type OptionConfig = {
  margin :: Margin,
  stroke :: String,
  lineVisibility :: Boolean
}

type DropdownContentConfig = {
  stroke :: String,
  margin :: Margin
}

-- | Component configuration
type Config = {
  visibility :: Visibility,
  isOpen :: Boolean,
  searchText :: String,
  placeholder :: String,
  options :: Array DropdownOption,
  optionConfig :: OptionConfig,
  dropdownContentConfig :: DropdownContentConfig,
  selectedOption :: Mb.Maybe DropdownOption,
  searchFieldConfig :: PrimaryEditTextController.Config,
  isLoading :: Boolean,
  hasMoreOptions :: Boolean,
  offset :: Int,
  limit :: Int,
  listMaxHeight :: Int
}

-- | Default configuration
config :: Config
config = {
  visibility: GONE,
  isOpen: false,
  searchText: "",
  placeholder: "",
  options: [],
  optionConfig : {
    margin : Margin 0 12 0 0,
    stroke : "1," <> Color.grey900,
    lineVisibility : false
  },
  dropdownContentConfig : {
    stroke : "",
    margin : Margin 0 0 0 0
  },
  selectedOption: Mb.Nothing,
  searchFieldConfig: PrimaryEditTextController.config {
    editText
      { color = Color.black800
      , singleLine = true
      , placeholder = ""
      , textStyle = FontStyle.SubHeading3
      }
    , background = Color.white900
    , topLabel
      { text = ""
      , color = Color.black800
      , textStyle = FontStyle.Body3
      }
    , stroke = ("1,"<> Color.black500)
    , margin = (Margin 0 0 0 0)
    , errorLabel
      { text = ""
      , margin = (MarginTop 1)
      }
    , showErrorLabel = false
    , width = MATCH_PARENT
    , textImage {
      visibility = VISIBLE,
      imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_drop_down"
    }
  },
  isLoading: false,
  hasMoreOptions: false,
  offset: 0,
  limit: 10,
  listMaxHeight: 300
}