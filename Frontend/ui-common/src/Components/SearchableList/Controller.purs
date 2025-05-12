module Components.SearchableList.Controller where

import Prelude
import Data.Maybe (Maybe(..))
import PrestoDOM (InputType(..),Gravity(..), Length(..), Orientation(..), PrestoDOM, Visibility(..), Accessiblity(..), alpha, background, color, cornerRadius, editText, fontStyle, gravity, height, hint, hintColor, imageUrl, imageView, lineHeight, letterSpacing, linearLayout, margin, onChange, orientation, padding, pattern, singleLine, stroke, text, textSize, textView, visibility, weight, width, id, inputType, multiLineEditText, maxLines, inputTypeI, onFocus, clickable, separator, separatorRepeat, accessibilityHint, accessibility, imageWithFallback)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Font.Style as FontStyle
import Common.Styles.Colors as Color
import Components.PrimaryEditText as PrimaryEditText
import Components.SelectRouteButton as SelectRouteButton
import Effect (Effect)

-- | Actions for the SearchableList component
data Action = 
    OnTextChange String
  | OnOptionClick Option
  | LoadMoreOptions
  | OnFocus Boolean
  | SelectRoute SelectRouteButton.Action
  | NoAction

-- | Type for list options
type Option = {
  label :: String,
  routeText :: Maybe String,
  sourceText :: Maybe String,
  destinationText :: Maybe String,
  value :: String,
  isDisabled :: Boolean,
  customView :: Boolean, -- Structured custom data
  isSelected :: Boolean
}

-- | Type for custom option data
type CustomData = {
  text :: Maybe String,  -- Optional additional text
  iconUrl :: Maybe String, -- Optional icon URL
  metadata :: Maybe String -- Optional additional metadata as String (can be JSON)
}

-- | Empty custom data
emptyCustomData :: CustomData
emptyCustomData = {
  text: Nothing,
  iconUrl: Nothing,
  metadata: Nothing
}

-- | Styling configuration for options
type OptionConfig = {
  margin :: Margin,
  padding :: Padding,
  background :: String,
  selectedBackground :: String,
  disabledAlpha :: Number,
  height :: Int,
  cornerRadius :: Number
}

-- | Component configuration
type Config = {
  -- Basic properties
  visibility :: Visibility,
  title :: String,
  placeholderText :: String,
  searchText :: String,
  
  -- Options and selection
  optionsList :: Array Option,
  selectedValue :: Maybe String,
  
  -- Search configuration
  isSearchable :: Boolean,
  searchInputBackground :: String,
  
  -- Styling
  optionConfig :: OptionConfig,
  mainContainerMargin :: Margin,
  mainContainerPadding :: Padding,
  listBackground :: String,
  listContainerMargin :: Margin,
  
  searchFieldConfig :: PrimaryEditText.Config,

  -- Pagination
  limit :: Maybe Int,
  offset :: Maybe Int,
  hasMoreOptions :: Boolean,
  isLoading :: Boolean
}

-- | Default configuration
config :: Config
config = {
  visibility: GONE,
  title: "Select an option",
  placeholderText: "Search",
  searchText: "",
  
  optionsList: [],
  selectedValue: Nothing,
  
  isSearchable: true,
  searchInputBackground: Color.white900,
  
  optionConfig: {
    margin: Margin 0 8 0 0,
    padding: Padding 16 16 16 16,
    background: Color.white900,
    selectedBackground: Color.blue600,
    disabledAlpha: 0.5,
    height: 56,
    cornerRadius: 8.0
  },
  
  mainContainerMargin: Margin 0 0 0 0,
  mainContainerPadding: Padding 0 0 0 0,
  listBackground: Color.white900,
  listContainerMargin: Margin 0 0 0 0,
  searchFieldConfig: PrimaryEditText.config {
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
      , visibility = GONE
      }
    , stroke = ("1,"<> Color.black500)
    , margin = (Margin 16 12 16 12)
    , errorLabel
      { text = ""
      , margin = (MarginTop 1)
      }
    , showErrorLabel = false
    , width = MATCH_PARENT
    , textImage {
      visibility = GONE,
      imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_drop_down"
    }
    , prefixImage {
      visibility = VISIBLE,
      imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_search_grey"
    }
  },
  limit: Just 10,
  offset: Just 0,
  hasMoreOptions: false,
  isLoading: false
}

-- | Helper function to update the selected option
updateSelectedOption :: String -> Array Option -> Array Option
updateSelectedOption value options =
  map (\option -> option { isSelected = (option.value == value) }) options 