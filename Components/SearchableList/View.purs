module Components.SearchableList.View where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.SearchableList.Controller (Action(..), Config, Option, CustomData)
import Components.PrimaryEditText.View as PrimaryEditText
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Data.Array (length, mapWithIndex, null)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons (screenHeight)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Mobility.Prelude (boolToVisibility)
import Font.Style as FontStyle
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, accessibilityHint, background, weight, clickable, color, cornerRadius, gravity, height, imageUrl, imageView, imageWithFallback, linearLayout, margin, maxHeight, onClick, orientation, padding, relativeLayout, scrollView, singleLine, stroke, text, textSize, textView, visibility, width, alpha, editText, hint, hintColor, onChange, onFocus)

-- | Main view function for SearchableList component
view :: forall w. (Action -> Effect Unit) -> Config w -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility state.visibility
  , margin state.mainContainerMargin
  , padding state.mainContainerPadding
  ]
  [ headerView push state
  , searchInputView push state
  , optionsListView push state
  ]

-- | View for the header/title
headerView :: forall w. (Action -> Effect Unit) -> Config w -> PrestoDOM (Effect Unit) w
headerView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ MarginBottom 8
  ]
  [ 
    textView $ 
    [ text state.title
    , width MATCH_PARENT
    , height WRAP_CONTENT
    , color "#000000"
    ] <> FontStyle.h3 TypoGraphy
  ]

-- | View for the search input field - now using PrimaryEditText
searchInputView :: forall w. (Action -> Effect Unit) -> Config w -> PrestoDOM (Effect Unit) w
searchInputView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , visibility $ boolToVisibility state.isSearchable
  , margin $ MarginBottom 16
  ]
  [
    case state.primaryEditTextConfig of
      Just config ->
        -- Use PrimaryEditText component
        PrimaryEditText.view 
          (\action -> case action of
              PrimaryEditTextController.TextChanged _ text -> push (OnTextChange text)
              PrimaryEditTextController.FocusChanged focus -> push (OnFocus focus)
              _ -> push NoAction
          ) 
          (PrimaryEditTextController.config {
            editText {
              color = config.textColor,
              singleLine = true,
              placeholder = config.placeholder,
              textStyle = FontStyle.Body1
            },
            stroke = "1," <> config.underlineColor,
            margin = config.margin,
            width = MATCH_PARENT
          })
      Nothing ->
        -- Fallback to basic search with icon
        linearLayout
        [ height $ V 48
        , width MATCH_PARENT
        , background state.searchInputBackground
        , cornerRadius 8.0
        , padding $ Padding 0 0 0 0
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ]
        [
          imageView
          [ width $ V 24
          , height $ V 24
          , margin $ MarginLeft 12
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_search"
          ],
          editText
          [ height MATCH_PARENT
          , width WRAP_CONTENT
          , weight 1.0
          , hint state.placeholderText
          , hintColor "#9E9E9E" -- Gray hint text
          , color "#000000"     -- Black text color
          , padding $ Padding 12 0 12 0
          , background "transparent"
          , singleLine true
          , text state.searchText
          , onChange push (\text -> OnTextChange text)
          , onFocus push (\focus -> OnFocus focus)
          ]
        ]
  ]

-- | View for the options list
optionsListView :: forall w. (Action -> Effect Unit) -> Config w -> PrestoDOM (Effect Unit) w
optionsListView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background state.listBackground
  , cornerRadius 8.0
  , margin state.listContainerMargin
  ]
  [
    scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , maxHeight $ V 300 -- Maximum height for the dropdown
    ]
    [
      linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      (if null state.optionsList
        then [emptyStateView]
        else (mapWithIndex (\index option -> renderOption push state option index) state.optionsList)
        <>
        [loadMoreView push state])
    ]
  ]

-- | Render an option using either custom or default view
renderOption :: forall w. (Action -> Effect Unit) -> Config w -> Option -> Int -> PrestoDOM (Effect Unit) w
renderOption push state option index = 
  case state.customOptionView of
    Just customRender -> customRender push option index
    Nothing -> defaultOptionItemView push state option index

-- | Default view for individual option items
defaultOptionItemView :: forall w. (Action -> Effect Unit) -> Config w -> Option -> Int -> PrestoDOM (Effect Unit) w
defaultOptionItemView push state option index =
  linearLayout
  [ height $ V state.optionConfig.height
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , padding state.optionConfig.padding
  , margin state.optionConfig.margin
  , background $ if option.isSelected then state.optionConfig.selectedBackground else state.optionConfig.background
  , cornerRadius state.optionConfig.cornerRadius
  , alpha $ if option.isDisabled then state.optionConfig.disabledAlpha else 1.0
  , gravity CENTER_VERTICAL
  , onClick push $ const $ if option.isDisabled then NoAction else OnOptionClick option
  , clickable $ not option.isDisabled
  ]
  [
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , weight 1.0
    , orientation VERTICAL
    ]
    [
      textView $
      [ text option.label
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , color $ if option.isSelected then "#FFFFFF" else "#000000"
      ] <> FontStyle.body1 TypoGraphy
    ],
    -- Check mark for selected item
    imageView
    [ width $ V 24
    , height $ V 24
    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_tick"
    , visibility $ boolToVisibility option.isSelected
    ]
  ]

-- | View for empty state when no options are available
emptyStateView :: forall w. PrestoDOM (Effect Unit) w
emptyStateView =
  linearLayout
  [ height $ V 60
  , width MATCH_PARENT
  , gravity CENTER
  ]
  [
    textView $
    [ text "No options available"
    , color "#757575" -- Gray text
    ] <> FontStyle.body2 TypoGraphy
  ]

-- | View for load more button at the end of the list
loadMoreView :: forall w. (Action -> Effect Unit) -> Config w -> PrestoDOM (Effect Unit) w
loadMoreView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER
  , padding $ Padding 0 16 0 16
  , visibility $ boolToVisibility state.hasMoreOptions
  , margin $ MarginVertical 8 8
  ]
  [
    linearLayout
    [ height $ V 40
    , width MATCH_PARENT
    , background "#EEEEEE"
    , cornerRadius 8.0
    , gravity CENTER
    , onClick push $ const LoadMoreOptions
    , clickable true
    ]
    [
      textView $
      [ text $ if state.isLoading then "Loading..." else "Load More"
      , color "#424242"
      ] <> FontStyle.body2 TypoGraphy
    ]
  ] 