module Components.SearchableList.View where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.SearchableList.Controller (Action(..), Config, Option, CustomData)
import Data.Array (length, mapWithIndex, null)
import Data.Maybe (fromMaybe, isJust, maybe, Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons (screenHeight)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Mobility.Prelude (boolToVisibility)
import Font.Style as FontStyle
import Components.SelectRouteButton as SelectRouteButton
import Styles.Colors as Color
import Components.PrimaryEditText as PrimaryEditText
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, accessibilityHint, background, weight, clickable, color, cornerRadius, gravity, height, imageUrl, imageView, imageWithFallback, linearLayout, margin, maxHeight, onClick, orientation, padding, relativeLayout, scrollView, singleLine, stroke, text, textSize, textView, visibility, width, alpha, editText, hint, hintColor, onChange, onFocus)

-- | Main view function for SearchableList component
view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
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
  , linearLayout [
    width MATCH_PARENT,
    height $ V 1,
    background Color.grey900,
    margin $ MarginVertical 4 4
    ][]
  , optionsListView push state
  ]

-- | View for the header/title
headerView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
headerView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 16 0 16 8
  ]
  [ 
    textView $ 
    [ text state.title
    , width MATCH_PARENT
    , height WRAP_CONTENT
    , color Color.black800
    ] <> FontStyle.body1 TypoGraphy
  ]

-- | View for the search input field
searchInputView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
searchInputView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , visibility $ boolToVisibility state.isSearchable
  ]
  [
    PrimaryEditText.view 
      (\action -> case action of
          PrimaryEditText.TextChanged _ text -> push (OnTextChange text)
          PrimaryEditText.FocusChanged focus -> push (OnFocus focus)
          _ -> push NoAction
      ) state.searchFieldConfig
  ]

-- | View for the options list
optionsListView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
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
    [ height $ V (screenHeight unit - 400)
    , width MATCH_PARENT
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
renderOption :: forall w. (Action -> Effect Unit) -> Config -> Option -> Int -> PrestoDOM (Effect Unit) w
renderOption push state option index = 
  case option.customView of
    true -> SelectRouteButton.view (push <<< SelectRoute) (mkRouteButtonConfig (fromMaybe "" option.routeText) (fromMaybe "" option.sourceText) (fromMaybe "" option.destinationText))
    false -> defaultOptionItemView push state option index
  where
    mkRouteButtonConfig routeNumber sourceName destinationName = SelectRouteButton.defaultConfig {
          routeNumber = routeNumber
        , sourceName = sourceName
        , onClick = SelectRouteButton.Select index option.value
        , destinationName = destinationName
        , showChevron = false
        , cornerRadius = 12.0
        , margin = Margin 10 16 10 24
        , padding = Padding 16 16 16 16
        , fontSize = FontStyle.Body20
        , showDot = true
      }    

-- | Default view for individual option items
defaultOptionItemView :: forall w. (Action -> Effect Unit) -> Config -> Option -> Int -> PrestoDOM (Effect Unit) w
defaultOptionItemView push state option index =
  linearLayout
  [ height $ V state.optionConfig.height
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , padding state.optionConfig.padding
  , margin state.optionConfig.margin
  , background $ if option.isSelected || (maybe false (\selectedValue -> selectedValue == option.value) state.selectedValue) then state.optionConfig.selectedBackground else state.optionConfig.background
  , cornerRadius state.optionConfig.cornerRadius
  , alpha $ if option.isDisabled then state.optionConfig.disabledAlpha else 1.0
  , gravity CENTER_VERTICAL
  , onClick push $ const $ if option.isDisabled then NoAction else OnOptionClick option
  , clickable $ not option.isDisabled
  , stroke $ "1," <> Color.grey900
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
loadMoreView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
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