{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DropdownTextField.View where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.DropdownTextField.Controller (Action(..), Config, DropdownOption)
import Components.PrimaryEditText.View as PrimaryEditText
import Components.PrimaryEditText.Controller as PrimaryEditText
import Data.Array (length, mapWithIndex, null)
import Data.Maybe (fromMaybe, isJust)
import Effect (Effect)
import Engineering.Helpers.Commons (screenHeight)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Mobility.Prelude (boolToVisibility)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, accessibilityHint, background, weight, clickable, color, cornerRadius, gravity, height, imageUrl, imageView, imageWithFallback, linearLayout, margin, maxHeight, onClick, orientation, padding, relativeLayout, scrollView, singleLine, stroke, text, textSize, textView, visibility, width, alpha)
import Font.Style as FontStyle
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Common.Styles.Colors as Color
import Font.Size as Font
import Debug

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility state.visibility
  , margin $ MarginBottom 16
  ]
  [ searchField push state
  , dropdownContent push state
  ]

searchField :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
searchField push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  ]
  [ 
    PrimaryEditText.view (\action -> case action of
      PrimaryEditText.TextChanged _ text -> if text /= "false" && text /= "true" then push (OnTextChange text) else pure unit -- true/false Condition Boolean Value was coming in primaryEditText
      PrimaryEditText.FocusChanged val -> push (FocusChanged val)
      _ -> pure unit) state.searchFieldConfig
  ]

dropdownContent :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
dropdownContent push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility $ boolToVisibility state.isOpen
  , margin $ state.dropdownContentConfig.margin
  , weight 1.0
  , background Color.white900
  , stroke state.dropdownContentConfig.stroke
  , cornerRadii (Corners 16.0 true true false true)
  ]
  [ scrollView
    [ height WRAP_CONTENT
    , maxHeight ((if null state.options 
                     then 60  -- Height for "No options available" message
                     else min 280 (length state.options * 60)))  -- Dynamic height based on options
    
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      (if null state.options 
        then [emptyState]
        else (mapWithIndex (\index option -> optionItem push state option index (length state.options)) state.options))
    ]
  ]

optionsList :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
optionsList push state =
  scrollView
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , maxHeight state.listMaxHeight
  ]
  [
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    (mapWithIndex (\index option -> optionItem push state option index (length state.options)) state.options)
  ]

optionItem :: forall w. (Action -> Effect Unit) -> Config -> DropdownOption -> Int -> Int -> PrestoDOM (Effect Unit) w
optionItem push state option index totalItems =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ state.optionConfig.margin
  , stroke $ state.optionConfig.stroke
  , cornerRadius 8.0
  , alpha $ if option.optionEnabled then 1.0 else 0.5
  ]
  [ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding $ Padding 20 16 20 16
    , background $ if option.selected then Color.blue600 else Color.white900
    , onClick push $ if option.optionEnabled then const $ OnOptionSelect option else const $ NoAction
    , clickable true
    , gravity CENTER_VERTICAL
    ]
    [ 
      linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      [
        textView $
        [ text option.name
        , width MATCH_PARENT
        , height WRAP_CONTENT
        , color Color.black900
        ] <> FontStyle.body1 TypoGraphy
        
        , textView $
          [ text option.description
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , color Color.black700
          , visibility $ boolToVisibility $ option.description /= ""
          , margin $ MarginTop 4
          , singleLine true
          ] <> FontStyle.body20 TypoGraphy
      ]
      , imageView
        [ width (V 24)
        , height (V 24)
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_tick"
        , visibility $ boolToVisibility option.selected
        ]
    ]
    , linearLayout
      [ height (V 1)
      , width MATCH_PARENT
      , visibility $ boolToVisibility state.optionConfig.lineVisibility
      , margin $ Margin 16 0 16 0 
      , background $ if index == totalItems - 1 then Color.white900 else Color.grey900
      ]
      []
  ]

emptyState :: forall w. PrestoDOM (Effect Unit) w
emptyState =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ Padding 16 16 16 16
  , background Color.white900
  ]
  [ textView $
    [ text "No options available"
    , width MATCH_PARENT
    , height WRAP_CONTENT
    , color Color.black700
    , gravity CENTER
    ] <> FontStyle.body1 TypoGraphy
  ]

loadMoreButton :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
loadMoreButton push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ Padding 16 12 16 12
  , background Color.white900
  , visibility $ boolToVisibility state.hasMoreOptions
  , onClick push $ const LoadMoreOptions
  , clickable true
  ]
  [ textView $
    [ text "Load more"
    , width WRAP_CONTENT
    , height WRAP_CONTENT
    , color Color.blue900
    ] <> FontStyle.body20 TypoGraphy
  ]