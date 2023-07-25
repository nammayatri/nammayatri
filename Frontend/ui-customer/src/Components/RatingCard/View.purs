{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RatingCard.View where

import Animation (fadeIn)
import Components.FareBreakUp as FareBreakUp
import Components.PrimaryButton as PrimaryButton
import Components.RatingCard.Controller (Action(..), RatingCardState)
import Components.SourceToDestination as SourceToDestination
import Data.Array (mapWithIndex, (!!))
import Data.Maybe (fromMaybe)
import Data.String (split, Pattern(..))
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth, os)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (getBtnLoader, getKeyInSharedPrefKeys)
import Language.Strings (getString, getKey, LANGUAGE_KEY(..))
import Language.Types (STR(..))
import Prelude (Unit, const, unit, ($), (-), (<<<), (<=), (<>), (==), (<), (/), (/=), not, (&&))
import PrestoDOM (Gravity(..), InputType(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), PrestoDOM, Screen, visibility, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, hint, imageUrl, imageView, inputType, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, relativeLayout, singleLine, stroke, text, textSize, textView, weight, width, multiLineEditText, pattern, maxLines, editText, imageWithFallback, scrollBarY, scrollView, adjustViewWithKeyboard)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Screens.Types(Stage(..), ZoneType(..))
import Common.Types.App


view :: forall w. (Action -> Effect Unit) -> RatingCardState -> PrestoDOM ( Effect Unit ) w
view push state =
  PrestoAnim.animationSet [ fadeIn true ] $
  relativeLayout
  [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , onClick push $ const BackPressed
  ][  linearLayout
      [ orientation VERTICAL
      , height MATCH_PARENT
      , width MATCH_PARENT
      , alignParentBottom "true,-1"
      , gravity BOTTOM
      , background Color.black9000
      ][ currentRatingView push state 
        ] 
      ]
  

currentRatingView :: forall w. (Action -> Effect Unit) -> RatingCardState -> PrestoDOM (Effect Unit) w
currentRatingView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 16 24 16 30
  , background Color.white900
  , cornerRadii $ Corners 20.0 true true false false
  , clickable true
  , adjustViewWithKeyboard "true"
  , onClick push $ const NoAction
  ][  starRatingView state push
    , editTextView state push
    , PrimaryButton.view (push <<< PrimaryButtonAC ) (rideRatingButtonConfig state)
  ]


--------------------------------------------------- horizontalLine ---------------------------------------------------

horizontalLine :: forall w. RatingCardState -> PrestoDOM (Effect Unit) w
horizontalLine state =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.grey900
  , margin $ MarginBottom 24
  ][]

--------------------------------------------------- emptyLayout ---------------------------------------------------

emptyLayout :: forall w. RatingCardState -> PrestoDOM (Effect Unit) w
emptyLayout state =
  linearLayout
  [height $ V 0][]

--------------------------------------------------- editTextView ---------------------------------------------------

editTextView :: forall w. RatingCardState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
editTextView state push =
  linearLayout
  [ height $ V 94
  , width MATCH_PARENT
  , background Color.grey800
  , cornerRadius 8.0
  , orientation HORIZONTAL
  , margin $ MarginBottom 24
  , padding $ Padding 16 16 16 16
  ][  imageView
      [ imageWithFallback "ny_ic_message_square,https://assets.juspay.in/nammayatri/images/common/ny_ic_message_square.png"
      , height $ V 16
      , width $ V 16
      , margin $ if os == "ANDROID" then MarginRight 9 else  Margin 0 6 9 0
      ]
    , (if os == "ANDROID" then editText else multiLineEditText)
      $
      [ height MATCH_PARENT
      , width $ WRAP_CONTENT
      , gravity LEFT
      , padding $ Padding 0 0 0 0
      , textSize FontSize.a_12
      , background Color.grey800
      , color Color.black
      , fontStyle $ FontStyle.regular LanguageStyle
      , hint $ getString HELP_US_WITH_YOUR_FEEDBACK_OPTIONAL
      , weight 1.0
      , pattern "[^\n]*,255"
      , singleLine false
      , onChange push FeedbackChanged
      ]

  ]

--------------------------------------------------- rideRatingButtonConfig ---------------------------------------------------

rideRatingButtonConfig :: RatingCardState -> PrimaryButton.Config
rideRatingButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = (getString SUBMIT_FEEDBACK)
        , color = Color.yellow900
        , textSize = FontSize.a_16
        , width = MATCH_PARENT
        }
      , isClickable = if state.data.rating < 1 then false else true
      , alpha = if state.data.rating < 1 then 0.4 else 1.0
      , margin = (Margin 0 0 0 0)
      , height = (V 48)
      , gravity = CENTER_VERTICAL
      , cornerRadius = 8.0
      , background = Color.black900
      , id = "RideRatingButton"
      , enableLoader = (getBtnLoader "RightRatingButton")
      }
  in primaryButtonConfig'

------------------------starRatingView--------------------------

starRatingView :: forall w . RatingCardState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
starRatingView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (MarginBottom 24)
    , gravity CENTER
    , padding (PaddingVertical 16 16)
    , cornerRadius 8.0
    ][textView
        [ height WRAP_CONTENT
        , width $ V (screenWidth unit - 64)
        , textSize FontSize.a_16
        , text $ getString RATE_YOUR_EXPERIENCE
        , color Color.black800
        , maxLines 2
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , gravity CENTER
        , lineHeight "20"
        , margin (MarginBottom 16)
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        ](mapWithIndex (\index item ->
                          linearLayout
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , margin (MarginHorizontal 5 5)
                          , onClick push $ const (Rating item)
                          ][imageView
                              [ height $ V 30
                              , width $ V 30
                              , imageWithFallback if item <= state.data.rating then "ny_ic_star_active,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_active.png" else "ny_ic_star_inactive,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_inactive.png"
                              ]
                          ]) [1,2,3,4,5])
    ]
