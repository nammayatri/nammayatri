{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.FavDriverGotIt.View where

import Animation (fadeIn)
import Components.PrimaryButton as PrimaryButton
import Components.FavDriverGotIt.Controller (Action(..), FavDriverConfig)
import Components.SourceToDestination as SourceToDestination
import Data.Array (mapWithIndex, (!!), any, elem, find, head, filter, length)
import Data.Maybe (fromMaybe)
import Data.String (split, Pattern(..))
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth, os)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (getBtnLoader, getKeyInSharedPrefKeys)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, unit, ($), (-), (<<<), (<=), (<>), (==), (<), (/), (/=), not, (&&), map, (<$>), (||),show)
import PrestoDOM (Gravity(..), InputType(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), Accessiblity(..), PrestoDOM, Screen, visibility, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, hint, imageUrl, imageView, inputType, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, relativeLayout, singleLine, stroke, text, textSize, textView, weight, width, multiLineEditText, pattern, maxLines, editText, imageWithFallback, scrollBarY, scrollView, adjustViewWithKeyboard, accessibilityHint, accessibility)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Common.Types.App (FeedbackAnswer, LazyCheck(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Data.Maybe (Maybe(..))
import Debug

view :: forall w. (Action -> Effect Unit) -> FavDriverConfig -> PrestoDOM ( Effect Unit ) w
view push state =
  let _ = spy "prinintg state.primaryButtonConfig -> " state.primaryButtonConfig
  in
  PrestoAnim.animationSet [ fadeIn true ] $
  relativeLayout
  [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , gravity BOTTOM
    , background Color.black9000
  ][  mainView push state 
    , linearLayout
      [height WRAP_CONTENT
      , width MATCH_PARENT
      , padding $ PaddingBottom 16
      , alignParentBottom "true,-1"
      , adjustViewWithKeyboard "true"
      , background Color.white900
      ][PrimaryButton.view (push <<< GotIt ) (state.primaryButtonConfig)]
  ]
  

mainView :: forall w. (Action -> Effect Unit) -> FavDriverConfig -> PrestoDOM (Effect Unit) w
mainView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 16 24 16 57
  , margin (MarginBottom if os == "IOS" then 30 else 0)
  , background Color.white900
  , cornerRadii $ Corners 20.0 true true false false
  , clickable true
  , alignParentBottom "true,-1"
  ][  scrollView 
      [ height if os == "IOS" then (V 500) else WRAP_CONTENT
      , width MATCH_PARENT
      , scrollBarY false 
      ][ linearLayout 
         [ height WRAP_CONTENT
         , width MATCH_PARENT
         , orientation VERTICAL
         , padding $ PaddingBottom if os == "IOS" then 40 else 0
         ][
            textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , accessibilityHint $ state.driverName <> " is now added to your favourite list!"
          , text $ state.driverName <> " is now added to your favourite list!"
          , color Color.black900
          , maxLines 2
          , gravity CENTER
          , margin $ Margin 22 0 22 10
          , visibility VISIBLE
          ] <> FontStyle.h1 LanguageStyle
          , driverInfo state push
          ]
       ]
  ]

------------------------driverInfo--------------------------

driverInfo :: forall w . FavDriverConfig -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
driverInfo state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , padding (PaddingBottom 16)
    , cornerRadius 8.0
    , margin $ MarginTop 10
    ][
      relativeLayout
      [
        height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity CENTER
      ][
        imageView [
          imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_driver_avatar"
        , height $ V 80
        , width $ V 80
        , cornerRadius 50.0
        , margin $ MarginLeft 20
         ],
        imageView [
          imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_greenthumbs_up"
        , height $ V 40
        , width $ V 40
        , cornerRadius 50.0
        , margin $ Margin 85 20 0 0
         ]
      ]
    , linearLayout [
        orientation HORIZONTAL
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginBottom 10
      , gravity CENTER
      ][
        textView $
          [ height $ V 18
          , width $ V 260
          , accessibilityHint $ "I am " <> state.driverName <>", know about me >"
          , accessibility state.accessibility
          , text $ "I am " <> state.driverName <>", know about me >"
          , color Color.blue900
          , maxLines 1
          , gravity CENTER
          , margin $ Margin 0 2 0 7
          ]
      ]
    
    , linearLayout
        [
          height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginBottom 15 
        , orientation HORIZONTAL
        , visibility VISIBLE
        , stroke ("1,"<> Color.yellow900)
        , cornerRadius $ 22.0
        -- , padding $ Padding 3 1 2 1
        , background Color.creamy
        ][
          imageView [
          imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blacktick"
        , height $ V 15
        , width $ V 20
        , visibility VISIBLE 
      ]
      , textView $
          [ height $ V 25
          , width $ V 130
          , accessibilityHint "Marked as Favourite"
          , accessibility state.accessibility
          , text $ "Marked as Favourite"
          , color Color.black900
          , maxLines 1
          , gravity CENTER
          , margin $ Margin 1 1 4 2
          ] 
        ]
    ]

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView =
  textView
  [ width WRAP_CONTENT
  , height $ V 0
  ]