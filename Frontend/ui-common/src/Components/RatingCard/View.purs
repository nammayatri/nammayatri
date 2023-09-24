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
import Components.PrimaryButton as PrimaryButton
import Components.RatingCard.Controller (Action(..), RatingCardConfig, FeedbackItem(..))
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
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Data.Maybe (Maybe(..))

view :: forall w. (Action -> Effect Unit) -> RatingCardConfig -> PrestoDOM ( Effect Unit ) w
view push state =
  PrestoAnim.animationSet [ fadeIn true ] $
  relativeLayout
  [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , gravity BOTTOM
    , onClick push $ const BackPressed
    , background Color.black9000
  ][  currentRatingView push state 
    , linearLayout
      [height WRAP_CONTENT
      , width MATCH_PARENT
      , padding $ PaddingBottom 16
      , alignParentBottom "true,-1"
      , adjustViewWithKeyboard "true"
      , background Color.white900
      ][PrimaryButton.view (push <<< PrimaryButtonAC ) (state.primaryButtonConfig)]
  ]
  

currentRatingView :: forall w. (Action -> Effect Unit) -> RatingCardConfig -> PrestoDOM (Effect Unit) w
currentRatingView push state = 
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
  , onClick push $ const NoAction
  ][  scrollView 
      [ height if os == "IOS" then (V 500) else WRAP_CONTENT
      , width MATCH_PARENT
      , scrollBarY false 
      ][ linearLayout 
         [ height WRAP_CONTENT
         , width MATCH_PARENT
         , orientation VERTICAL
         , padding $ PaddingBottom if os == "IOS" then 40 else 0
         ][ starRatingView state push
          , if state.showFeedbackPill then feedbackPillView state push else dummyTextView
          , editTextView state push
          ]
       ]
  ]

-------------------------------------------------- feedbackPillView ---------------------------------------------------
feedbackPillView :: forall w. RatingCardConfig -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
feedbackPillView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_VERTICAL
    , padding (PaddingBottom 16)
    ](map  
      (\list1 ->  
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_HORIZONTAL
        , margin $ MarginBottom 6
        ](map 
            (\item -> 
              let isSelected = checkPillSelected item.text state.data.feedbackList item.id
              in
                linearLayout
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , cornerRadius 20.0
                  , stroke ("1," <> if isSelected then Color.blue900 else Color.grey900)
                  , margin $ Margin 6 6 6 6
                  , background if isSelected then Color.blue600 else Color.white900
                  , onClick push $ const $ SelectPill item.text item.id
                  ][ textView
                      [ height WRAP_CONTENT
                      , textSize FontSize.a_12
                      , fontStyle $ FontStyle.medium LanguageStyle
                      , text item.text
                      , accessibilityHint $ item.text <> if isSelected then " : Selected" else " : Un Selected"
                      , accessibility state.accessibility
                      , color if isSelected then Color.blue900 else Color.black800
                      , padding $ Padding 12 12 12 12
                      ]
                  ]
            )list1
          ) 
      ) (getFeedbackPillData state.data.rating state.feedbackPillData)
    ) 

getFeedbackPillData :: Int -> Array (Array (Array FeedbackItem)) -> Array (Array FeedbackItem)
getFeedbackPillData rating feedbackPillData = fromMaybe [] $ (feedbackPillData) !! (rating - 1)
                            
checkPillSelected :: String -> Array FeedbackAnswer -> String -> Boolean
checkPillSelected feedbackItem feedbackList itemId =
  let
    selectedItems = filter (\item -> item.questionId == itemId) feedbackList
  in
    any (\item -> feedbackItem `elem` item.answer) selectedItems


--------------------------------------------------------------------------------------------------------------------

--------------------------------------------------- editTextView ---------------------------------------------------

editTextView :: forall w. RatingCardConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
editTextView state push =
  linearLayout
  [ height $ V 94
  , width MATCH_PARENT
  , background Color.grey800
  , cornerRadius 8.0
  , orientation HORIZONTAL
  , margin $ MarginBottom 24
  , padding $ Padding 16 16 16 0
  ][  imageView 
      [ imageWithFallback $ "ny_ic_message_square," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_message_square.png"
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
      , background Color.grey800
      , color Color.black 
      , hint state.feedbackPlaceHolder
      , weight 1.0
      , pattern "[^\n]*,255"
      , singleLine false 
      , onChange push FeedbackChanged 
      ] <> FontStyle.body3 LanguageStyle

  ]

------------------------starRatingView--------------------------

starRatingView :: forall w . RatingCardConfig -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
starRatingView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , padding (PaddingBottom 16)
    , cornerRadius 8.0
    ][ imageView [
        imageWithFallback $ "ny_ic_driver_avatar,"<> (getAssetStoreLink FunctionCall)<>"ny_ic_driver_avatar.png"
        , height $ V 56
        , width $ V 56
        , cornerRadius 50.0
        , visibility if state.showProfileImg then VISIBLE else GONE
      ]
    , linearLayout [
        orientation HORIZONTAL
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginBottom 10
      , gravity CENTER
      ][
        textView $
          [ height WRAP_CONTENT
          , width $ V (screenWidth unit - 64)
          , accessibilityHint "Rate Your Ride"
          , accessibility state.accessibility
          , text $ state.title
          , color Color.black800
          , padding $ PaddingBottom 4
          , maxLines 2
          , gravity CENTER
          ] <> FontStyle.h3 LanguageStyle
      , imageView [
          width $ V 16 
        , height $ V 16 
        , accessibility DISABLE
        , onClick push $ const BackPressed
        , gravity CENTER
        , visibility state.closeImgVisible
        , imageWithFallback $ "ny_ic_cancel_unfilled," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_cancel_unfilled.png"
        ]
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
                              [ height $ V 35
                              , width $ V 35
                              , accessibilityHint (show item <> " Star : " <> (if item <= state.data.rating then "Selected" else "Un Selected") )
                              , accessibility state.accessibility
                              , imageWithFallback if item <= state.data.rating then "ny_ic_star_active," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_star_active.png" else "ny_ic_star_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_star_inactive.png"
                              ]
                          ]) [1,2,3,4,5])
    , if state.showFeedbackPill then feedbackBasedOnRatingView state push else dummyTextView
    ]

feedbackBasedOnRatingView :: forall w . RatingCardConfig -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
feedbackBasedOnRatingView state push = 
    textView
        [ height WRAP_CONTENT
        , width $ V (screenWidth unit - 64)
        , textSize FontSize.a_16
        , text $ fromMaybe "" $ state.overallFeedbackArray !! (state.data.rating-1)
        , color Color.black800
        , maxLines 2
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , gravity CENTER
        , margin (MarginTop 16)
        ]
dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView =
  textView
  [ width WRAP_CONTENT
  , height $ V 0
  ]