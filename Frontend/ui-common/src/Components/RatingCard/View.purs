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
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Data.Maybe (Maybe(..))
import Debug

view :: forall w. (Action -> Effect Unit) -> RatingCardConfig -> PrestoDOM ( Effect Unit ) w
view push state =
  let _ = spy "key in card" state 
  in 
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
      , margin $ MarginTop 14
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
          , linearLayout
            [
              height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER
            ]
            [
              textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , accessibilityHint "Bad Experience 😔"
            , accessibility state.accessibility
            , text $ "Bad Experience 😔"
            , color Color.black900
            , maxLines 1
            , gravity CENTER
            , visibility if state.data.rating == 1 then VISIBLE else GONE
          ] <> FontStyle.h3 LanguageStyle
            , textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , accessibilityHint "(Select Multiple Items)"
            , accessibility state.accessibility
            , text $ "(Select Multiple Items)"
            , color Color.black700
            , maxLines 1
            , gravity CENTER
            , margin $ Margin 0 7 0 21
            , visibility if state.data.rating == 1 then VISIBLE else GONE
          ]
            ]
            , if state.showFeedbackPill && state.data.rating == 1 then feedbackPillView state push else dummyTextView
            , if state.data.rating == 1 then editTextView state push else dummyTextView
          ]
       ]
  ]

-------------------------------------------------- feedbackPillView ---------------------------------------------------
givePillsImage :: String -> Boolean -> String
givePillsImage txt isSelected = case txt of
                      "Rude Driver" -> if isSelected then "ic_rudedriver_selected" else "ic_rudedriver_unselected"
                      "Unsafe Trip" -> if isSelected then "ic_unsafetrip_selected" else "ic_unsafetrip_unselected"
                      "Charged More" -> if isSelected then "ic_chargedmore_selected" else "ic_chargedmore_unselected"
                      "Late Drop/Pickup" -> if isSelected then "ic_latedrop_selected" else "ic_latedrop_unselected"
                      _ -> "Nothing"


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
        , margin $ MarginBottom 6
        ](map 
            (\item -> 
              let imageName = givePillsImage item.text isSelected
                  isSelected = checkPillSelected item.text state.data.feedbackList item.id
              in
                linearLayout
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , margin $ Margin 13 0 13 0
                  , background Color.white900
                  , orientation VERTICAL
                  ][  
                    imageView 
                      [ imageWithFallback $ fetchImage FF_COMMON_ASSET imageName
                      , height $ V 55
                      , width $ V 68
                      , visibility if state.showProfileImg then VISIBLE else GONE 
                      , gravity CENTER_HORIZONTAL
                      , onClick push $ const $ SelectPill item.text item.id ]
                    ,textView
                      [ height $ V 40
                      , width $ V 58
                      , textSize FontSize.a_12
                      , fontStyle $ FontStyle.medium LanguageStyle
                      , text item.text
                      , accessibilityHint $ item.text <> if isSelected then " : Selected" else " : Un Selected"
                      , accessibility state.accessibility
                      , color Color.black700
                      , maxLines 2
                      , gravity CENTER_HORIZONTAL
                      , margin $ MarginLeft 4
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
  , background Color.white800
  , cornerRadius 8.0
  , orientation HORIZONTAL
  , margin $ MarginBottom 24
  , padding $ Padding 16 16 16 0
  , stroke $ "1," <> Color.grey800
  ][  imageView 
      [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_message_square"
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
      , background Color.white800
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
    ][
      relativeLayout[
        height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER_HORIZONTAL
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
        , margin $ MarginLeft if state.data.rating == 1 then 20 else 0
        , visibility if state.showProfileImg then VISIBLE else GONE ],
        imageView [
          imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_redthumbs_down"
        , height $ V 40
        , width $ V 40
        , cornerRadius 50.0
        , visibility if state.data.rating == 1 then VISIBLE else GONE 
        , margin $ Margin 85 20 0 0]
      ]
    ,linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER
    , background Color.creamy
    , cornerRadius 50.0
    , padding (Padding 7 0 7 1)
    , visibility if state.data.rating == 1 then GONE else VISIBLE
    , stroke ("1,"<> Color.yellow900)
    , margin $ Margin 120 65 0 0
    ][
      imageView [
        imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_heart_red"
        , height $ V 15
        , width $ V 15
        , visibility VISIBLE 
        , margin $ MarginTop 2
      ]
      , textView $
          [ 
           accessibilityHint $ "by "<> show (fromMaybe 0 state.favCount) <>" customers"
          , text  $ "by "<> show (fromMaybe 0 state.favCount) <>" customers"
          , color Color.black900
          , maxLines 1
          , gravity CENTER
          , margin $ Margin 4 0 2 0
          ] <> FontStyle.tags LanguageStyle
        , 
        imageView [
        imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_info"
        , height $ V 17
        , width $ V 17
        , visibility VISIBLE 
        , margin $ MarginTop 2
      ]
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
          , text $ "I am " <> state.data.driverName <>", know about me >"
          , color Color.blue900
          , maxLines 1
          , gravity CENTER
          , margin $ Margin 0 6 0 7
          ]
      ]
      , 
        imageView [
        imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_snowwhiteline"
        , width $ V 370
        , visibility VISIBLE 
      ] 
    , textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , accessibilityHint "Hope you like the ride!"
          , accessibility state.accessibility
          , text $ "Hope you like the ride!"
          , color Color.black900
          , maxLines 1
          , gravity CENTER
          , margin $ MarginTop 15
          , visibility if state.data.rating == 1 then GONE else VISIBLE
          ] <> FontStyle.h3 LanguageStyle
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , margin $ Margin 0 15 0 25
        , visibility if state.data.rating == 1 then GONE else VISIBLE
        ](mapWithIndex (\index item ->
                          linearLayout
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , margin (MarginHorizontal 5 5)
                          , onClick push $ const (Rating item)
                          ][imageView
                              [ height $ V 50
                              , width $ V 50
                              , margin $ MarginTop 5
                              , accessibilityHint (show item <> " thumbs : " <> (if item == state.data.rating then "Selected" else "Un Selected") )
                              , accessibility state.accessibility
                              , imageWithFallback  $ fetchImage FF_COMMON_ASSET $ if item == 5 then if state.data.rating == 5 then "ic_thumbsup_selected" else "ic_thumbsup_unselected" else if state.data.rating == 1 then "ic_thumbsdown_selected" else "ic_thumbsdown_unselected"
                              ]
                          ]) [5, 1])
    , linearLayout
        [
          height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginBottom 15 
        , orientation HORIZONTAL
        , visibility if state.data.rating == 5 && state.isAlreadyFav == Just false then VISIBLE else GONE
        , stroke ("1,"<> if state.data.favDriver == true then Color.red else Color.grey900)
        , cornerRadius $ 22.0
        , padding $ Padding 13 5 8 7
        , background if state.data.favDriver == true then Color.red900Opacity10 else Color.white900
        , onClick push $ const (Favourite)
        ][
          imageView [
          imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_heart_red"
        , height $ V 22
        , width $ V 22
        , visibility VISIBLE 
      ]
      , textView $
          [ height $ V 30
          , width $ V 120
          , accessibilityHint "Mark as Favourite"
          , accessibility state.accessibility
          , text $ "Mark as Favourite"
          , color Color.black900
          , maxLines 1
          , gravity CENTER
          , margin $ MarginLeft 4
          ] 
        ]
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