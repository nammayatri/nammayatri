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
import Components.RatingCard.Controller (Action(..), RatingCardState, FeedbackItem(..), feedbackPillData)
import Components.SourceToDestination as SourceToDestination
import Data.Array (mapWithIndex, (!!), any, elem, find, head, filter)
import Data.Maybe (fromMaybe)
import Data.String (split, Pattern(..))
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth, os)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (getBtnLoader, getKeyInSharedPrefKeys)
import Language.Strings (getString, getKey, LANGUAGE_KEY(..))
import Language.Types (STR(..))
import Prelude (Unit, const, unit, ($), (-), (<<<), (<=), (<>), (==), (<), (/), (/=), not, (&&), map, (<$>), (||),show)
import PrestoDOM (Gravity(..), InputType(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), Accessiblity(..), PrestoDOM, Screen, visibility, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, hint, imageUrl, imageView, inputType, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, relativeLayout, singleLine, stroke, text, textSize, textView, weight, width, multiLineEditText, pattern, maxLines, editText, imageWithFallback, scrollBarY, scrollView, adjustViewWithKeyboard, accessibilityHint, accessibility)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Screens.Types(Stage(..), ZoneType(..))
import Common.Types.App
import Services.API(FeedbackAnswer)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Data.Maybe (Maybe(..))

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
    , feedbackPillView state push
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

-------------------------------------------------- feedbackPillView ---------------------------------------------------
feedbackPillView :: forall w. RatingCardState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
feedbackPillView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_VERTICAL
    , margin $ MarginBottom 26
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
                      , accessibility ENABLE
                      , color if isSelected then Color.blue900 else Color.black800
                      , padding $ Padding 12 12 12 12
                      ]
                  ]
            )list1
          ) 
      ) (getFeedbackPillData state.data.rating)
    ) 

getFeedbackPillData :: Int -> Array (Array FeedbackItem)
getFeedbackPillData rating = fromMaybe [] $ (feedbackPillData FunctionCall) !! (rating - 1)
                            
checkPillSelected :: String -> Array FeedbackAnswer -> String -> Boolean
checkPillSelected feedbackItem feedbackList itemId =
  let
    selectedItems = filter (\item -> item.questionId == itemId) feedbackList
  in
    any (\item -> feedbackItem `elem` item.answer) selectedItems


--------------------------------------------------------------------------------------------------------------------

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
      , hint $ getString HELP_US_WITH_YOUR_FEEDBACK_OPTIONAL
      , weight 1.0
      , pattern "[^\n]*,255"
      , singleLine false 
      , onChange push FeedbackChanged 
      ] <> FontStyle.body3 LanguageStyle

  ]

--------------------------------------------------- rideRatingButtonConfig ---------------------------------------------------

rideRatingButtonConfig :: RatingCardState -> PrimaryButton.Config
rideRatingButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = (getString SUBMIT_FEEDBACK)
        , accessibilityHint = "You Rated " <> show state.data.rating <> " stars : Submit Feedback : Button"
        , color = if state.data.rating < 1 && state.data.appConfig.isGradient == "true" then "#696A6F" else state.data.appConfig.primaryTextColor
        , width = MATCH_PARENT
        }
      , isClickable = if state.data.rating < 1 then false else true
      , alpha = if not (state.data.rating < 1) || state.data.appConfig.isGradient == "true" then 1.0 else 0.4
      , margin = (Margin 0 0 0 0)
      , height = (V 48)
      , gravity = CENTER
      , isGradient = if state.data.rating < 1 then false else if state.data.appConfig.isGradient == "true" then true else false
      , cornerRadius = state.data.appConfig.ratingConfig.buttonCornerRadius
      , background = if state.data.rating < 1 && state.data.appConfig.isGradient == "true" then "#F1F1F4" else state.data.appConfig.primaryBackground
      , id = "RideRatingButton"
      , enableLoader = (getBtnLoader "RightRatingButton")
      }
  in primaryButtonConfig'

------------------------
--------------------------

starRatingView :: forall w . RatingCardState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
starRatingView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (MarginBottom 10)
    , gravity CENTER
    , padding (PaddingVertical 16 16)
    , cornerRadius 8.0
    ][ imageView [
        imageWithFallback "ny_ic_driver_avatar,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_avatar.png"
        , height $ V 56
        , width $ V 56
        , cornerRadius 50.0
      ]
    ,textView $
        [ height WRAP_CONTENT
        , width $ V (screenWidth unit - 64)
        , accessibilityHint $ "Rate Your Ride With " <> state.data.driverName
        , accessibility ENABLE
        , text $ getString RATE_YOUR_RIDE_WITH <> state.data.driverName
        , color Color.black800
        , maxLines 2
        , gravity CENTER
        , margin (MarginVertical 8 16)
        ] <> FontStyle.subHeading2 LanguageStyle
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
                              , accessibility ENABLE
                              , imageWithFallback if item <= state.data.rating then "ny_ic_star_active," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_star_active.png" else "ny_ic_star_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_star_inactive.png"
                              ]
                          ]) [1,2,3,4,5])
    , textView
        [ height WRAP_CONTENT
        , width $ V (screenWidth unit - 64)
        , textSize FontSize.a_16
        , text case state.data.rating of  
                1 -> (getString TERRIBLE_EXPERIENCE)
                2 -> (getString POOR_EXPERIENCE)
                3 -> (getString NEEDS_IMPROVEMENT)
                4 -> (getString ALMOST_PERFECT)
                5 -> (getString AMAZING)
                _ -> ""
        , color Color.black800
        , maxLines 2
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , gravity CENTER
        , margin (MarginTop 16)
        ]
    ]
