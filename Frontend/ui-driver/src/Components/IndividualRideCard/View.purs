{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.IndividualRideCard.View where

import Prelude (Unit, ($), (<<<) , const, (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..),PrestoDOM, linearLayout, clickable,frameLayout, height, width, text, textSize, textView, relativeLayout, orientation, gravity, padding, imageView, imageUrl, background, margin, cornerRadius, shimmerFrameLayout, color, fontStyle, maxLines, ellipsize, layoutGravity, visibility, weight, imageWithFallback)
import Components.IndividualRideCard.Controller(Action(..)) 
import Screens.RideHistoryScreen.Controller (Action(..)) as RideHistoryScreen
import Effect (Effect)
import Screens.Types (IndividualRideCardState)
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.List as PrestoList
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Common.Types.App

view :: forall w .  (RideHistoryScreen.Action  -> Effect Unit)  -> PrestoDOM (Effect Unit) w
view push =
  relativeLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][  shimmerView
    , cardView push
  ]

shimmerView  :: forall w. PrestoDOM (Effect Unit) w 
shimmerView =
    linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding (Padding 16 20 16 20)
  , orientation VERTICAL
  , PrestoList.visibilityHolder "shimmer_visibility"
  , background Color.white900
  ][  rideDetailsShimmerView
    , sourceAndDestinationShimmerView
    , separator
   ]

cardView :: forall w. (RideHistoryScreen.Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w 
cardView push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding (Padding 0 20 0 20)
  , PrestoList.visibilityHolder "card_visibility"
  , orientation VERTICAL
  , clickable true
  , background Color.white900
  , PrestoList.onClickHolder push $ RideHistoryScreen.IndividualRideCardAction <<< Select
  ][  rideDetails
    , sourceAndDestination
    , rideWithDetails
    , separator
   ]

rideDetails :: forall w. PrestoDOM (Effect Unit) w 
rideDetails = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , padding (Padding 16 5 16 16)
  ][  textView
      [ PrestoList.textHolder "date"
      , textSize FontSize.a_14
      , color Color.black700
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , imageView
      [ imageWithFallback "ny_ic_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png"
      , height $ V 5
      , width $ V 5
      , cornerRadius 2.5
      , background Color.black700
      , margin (Margin 6 0 6 0)
      ]
    , textView
      [ PrestoList.textHolder "time"
      , textSize FontSize.a_14
      , color Color.black700
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity RIGHT 
      ][
        textView
          [ text "₹"
          , textSize FontSize.a_20
          , PrestoList.colorHolder "amountColor"
          , fontStyle $ FontStyle.medium LanguageStyle
          ]  
        , textView
          [ PrestoList.textHolder "total_amount"
          , textSize FontSize.a_20
          , PrestoList.colorHolder "amountColor"
          , margin (MarginRight 12)
          , fontStyle $ FontStyle.medium LanguageStyle
          ]
        , textView
          [ text (getString CANCELLED_)
          , background Color.peach
          , cornerRadius 3.0
          , visibility GONE
          , textSize FontSize.a_12
          , color Color.red
          , padding (Padding 10 2 10 2)
          ]
        ]
    ]

sourceAndDestination :: forall w . PrestoDOM (Effect Unit) w 
sourceAndDestination =
  frameLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity LEFT
  , PrestoList.visibilityHolder "card_visibility"
  , padding (Padding 16 0 16 10)
  ][  imageView
      [ imageUrl "ic_line"
      , height MATCH_PARENT
      , width $ V 2
      , gravity LEFT
      , margin (Margin 7 8 0 0)
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  linearLayout
          [ orientation HORIZONTAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , margin (MarginBottom 26)
          ][  imageView
              [ margin(MarginTop 5)
              , imageWithFallback "ny_ic_source_dot,https://assets.juspay.in/nammayatri/images/common/ny_ic_source_dot.png"
              , height $ V 19
              , width $ V 17
              ]
            , textView
              [ PrestoList.textHolder "source"
              , textSize FontSize.a_14
              , padding (Padding 10 0 70 2)
              , fontStyle $ FontStyle.regular LanguageStyle
              , color Color.black700
              , maxLines 1
              , ellipsize true
              ]
            ]
          , linearLayout
            [ orientation HORIZONTAL
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , background Color.white900
            ][  imageView
                [ imageWithFallback "ny_ic_destination,https://assets.juspay.in/nammayatri/images/driver/ny_ic_destination.png"
                , height $ V 16
                , width $ V 14
                ]
              , textView
                [ PrestoList.textHolder "destination"
                , textSize FontSize.a_14
                , layoutGravity "center_vertical"
                , padding (Padding 10 0 70 2)
                , fontStyle $ FontStyle.regular LanguageStyle
                , maxLines 1
                , ellipsize true
                , color Color.black700
                ]
              ]
        ]
      
    ]

rideWithDetails :: forall w. PrestoDOM (Effect Unit) w 
rideWithDetails = 
  textView
    [ PrestoList.textHolder "rideDistance"
    , PrestoList.visibilityHolder "ride_distance_visibility"
    , textSize FontSize.a_14
    , height WRAP_CONTENT
    , fontStyle $ FontStyle.regular LanguageStyle
    , layoutGravity "center_vertical"
    , padding (Padding 16 0 70 20)
    , maxLines 1
    , ellipsize true
    , color Color.black700
    ]

separator :: forall w. PrestoDOM (Effect Unit) w 
separator = 
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.separatorViewColor
  ][]


    
rideDetailsShimmerView :: forall w. PrestoDOM (Effect Unit) w 
rideDetailsShimmerView = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , margin (MarginBottom 16)
  ][ sfl $ linearLayout[
      height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation HORIZONTAL
      , height $ V 17
      , background Color.borderGreyColor
      , cornerRadius 5.0
      , margin (Margin 16 0 6 0)
  ][
    textView
      [ PrestoList.textHolder "date"
      , textSize FontSize.a_14
      , color Color.borderGreyColor
      ]
    ,  textView
      [ PrestoList.textHolder "time"
      , textSize FontSize.a_14
      , margin (MarginLeft 46)
      , color Color.borderGreyColor
      ]]
    , linearLayout
      [
        width MATCH_PARENT
      , height MATCH_PARENT
      , weight 1.0
      ][]
    ,shimmerFrameLayout[
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity RIGHT
    -- , background Color.borderGreyColor
    ][textView
          [ PrestoList.textHolder "total_amount"
          , textSize FontSize.a_14
          , color Color.borderGreyColor
          , background Color.borderGreyColor
          , cornerRadius 5.0
          , width MATCH_PARENT
          , gravity RIGHT
          ]]
    ]



sourceAndDestinationShimmerView :: forall w. PrestoDOM (Effect Unit) w
sourceAndDestinationShimmerView =
  frameLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity LEFT
  , PrestoList.visibilityHolder "shimmer_visibility"
  , padding (PaddingBottom 16)
  ][sfl $  imageView[
    imageWithFallback "ny_ic_shimmer_img,https://assets.juspay.in/nammayatri/images/common/ny_ic_shimmer_img.png"
  , height $ V 57
  , margin (MarginLeft 4)
  , weight 1.0
  , width $ V 12
  ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  linearLayout
          [ orientation HORIZONTAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , margin (Margin 20 0 0 26)
          ][  
            sfl $  linearLayout[
              width MATCH_PARENT
              , height $ V 15
              , background Color.borderGreyColor
              , cornerRadius 5.0
              , margin (MarginLeft 12)
            ][
              textView
              [ PrestoList.textHolder "source"
              , color Color.borderGreyColor
              , cornerRadius 5.0
              ]
            ]]
        , linearLayout
          [ orientation HORIZONTAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , margin (MarginLeft 20)
          , background Color.white900
          ][  
             sfl $ linearLayout [
              height $ V 15
            , width MATCH_PARENT
            , background Color.borderGreyColor
            , cornerRadius 5.0
            , margin (MarginLeft 12)
            ][textView
              [ PrestoList.textHolder "destination"
              , color Color.borderGreyColor
              ]]
            ]
        ]
    ]

sfl :: forall w. PrestoDOM (Effect Unit) w -> PrestoDOM (Effect Unit) w
sfl a = shimmerFrameLayout [
  height WRAP_CONTENT
, width WRAP_CONTENT
] [a]