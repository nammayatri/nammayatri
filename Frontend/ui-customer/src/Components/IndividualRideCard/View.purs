{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.IndividualRideCard.View where

import Common.Types.App

import Components.IndividualRideCard.Controller (Action(..))
import Effect (Effect)
import Engineering.Helpers.Commons (os)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..), VIEW_DETAILS)
import Prelude (Unit, ($), (<>), (<<<), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, color, cornerRadius, ellipsize, fontStyle, frameLayout, gravity, height, imageUrl, imageView, layoutGravity, linearLayout, margin, maxLines, orientation, padding, relativeLayout, shimmerFrameLayout, text, textSize, textView, weight, width, stroke, lineHeight, imageWithFallback)
import PrestoDOM.List as PrestoList
import Screens.MyRidesScreen.Controller (Action(..)) as Screen
import Screens.Types (IndividualRideCardState, Stage(..))
import Storage
import Styles.Colors as Color
import Storage
import Constant.Test as Id

view :: forall w .  (Screen.Action  -> Effect Unit) -> IndividualRideCardState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , Id.testId $ Id.Component Id.individualRideCard
  ][  shimmerView push state
    , cardView push state
  ]


shimmerView  :: forall w. (Screen.Action  -> Effect Unit) -> IndividualRideCardState -> PrestoDOM (Effect Unit) w 
shimmerView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ Padding 16 20 16 20
  , margin $ Margin 16 20 16 4
  , cornerRadius 8.0
  , stroke $ "1,"<>Color.grey900
  , orientation VERTICAL
  , PrestoList.visibilityHolder "shimmerVisibility"
  ][  rideDetailsShimmerView state
    , separator
    , sourceAndDestinationShimmerView state
    , separator
    , viewDetailsAndRepeatRideShimmer state
   ]

cardView :: forall w. (Screen.Action -> Effect Unit) -> IndividualRideCardState -> PrestoDOM (Effect Unit) w 
cardView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ Padding 16 20 16 20
  , PrestoList.visibilityHolder "cardVisibility"
  , orientation VERTICAL
  , margin $ Margin 16 20 16 4
  , cornerRadius 8.0
  , stroke $ "1,"<>Color.grey900
  , background Color.white900
  ][  rideDetails state
    , separator
    , sourceAndDestination  
    , separator
    , viewDetailsAndRepeatRide push state
   ]

rideDetails :: forall w. IndividualRideCardState ->  PrestoDOM (Effect Unit) w 
rideDetails state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , margin (MarginBottom 20)
  ][  textView
      ([ PrestoList.textHolder "date"
      , textSize FontSize.a_14
      , color Color.black800
      , lineHeight "20"
      , fontStyle $ FontStyle.semiBold LanguageStyle
      ])
    , linearLayout
      [ 
       height $ V 5
      , width $ V 5
      , cornerRadius 2.5
      , background Color.black600
      , margin (Margin 8 3 8 0)
      ][]
    , textView
      ([ PrestoList.textHolder "time"
      , color Color.black800
      , textSize FontSize.a_14 
      , lineHeight "20"
      , fontStyle $ FontStyle.semiBold LanguageStyle
      ] )
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity RIGHT 
      ][  linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
    
          , layoutGravity "center_vertical"
          ][  textView
              [ PrestoList.textHolder "totalAmount"
              , textSize FontSize.a_18
              , color Color.black800
              , PrestoList.visibilityHolder "isSuccessfull"
              , gravity CENTER_VERTICAL
              , fontStyle $ FontStyle.semiBold LanguageStyle
              , height WRAP_CONTENT
              ]
            , linearLayout[
              height WRAP_CONTENT
              , width WRAP_CONTENT
              , padding (Padding 8 4 8 6)
              , PrestoList.visibilityHolder "isCancelled"
              , cornerRadius 3.0
              , background Color.cancelledBg
            ][textView
              [ text (getString CANCELLED)
              , PrestoList.visibilityHolder "isCancelled"
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              , textSize FontSize.a_12
              , lineHeight "16"
              , color Color.red
              , fontStyle $ FontStyle.medium LanguageStyle
              ]]
            ]
        ]
    ]

sourceAndDestination :: forall w . PrestoDOM (Effect Unit) w 
sourceAndDestination =
  frameLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity LEFT
  , PrestoList.visibilityHolder "cardVisibility"
  , margin $ MarginVertical 20 20
  ][  imageView
      [ imageUrl "ic_line"
      , height MATCH_PARENT
      , width $ V 2
      , gravity LEFT
      , margin (Margin 7 12 0 0)
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  linearLayout
          [ orientation HORIZONTAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , margin (MarginBottom 20)
          ][  imageView
              [ imageWithFallback "ny_ic_source_dot,https://assets.juspay.in/nammayatri/images/common/ny_ic_source_dot.png"
              , height $ V 16
              , width $ V 16
              , margin (MarginTop 2)
              ]
            , textView
              [ PrestoList.textHolder "source"
              , textSize FontSize.a_14
              , padding (PaddingHorizontal 12 2)
              , fontStyle $ FontStyle.regular LanguageStyle
              , width MATCH_PARENT
              , color Color.black700
              , width MATCH_PARENT
              , maxLines 1
              , ellipsize true
              ]
            ]
        , linearLayout
          [ orientation HORIZONTAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , background if os == "IOS" then Color.transparent else Color.white900
          ][  imageView
              [ imageWithFallback "ny_ic_loc_red,https://assets.juspay.in/nammayatri/images/common/ny_ic_loc_red.png"
              , height $ V 16
              , width $ V 16
              , margin (MarginTop 2)
              ]
            , textView
              [ PrestoList.textHolder "destination"
              , textSize FontSize.a_14
              , layoutGravity "center_vertical"
              , width MATCH_PARENT
              , padding (PaddingHorizontal 12 2)
              , maxLines 1
              , ellipsize true
              , fontStyle $ FontStyle.regular LanguageStyle
              , color Color.black700
              ]
            ]
        ]
    ]


separator :: forall w. PrestoDOM (Effect Unit) w 
separator = 
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.grey900
  ][]


    
rideDetailsShimmerView :: forall w. IndividualRideCardState -> PrestoDOM (Effect Unit) w 
rideDetailsShimmerView state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , margin (MarginBottom 20)
  ][ sfl $ linearLayout[
      height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation HORIZONTAL
      , height $ V 17
      , background Color.borderGreyColor
      , cornerRadius 5.0
      , margin (MarginRight 6)
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
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity RIGHT
      , orientation HORIZONTAL
      ][  sfl $ textView
          [ PrestoList.textHolder "totalAmount"
          , textSize FontSize.a_14
          , color Color.borderGreyColor
          , background Color.borderGreyColor
          , cornerRadius 5.0
          , width MATCH_PARENT
          , gravity RIGHT
          ]
        ]
    ]



sourceAndDestinationShimmerView :: forall w. IndividualRideCardState -> PrestoDOM (Effect Unit) w
sourceAndDestinationShimmerView state =
  frameLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity LEFT
  , PrestoList.visibilityHolder "shimmerVisibility"
  , margin $ MarginVertical 20 20
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


viewDetailsAndRepeatRide :: forall w. (Screen.Action  -> Effect Unit) -> IndividualRideCardState -> PrestoDOM (Effect Unit) w 
viewDetailsAndRepeatRide push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginTop 15
  ][ textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString VIEW_DETAILS
      , color Color.blue900
      , textSize FontSize.a_14
      , fontStyle $ FontStyle.medium LanguageStyle
      , padding $ Padding 10 3 10 3
      , PrestoList.onClickHolder push $ Screen.IndividualRideCardActionController <<< OnClick
      ]
    , linearLayout 
      [ width $ V 1
      , height if os == "IOS" then (V 20) else MATCH_PARENT
      , margin $ MarginHorizontal 40 40
      , background Color.grey900
      ][]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString REPEAT_RIDE
      , color Color.blue900
      , textSize FontSize.a_14
      , fontStyle $ FontStyle.medium LanguageStyle
      , PrestoList.alphaHolder "alpha"
      , PrestoList.onClickHolder push $ (if (isLocalStageOn HomeScreen) then Screen.IndividualRideCardActionController <<< RepeatRide else Screen.IndividualRideCardActionController <<< NoAction)
      , padding $ Padding 10 3 10 3
      ]
  ]

viewDetailsAndRepeatRideShimmer :: forall w. IndividualRideCardState -> PrestoDOM (Effect Unit) w 
viewDetailsAndRepeatRideShimmer state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_HORIZONTAL
  , margin $ MarginTop 15
  ][  sfl $ textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString VIEW_DETAILS
      , color Color.grey900
      , background Color.grey900
      , textSize FontSize.a_14
      , fontStyle $ FontStyle.medium LanguageStyle
      , padding $ Padding 10 3 10 3
      , PrestoList.visibilityHolder "shimmerVisibility"
      ]
    , linearLayout 
      [ width $ V 1
      , height if os == "IOS" then (V 10) else MATCH_PARENT
      , margin $ MarginHorizontal 40 40
      , background Color.grey900
      ][]
    , sfl $ textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString REPEAT_RIDE
      , PrestoList.visibilityHolder "shimmerVisibility"
      , color Color.grey900
      , background Color.grey900
      , textSize FontSize.a_14
      , fontStyle $ FontStyle.medium LanguageStyle
      , padding $ Padding 10 3 10 3
      ]
  ]

sfl :: forall w. PrestoDOM (Effect Unit) w -> PrestoDOM (Effect Unit) w
sfl a = shimmerFrameLayout [
  height WRAP_CONTENT
, width WRAP_CONTENT
] [a]