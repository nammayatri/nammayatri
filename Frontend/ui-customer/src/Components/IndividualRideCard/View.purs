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
import Storage
import Storage

import Common.Types.App (LazyCheck(..))
import Components.IndividualRideCard.Controller (Action(..))
import Components.SeparatorView.View as SeparatorView
import Effect (Effect)
import Engineering.Helpers.Commons (os)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink, getCommonAssetStoreLink)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<>), (<<<), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..),alpha, background, color, cornerRadius, ellipsize, fontStyle, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, margin, maxLines, orientation, padding, relativeLayout, shimmerFrameLayout, stroke, text, textSize, textView, visibility, weight, width, accessibilityHint, accessibility)
import PrestoDOM.List as PrestoList
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.MyRidesScreen.Controller (Action(..)) as Screen
import Screens.Types (IndividualRideCardState, Stage(..), ZoneType(..))
import Styles.Colors as Color

view :: forall w .  (Screen.Action  -> Effect Unit) -> IndividualRideCardState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][  if os == "IOS" then textView [] else shimmerView push state 
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
  , PrestoList.visibilityHolder "cardVisibility"
  , orientation VERTICAL
  , margin $ Margin 16 20 16 4
  , cornerRadius 8.0
  , stroke $ "1,"<>Color.grey900
  , background Color.white900
  ][  zoneView state
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , padding $ Padding 16 20 16 20
      , cornerRadius 8.0
      , orientation VERTICAL
      , background Color.white900
      ][  rideDetails push state
        , separator
        , sourceAndDestination push
        , separator
        , viewDetailsAndRepeatRide push state
      ]
   ]

zoneView :: forall w. IndividualRideCardState ->  PrestoDOM (Effect Unit) w
zoneView state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background Color.blue800
  , padding (PaddingVertical 5 5)
  , orientation HORIZONTAL
  , gravity CENTER
  , cornerRadii $ Corners 8.0 true true false false
  , PrestoList.visibilityHolder "zoneVisibility"
  ][ imageView
     [ width (V 15)
     , height (V 15)
     , margin (MarginRight 6)
     , imageWithFallback "ny_ic_metro_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_metro_white.png"
     ]
   , textView
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , textSize FontSize.a_14
     , text (getString METRO_RIDE)
     , color Color.white900
     ]
   ]

rideDetails :: forall w. (Screen.Action -> Effect Unit) -> IndividualRideCardState ->  PrestoDOM (Effect Unit) w
rideDetails push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , padding $ PaddingHorizontal 16 16
  , PrestoList.onClickHolder push $ Screen.IndividualRideCardActionController <<< OnClick
  , margin (MarginBottom 20)
  ][  textView
      ([ PrestoList.textHolder "date"
      , color Color.black800
      , lineHeight "20"
      ] <> FontStyle.body6 LanguageStyle)
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
      ] <> FontStyle.body6 LanguageStyle)
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
          ][  textView (
              [ PrestoList.textHolder "totalAmount"
              , color Color.black800
              , PrestoList.visibilityHolder "isSuccessfull"
              , gravity CENTER_VERTICAL
              , height WRAP_CONTENT
              ] <> FontStyle.h3 LanguageStyle)
            , linearLayout[
              height WRAP_CONTENT
              , width WRAP_CONTENT
              , padding (Padding 8 4 8 6)
              , PrestoList.visibilityHolder "isCancelled"
              , cornerRadius 3.0
              , background Color.cancelledBg
            ][textView (
              [ text (getString CANCELLED)
              , PrestoList.visibilityHolder "isCancelled"
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              , color Color.red
              ] <> FontStyle.tags LanguageStyle)]
            ]
        ]
    ]

sourceAndDestination :: forall w . (Screen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
sourceAndDestination push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity LEFT
  , PrestoList.visibilityHolder "cardVisibility"
  , PrestoList.onClickHolder push $ Screen.IndividualRideCardActionController <<< OnClick
  , margin $ MarginVertical 20 20
  , padding $ PaddingHorizontal 16 16
  , orientation VERTICAL
  ][ linearLayout
      [ orientation HORIZONTAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      ][  imageView
          [ imageWithFallback $ "ny_ic_source_dot," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_source_dot.png"
          , height $ V 15
          , accessibility ENABLE 
          , accessibilityHint "Source Location is :"
          , width $ V 15
          , margin (MarginTop 2)
          ]
        , textView (
          [ PrestoList.textHolder "source"
          , padding (PaddingHorizontal 12 2)
          , width MATCH_PARENT
          , color Color.black700
          , accessibility ENABLE
          , width MATCH_PARENT
          , maxLines 1
          , ellipsize true
          ] <> FontStyle.paragraphText LanguageStyle)
        ]
    , SeparatorView.view separatorConfig
    , linearLayout
      [ orientation HORIZONTAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , background if os == "IOS" then Color.transparent else Color.white900
      ][  imageView
          [ imageWithFallback $ "ny_ic_loc_red," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_loc_red.png"
          , height $ V 16
          , width $ V 16
          , accessibility ENABLE 
          , accessibilityHint "Destination Location is : "
          , margin (MarginTop 2)
          ]
        , textView (
          [ PrestoList.textHolder "destination"
          , layoutGravity "center_vertical"
          , accessibility ENABLE
          , width MATCH_PARENT
          , padding (PaddingHorizontal 12 2)
          , maxLines 1
          , ellipsize true
          , color Color.black700
          ] <> FontStyle.paragraphText LanguageStyle)
        ]
    ]


separator :: forall w. PrestoDOM (Effect Unit) w
separator =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , padding $ PaddingHorizontal 16 16
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
    textView $
      [ PrestoList.textHolder "date"
      , color Color.borderGreyColor
      ]
    ,  textView $
      [ PrestoList.textHolder "time"
      , margin (MarginLeft 46)
      , color Color.borderGreyColor
      ] <> FontStyle.paragraphText TypoGraphy]
  
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity RIGHT
      , orientation HORIZONTAL
      ][  sfl $ textView $
          [ PrestoList.textHolder "totalAmount"
          , color Color.borderGreyColor
          , background Color.borderGreyColor
          , cornerRadius 5.0
          , width MATCH_PARENT
          , gravity RIGHT
          ] <> FontStyle.paragraphText TypoGraphy
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
    imageWithFallback $ "ny_ic_shimmer_img," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_shimmer_img.png"
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
  ][ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString VIEW_DETAILS
      , accessibilityHint $ "View Details : Button"
      , accessibility ENABLE
      , color Color.blue900
      , padding $ Padding 26 18 50 3
      , PrestoList.onClickHolder push $ Screen.IndividualRideCardActionController <<< OnClick
      ] <> FontStyle.body1 LanguageStyle
    , linearLayout 
      [ width $ V 1
      , height if os == "IOS" then (V 20) else MATCH_PARENT
      , margin $ MarginTop 15
      , background Color.grey900
      ][]
    , textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString REPEAT_RIDE
      , accessibilityHint $ "Repeat Ride : Button"
      , accessibility ENABLE
      , color Color.blue900
      , PrestoList.alphaHolder "alpha"
      , alpha $ if (isLocalStageOn HomeScreen) then 1.0 else 0.5
      , PrestoList.onClickHolder push $ (if (isLocalStageOn HomeScreen) then Screen.IndividualRideCardActionController <<< RepeatRide else Screen.IndividualRideCardActionController <<< NoAction)
      , padding $ PaddingVertical 18 3
      , weight 1.0
      , gravity CENTER
      ] <> FontStyle.body1 LanguageStyle)
  ]

viewDetailsAndRepeatRideShimmer :: forall w. IndividualRideCardState -> PrestoDOM (Effect Unit) w
viewDetailsAndRepeatRideShimmer state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_HORIZONTAL
  , margin $ MarginTop 15
  ][  sfl $ textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString VIEW_DETAILS
      , color Color.grey900
      , background Color.grey900
      , padding $ Padding 10 3 10 3
      , PrestoList.visibilityHolder "shimmerVisibility"
      ] <> FontStyle.body1 LanguageStyle)
    , linearLayout 
      [ width $ V 1
      , height if os == "IOS" then (V 10) else MATCH_PARENT
      , margin $ MarginHorizontal 40 40
      , background Color.grey900
      ][]
    , sfl $ textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString REPEAT_RIDE
      , PrestoList.visibilityHolder "shimmerVisibility"
      , color Color.grey900
      , background Color.grey900
      , padding $ Padding 10 3 10 3
      ] <> FontStyle.body1 LanguageStyle)
  ]

sfl :: forall w. PrestoDOM (Effect Unit) w -> PrestoDOM (Effect Unit) w
sfl a = shimmerFrameLayout [
  height WRAP_CONTENT
, width WRAP_CONTENT
] [a]

separatorConfig :: SeparatorView.Config
separatorConfig = 
  {
    orientation : VERTICAL
  , count : 3
  , height : V 4
  , width : V 2
  , layoutWidth : V 14
  , layoutHeight : V 16
  }