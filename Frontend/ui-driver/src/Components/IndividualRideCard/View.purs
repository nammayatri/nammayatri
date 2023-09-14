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

import Common.Styles.Colors as Colors
import Components.IndividualRideCard.Controller (Action(..))
import Components.SeparatorView.View as SeparatorView
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getCommonAssetStoreLink)
import JBridge (getArray)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getValueFromConfig)
import Prelude (Unit, ($), (<<<), const, (==), (<>))
import Prelude (Unit, ($), (<<<), const, (==), (<>), map)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, maxLines, orientation, padding, relativeLayout, shimmerFrameLayout, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, maxLines, orientation, padding, relativeLayout, shimmerFrameLayout, text, textSize, textView, visibility, weight, width)
import PrestoDOM.List as PrestoList
import PrestoDOM.Properties (cornerRadii, orientation, visibility, width)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.RideHistoryScreen.Controller (Action(..)) as RideHistoryScreen
import Screens.RideHistoryScreen.Controller (Action(..)) as RideHistoryScreen
import Screens.RideSelectionScreen.Controller (Action(..)) as RideSelectionScreen
import Screens.Types (IndividualRideCardState, Tag)
import Styles.Colors as Color

view :: forall w .  (RideHistoryScreen.Action  -> Effect Unit)  -> PrestoDOM (Effect Unit) w
view push =
  relativeLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][  shimmerView
    , cardView push
  ]

selectView :: forall w .  (RideSelectionScreen.Action  -> Effect Unit)  -> PrestoDOM (Effect Unit) w
selectView push =
  relativeLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][  shimmerView
    , selectCardView push
  ]

shimmerView :: forall w. PrestoDOM (Effect Unit) w 
shimmerView =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , PrestoList.visibilityHolder "shimmer_visibility"
  , orientation VERTICAL
  , clickable true
  , background Color.white900
  , margin $ Margin 16 10 16 4
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , padding $ PaddingHorizontal 16 16
      ][  rideDetailsShimmerView
        , separator
        , sourceAndDestinationShimmerView
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , padding $ PaddingBottom 10    
          ][ sfl $ textView
              [ textSize FontSize.a_14
              , height WRAP_CONTENT
              , fontStyle $ FontStyle.regular LanguageStyle
              , color Color.black700
              ]
          ]
      ]
   ]

cardView :: forall w. (RideHistoryScreen.Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w 
cardView push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , PrestoList.visibilityHolder "card_visibility"
  , orientation VERTICAL
  , clickable true
  , background Color.white900
  , margin $ Margin 16 10 16 4
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , PrestoList.onClickHolder push $ RideHistoryScreen.IndividualRideCardAction <<< Select
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , padding $ PaddingHorizontal 16 16
      ][ rideDetails false
        , separator
        , sourceAndDestination
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , margin $ MarginBottom 10   
          ][  tagView $ tagList FunctionCall
            , distanceAndCustomerName
          ]
      ]
   ]

tagView :: forall w. (Array Tag) -> PrestoDOM (Effect Unit) w 
tagView config =
  linearLayout 
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  ](map (\item ->
      linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 26.0
      , background item.background
      , PrestoList.visibilityHolder item.text
      , padding $ Padding 10 4 10 4
      , margin $ MarginRight 5
      ][  imageView
          [ imageWithFallback $ item.image <> ","
          , height $ V 14
          , width $ V 14
          ]
      ]) config)

tagList :: LazyCheck -> Array Tag
tagList _ = [
  {background : Colors.yellow200, image : "ny_ic_tip_icon", visibility : true, text : "tipTagVisibility", textColor : ""},
  {background : Colors.black200, image : "ny_ic_loc_black", visibility : true, text : "gotoTagVisibility", textColor : ""},
  {background : Colors.purple100, image : "ny_ic_disability_purple", visibility : true, text : "purpleTagVisibility", textColor : ""},
  {background : Colors.blue100, image : "ny_ic_star", visibility : true, text : "spLocTagVisibility", textColor : ""}
]

selectCardView :: forall w. (RideSelectionScreen.Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
selectCardView push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , PrestoList.visibilityHolder "card_visibility"
  , orientation VERTICAL
  , clickable true
  , background Color.white900
  , margin $ Margin 16 10 16 4
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , PrestoList.onClickHolder push $ RideSelectionScreen.IndividualRideCardAction <<< Select
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , padding $ PaddingHorizontal 16 16
      ][ rideDetails false
        , separator
        , sourceAndDestination
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , margin $ MarginBottom 10   
          ][  tagView $ tagList FunctionCall
            , distanceAndCustomerName
          ]
      ]
   ]

rideDetails :: Boolean -> forall w. PrestoDOM (Effect Unit) w 
rideDetails showTripId = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , padding $ PaddingVertical 12 12
  ][ linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][ linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          ][ textView $
              [ PrestoList.textHolder "shortRideId"
              , margin $ MarginRight 12
              , color Color.black900
              ] <> FontStyle.body1 TypoGraphy
          ]
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          , margin $ MarginTop 5
          ][  textView
              [ PrestoList.textHolder "date"
              , textSize FontSize.a_14
              , color Color.black700
              , fontStyle $ FontStyle.regular LanguageStyle
              ]
            , imageView
              [ imageWithFallback $ "ny_ic_circle," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_circle.png"
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
          ]
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity RIGHT 
      ][
        textView $
          [ text (getValueFromConfig "currency")
          , PrestoList.colorHolder "amountColor"
          ] <> FontStyle.body11 TypoGraphy
        , textView $
          [ PrestoList.textHolder "total_amount"
          , PrestoList.colorHolder "amountColor"
          , margin (MarginRight 12)
          ] <> FontStyle.body11 TypoGraphy
        , textView $
          [ text (getString CANCELLED_)
          , background Color.peach
          , cornerRadius 3.0
          , visibility GONE
          , color Color.red
          , padding (Padding 10 2 10 2)
          ] <> FontStyle.body3 TypoGraphy
        ]
    ]

sourceAndDestination :: forall w . PrestoDOM (Effect Unit) w 
sourceAndDestination =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity LEFT
  , PrestoList.visibilityHolder "card_visibility"
  , padding $ PaddingVertical 10 10
  , orientation VERTICAL
  ][ linearLayout
      [ orientation HORIZONTAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][  imageView
          [ imageWithFallback $  "ny_ic_source_dot," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_source_dot.png"
          , height $ V 16
          , width $ V 14
          ]
        , textView $
          [ PrestoList.textHolder "source"
          , padding (Padding 10 0 70 2)
          , color Color.black900
          , maxLines 1
          , ellipsize true
          ] <> FontStyle.paragraphText TypoGraphy
        ]
      , SeparatorView.view separatorConfig
      , linearLayout
        [ orientation HORIZONTAL
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , background Color.white900
        ][  imageView
            [ imageWithFallback $ "ny_ic_destination," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_destination.png"
            , height $ V 16
            , width $ V 14
            ]
          , textView $
            [ PrestoList.textHolder "destination"
            , layoutGravity "center_vertical"
            , padding (Padding 10 0 70 2)
            , maxLines 1
            , ellipsize true
            , color Color.black900
            ] <> FontStyle.paragraphText TypoGraphy
          ]
  ]

distanceAndCustomerName :: forall w. PrestoDOM (Effect Unit) w 
distanceAndCustomerName = 
  textView $
    [ PrestoList.textHolder "rideDistance"
    , height WRAP_CONTENT
    , color Color.black700
    ] <> FontStyle.paragraphText LanguageStyle

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
  ][ sfl $ linearLayout 
      [ orientation VERTICAL
      , width MATCH_PARENT
      ][ textView 
         [ PrestoList.textHolder "shortRideId"
         , textSize FontSize.a_18
         , cornerRadius 5.0
         , background Color.borderGreyColor
         , color Color.borderGreyColor
         , margin (MarginBottom 8)
         ]
       , linearLayout
         [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , height $ V 17
          , background Color.borderGreyColor
          , cornerRadius 5.0
          , margin (Margin 0 0 6 0)
         ][ textView
            [ PrestoList.textHolder "date"
            , textSize FontSize.a_14
            , color Color.borderGreyColor
            ]
          ,  textView
            [ PrestoList.textHolder "time"
            , textSize FontSize.a_14
            , margin (MarginLeft 46)
            , color Color.borderGreyColor
          ]
        ]
      ]
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
    ][textView $
          [ PrestoList.textHolder "total_amount"
          , color Color.borderGreyColor
          , background Color.borderGreyColor
          , cornerRadius 5.0
          , width MATCH_PARENT
          , gravity RIGHT
          ] <> FontStyle.paragraphText TypoGraphy]
    ]



sourceAndDestinationShimmerView :: forall w. PrestoDOM (Effect Unit) w
sourceAndDestinationShimmerView =
  frameLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity LEFT
  , PrestoList.visibilityHolder "shimmer_visibility"
  , padding $ PaddingVertical 16 16
  ][sfl $  imageView[
    imageWithFallback $ "ny_ic_shimmer_img," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_shimmer_img.png"
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
          ][ sfl $ linearLayout[
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
            ]
          ]
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

separatorConfig :: SeparatorView.Config
separatorConfig = 
  {
    orientation : VERTICAL
  , count : 2
  , height : V 4
  , width : V 2
  , layoutWidth : V 12
  , layoutHeight : V 12
  }