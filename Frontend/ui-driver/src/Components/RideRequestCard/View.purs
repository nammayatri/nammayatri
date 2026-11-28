module Components.RideRequestCard.View where

import Screens.RideRequestScreen.View
import Prelude
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, frameLayout, orientation, padding, text, textSize, textView, weight, width, onClick, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, stroke, lineHeight, visibility, afterRender, scrollBarY, singleLine ,imageWithFallback, rippleColor, clickable, relativeLayout, alignParentBottom, id, onAnimationEnd, swipeRefreshLayout, onRefresh,shimmerFrameLayout , ellipsize)
import Screens.Types as ST
import Components.RideRequestCard.Controller
import Effect (Effect)
import Helpers.Utils (toStringJSON, fetchImage, FetchImageFrom(..))
import Styles.Colors as Color
import Language.Strings (getString)
import Language.Types (STR(..))
import Font.Size as FontSize
import Screens.RideRequestScreen.Controller as RideRequestScreen
import Font.Style as FontStyle
import Animation as Anim
import Effect.Aff (launchAff)
import Common.Types.App (LazyCheck(..), CategoryListType)
import Screens.RideRequestScreen.ScreenData
import Data.Array (mapWithIndex)
import Data.Int (ceil, fromString, round, toNumber)
import Engineering.Helpers.Commons as EHC
import JBridge (getLayoutBounds)
import Data.Function.Uncurried (runFn1)
import PrestoDOM.Animation as PrestoAnim
import Animation
import PrestoDOM.List as PrestoList


view :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
view push =
  frameLayout
    []
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , padding $ Padding 16 16 16 12
        , margin $ Margin 16 0 16 16
        , cornerRadius 12.0
        , PrestoList.onClickHolder push $ Select
        , id $ EHC.getNewIDWithTag "cardHeight"
        , background Color.white900
        , rippleColor Color.rippleShade
        ]
        [ cardHeaderView push
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER
            ]
            [ linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , padding $ PaddingLeft 2
                ]
                [
                sourceView push
               ,destinationView push
                ]
            , linearLayout
                [ weight 1.0
                , height WRAP_CONTENT
                ]
                []
            , carView push
            ]
        , pill push
        ]
     , expireOverlay push
     , shimmerView push

    ]

cardHeaderView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
cardHeaderView push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    ]
    [ cardHeaderLeft push
    , linearLayout
        [ weight 1.0
        ]
        []
    , cardHeaderRight push
    ]

cardHeaderLeft :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
cardHeaderLeft push =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    ]
    [ imageView
        [ width $ V 30
        , height $ V 30
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_clock_unfilled_blue"
        , gravity CENTER
        , padding $ Padding 0 5 5 5
        , PrestoList.visibilityHolder "visiblePill"
        ]
    , textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , PrestoList.textHolder "time"
          , color Color.black900
          ]
        <> FontStyle.h3 TypoGraphy
    , imageView
        [ width $ V 15
        , height $ V 15
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_ellipse_401"
        , gravity CENTER
        , padding $ Padding 3 5 3 5
        ]
    , textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , PrestoList.textHolder "distance"
          , color Color.black900
          ]
        <> FontStyle.h3 TypoGraphy
    , imageView
        [ width $ V 15
        , height $ V 15
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_ellipse_401"
        , gravity CENTER
        , padding $ Padding 3 5 3 5
        ]
    , textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , PrestoList.textHolder "estimatedDuration"
          , color Color.black900
          ]
         <> FontStyle.h3 TypoGraphy
    ]

cardHeaderRight :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
cardHeaderRight push =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginRight 10
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , textSize FontSize.a_12
          , PrestoList.textHolder "totalAmount"
          , color Color.black900
          ]
        <> FontStyle.h3 TypoGraphy
    ]

pill :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
pill push =
  linearLayout
    [ height WRAP_CONTENT
    , width $ WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 4 4 6 4
    , weight 1.0
    , PrestoList.cornerRadiusHolder "cornerRadius"
    , PrestoList.backgroundHolder "pillColor"
    , PrestoList.visibilityHolder "visiblePill"
    , gravity CENTER_VERTICAL
    ]
    [ imageView
        [ width $ V 20
        , height $ V 20
        , PrestoList.imageUrlHolder "imageType"
        , padding $ Padding 4 0 2 1
        , margin $ MarginRight 4
        ]
    , textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , PrestoList.textHolder "rideType"
          , color Color.white900
          ]
        <> FontStyle.tags TypoGraphy
    ]

carView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
carView push =
  linearLayout
    [ height  WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , padding $ Padding 0 5 0 0
    ]
    [ frameLayout
        [ height $ V 50
        , width WRAP_CONTENT
        , gravity CENTER
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            ]
            [ imageView
                [ height $ V 60
                , width $ V 60
                , PrestoList.imageUrlHolder "image"
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , gravity BOTTOM
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                ]
                [ linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , background Color.lightGrey1
                    , cornerRadius 10.0
                    , gravity CENTER
                    , layoutGravity "center_horizontal"
                    , padding $ Padding 1 1 1 1
                    ]
                    [ textView
                        $ [ PrestoList.textHolder "vehicleType"
                          , width $ V 70
                          , gravity CENTER
                          , color Color.lightGreyBlue2
                          , height WRAP_CONTENT
                          ]
                        <> FontStyle.tags TypoGraphy
                    ]
                ]
            ]
        ]
    ]

expireOverlay :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
expireOverlay push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.grey700
    , alpha 0.6
    , gravity CENTER
    , margin $ Margin 16 0 16 16
    , cornerRadius 12.0
    ,PrestoList.visibilityHolder "overlayVisiblity"
    ]
    [ textView
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString VALIDITY_EXPIRED_STR
          , background Color.black900
          , padding $ Padding 12 2 12 2
          , color $ Color.white900
          , cornerRadius 15.0
          ]
    ]
sourceView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
sourceView push  =
  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity LEFT
      ,margin $ Margin 0 10 0 10
      ]
      [
    imageView
      [ width $ V 15
      , height $ V 15
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_pickup_indicator_green"
      ,padding $ Padding 0 0 2 0
      ]
  , textView $
      [ width $ V 200
      , height WRAP_CONTENT
      , padding $ Padding 2 0 0 2
      , ellipsize true
      , singleLine true
      , color Color.black900
      ,PrestoList.textHolder "source"
      ] <> FontStyle.tags TypoGraphy
  ]


destinationView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destinationView push  =
  linearLayout
      [ width $ V 200
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity LEFT
      , PrestoList.visibilityHolder "visible"
      , margin $ Margin 0 0 0 10
      ]
      [
    imageView
      [ width $ V 15
      , height $ V 15
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_pickup_indicator"
      ,padding $ Padding 0 0 2 0
      ]
  , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , padding $ Padding 2 0 0 2
      , ellipsize true
      , singleLine true
      ,PrestoList.textHolder "destination"
      , color Color.black900
      ] <> FontStyle.tags TypoGraphy
  ]


sfl :: forall w. PrestoDOM (Effect Unit) w -> PrestoDOM (Effect Unit) w
sfl a = shimmerFrameLayout [
  height WRAP_CONTENT
, width WRAP_CONTENT
] [a]


shimmerView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
shimmerView push =
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , padding $ Padding 16 16 16 12
        , margin $ Margin 16 0 16 16
        , cornerRadius 12.0
        , PrestoList.onClickHolder push $ Select
        , id $ EHC.getNewIDWithTag "cardHeight"
        ,PrestoList.visibilityHolder "shimmerVisibility"
        , background Color.white900
        ]
        [ cardHeaderShimmerView push
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER
            ]
            [ linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , padding $ PaddingLeft 2
                ]
                [
                sourceShimmerView push
               ,destinationShimmerView push
                ]
            , linearLayout
                [ weight 1.0
                , height WRAP_CONTENT
                ]
                []
            , carShimmerView push
            ]
        , pillShimmer push
        ]




cardHeaderShimmerView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
cardHeaderShimmerView push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    ]
    [ cardHeaderLeftShimmer push
    , linearLayout
        [ weight 1.0
        ]
        []
    , cardHeaderRightShimmer push
    ]


cardHeaderLeftShimmer :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
cardHeaderLeftShimmer push =
 sfl $ linearLayout
    [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER
      ,PrestoList.visibilityHolder "shimmerVisibility"
    ]
     [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , PrestoList.textHolder "time"
          , color Color.black900
          , color Color.borderGreyColor
          , background Color.borderGreyColor
          ]
        <> FontStyle.h3 TypoGraphy
    ,textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , PrestoList.textHolder "distance"
          , color Color.black900
          , color Color.borderGreyColor
          , background Color.borderGreyColor
          ]
        <> FontStyle.h3 TypoGraphy
    ]

cardHeaderRightShimmer :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
cardHeaderRightShimmer push =
  sfl $ linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , textSize FontSize.a_12
          , PrestoList.textHolder "totalAmount"
          , color Color.borderGreyColor
      , background Color.borderGreyColor
          ]
        <> FontStyle.h3 TypoGraphy
    ]
sourceShimmerView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
sourceShimmerView push  =
    linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ,gravity CENTER
      ,PrestoList.visibilityHolder "shimmerVisibility"
      ]
      [
    sfl $ imageView
      [ width $ V 15
      , height $ V 15
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_pickup_indicator_green"
      ,padding $ Padding 0 0 2 0
      , color Color.borderGreyColor
      , background Color.borderGreyColor
      , visibility GONE
      ]
  ,sfl $ textView $
      [ width $ V 200
      , height WRAP_CONTENT
      , padding $ Padding 2 0 0 2
      ,margin $ Margin 0 0 0 4
      ,PrestoList.textHolder "source"
      , color Color.borderGreyColor
      , background Color.borderGreyColor
      ] <> FontStyle.tags TypoGraphy
  ]

destinationShimmerView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destinationShimmerView push  =
    linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ,gravity CENTER
      , PrestoList.visibilityHolder "visible"
      ,PrestoList.visibilityHolder "shimmerVisibility"

      ]
     [
   sfl $ imageView
      [ width $ V 15
      , height $ V 15
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_pickup_indicator"
      ,padding $ Padding 0 0 2 0
      , color Color.borderGreyColor
      , background Color.borderGreyColor
      , visibility GONE
      ]
  , sfl $ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , padding $ Padding 2 0 0 2
      ,PrestoList.textHolder "destination"
       , color Color.borderGreyColor
      , background Color.borderGreyColor
      ] <> FontStyle.tags TypoGraphy
  ]




carShimmerView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
carShimmerView push =
 sfl $ linearLayout
    [ height $ V 70
    , width WRAP_CONTENT
    , orientation VERTICAL
    , padding $ Padding 0 5 0 0
    ,PrestoList.visibilityHolder "shimmerVisibility"
    ]
    [ frameLayout
        [ height $ V 50
        , width WRAP_CONTENT
        , gravity CENTER
        ]
       [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            ]
            [ imageView
                [ height $ V 60
                , width $ V 60
                , PrestoList.imageUrlHolder "image"
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , gravity BOTTOM
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                ]
                [ linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , background Color.lightGrey1
                    , cornerRadius 10.0
                    , gravity CENTER
                    , layoutGravity "center_horizontal"
                    , padding $ Padding 1 1 1 1
                    ]
                    [ textView
                        $ [ PrestoList.textHolder "vehicleType"
                          , width $ V 70
                          , gravity CENTER
                          , color Color.borderGreyColor
                          , background Color.borderGreyColor
                          , height WRAP_CONTENT
                          ]
                        <> FontStyle.tags TypoGraphy
                    ]
                ]
            ]
        ] ]

pillShimmer :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
pillShimmer push =
 sfl $ linearLayout
    [ height WRAP_CONTENT
    , width $ V 80
    , orientation HORIZONTAL
    , padding $ Padding 3 3 3 3
    , weight 1.0
    , cornerRadius 20.0
    , color Color.borderGreyColor
    , background Color.borderGreyColor
    ,PrestoList.visibilityHolder "visiblePill"
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , PrestoList.textHolder "rideType"
          , color Color.borderGreyColor
          , background Color.borderGreyColor
          ]
        <> FontStyle.tags TypoGraphy
    ]
