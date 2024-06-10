module Screens.EarningsScreen.Common.View where

import Screens.EarningsScreen.Daily.Controller
import Screens.EarningsScreen.ScreenData
import Common.Types.App
import Data.Maybe
import Effect
import Prelude
import PrestoDOM
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import Screens.EarningsScreen.Common.Types
import Helpers.Utils
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import JBridge (getWidthFromPercent, getHeightFromPercent)
import PrestoDOM.Types.DomAttributes (__IS_ANDROID)
import Data.FoldableWithIndex
import Debug



headerLayout :: forall action w. (action -> Effect Unit) -> action -> State -> Layout w
headerLayout push action state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding $ Padding 10 (EHC.safeMarginTopWithDefault 13) 10 13
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ getString EARNINGS
          , margin $ MarginLeft 10
          , padding $ PaddingBottom 2
          , color Color.black900
          ]
        <> FontStyle.h3 TypoGraphy
    , linearLayout [ weight 1.0 ] []
    , textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ getString HELP_FAQ
          , padding $ PaddingBottom 2
          , color Color.purple700
          , onClick push $ const $ action
          ]
        <> FontStyle.subHeading3 TypoGraphy
    ]


tabLayout :: forall action a. (action -> Effect Unit) -> action -> EarningsTab -> Boolean -> Boolean -> Layout a
tabLayout push action tabType startAnim resetAnim =
  let selectedIdx = foldlWithIndex (\idx acc item -> if item.type == tabType then idx else acc) 0 tabList
      backgroundWidth = (getWidthFromPercent 50) - 16
  in
  relativeLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  ][ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , padding $ Padding 4 4 4 4
    , cornerRadius 16.0
    ][ textView
        $ [ text $ "Hello Dummy"
          , color Color.white900
          , margin $ MarginVertical 6 6
          ]
        <> FontStyle.tags TypoGraphy
      ]
  , PrestoAnim.animationSet 
    [ Anim.translateInXWithPositioBoth (if selectedIdx == 0 then backgroundWidth else 0) (if selectedIdx == 0 then 0 else backgroundWidth - 8) 250 0 $ startAnim
    , Anim.translateInXWithPositioBoth (if selectedIdx == 0 then 0 else backgroundWidth) (if selectedIdx == 0 then backgroundWidth - 8 else 0) 250 150 $ resetAnim
    ] $ linearLayout
      [ width $ V $ backgroundWidth
      , height WRAP_CONTENT
      , background $ Color.black900
      , cornerRadius 14.0
      , gravity CENTER
      , margin $ Margin 4 4 4 4
      ][ textView
                    $ [ text $ "Hello Dummy"
                      , color Color.black900
                      , margin $ MarginVertical 6 6
                      ]
                    <> FontStyle.tags TypoGraphy]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 4 4 4 4
    , cornerRadius 16.0
    ]
    ( map
        ( \item ->
            let
              isSelected = item.type == tabType
            in
              linearLayout
                ( [ width $ V $ backgroundWidth
                  , height WRAP_CONTENT
                  , cornerRadius 14.0
                  , gravity CENTER
                  ]
                    <> if not isSelected then [ onClick push $ const action ] else []
                )
                [ textView
                    $ [ text $ getString item.string
                      , color if isSelected then Color.white900 else Color.black700
                      , margin $ MarginVertical 6 6
                      ]
                    <> FontStyle.tags TypoGraphy
                ]
        )
        tabList
    )
  ]

rideComponent :: forall action a. (action -> Effect Unit) -> action -> Int -> RideComponent -> Layout a
rideComponent push action idx item =
  let duration = idx * 20
  in
  PrestoAnim.animationSet 
  ([Anim.translateInXWithPosition (getWidthFromPercent $ min (idx * 5) 40) 250 true])
  $ 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.grey900
    , padding $ Padding 12 12 12 12
    , gravity CENTER
    , margin $ MarginBottom 12
    , cornerRadius 8.0
    , onClick push $ const $ action
    ]
    [ imageView
        [ height $ V 24
        , width $ V 25
        , imageWithFallback $ getVehicleVariantImage "SEDAN"
        ]
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ MarginLeft 8
        ]
        [ textView
            $ [ text item.serviceTierType
              , color Color.black900
              ]
            <> FontStyle.paragraphText TypoGraphy
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , margin $ MarginTop 5
            ]
            [ textView
                $ [ text item.date
                  , color Color.black700
                  ]
                <> FontStyle.body3 TypoGraphy
            , linearLayout
                [ height $ V 3
                , width $ V 3
                , cornerRadius 1.5
                , background Color.black700
                , margin $ MarginHorizontal 6 6
                ]
                []
            , textView
                $ [ text item.time
                  , color Color.black700
                  ]
                <> FontStyle.body3 TypoGraphy
            ]
        ]
    , linearLayout
        [ weight 1.0
        , height WRAP_CONTENT
        , gravity RIGHT
        , orientation VERTICAL
        ]
        $ [ textView
              $ [ text item.price
                , color Color.black
                , margin $ MarginBottom 4
                ]
              <> FontStyle.subHeading1 TypoGraphy
          ]
        <> ( map
              ( \tagItem ->
                  linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , background tagItem.background
                    , padding $ Padding 10 3 10 3
                    , cornerRadius 10.0
                    , margin $ MarginBottom 4
                    ]
                    [ textView
                        $ [ text tagItem.text
                          , color tagItem.color
                          ]
                        <> FontStyle.body17 TypoGraphy
                    ]
              )
              item.tags
          )
    ]

shimmerRideComponent :: forall a. Int -> RideComponent -> Layout a
shimmerRideComponent idx item =
  let duration = idx * 20
  in
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.grey900
    , padding $ Padding 12 12 12 12
    , gravity CENTER
    , margin $ MarginBottom 12
    , cornerRadius 8.0
    ]
    [  linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ MarginLeft 8
        ]
        [ sfl (getHeightFromPercent 2) (getWidthFromPercent 50) Color.grey900
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , margin $ MarginTop 5
            ]
            [ sfl (getHeightFromPercent 2) (getWidthFromPercent 30) Color.grey900
            ]
        ]
    , linearLayout
        [ weight 1.0
        , height WRAP_CONTENT
        , gravity RIGHT
        , orientation VERTICAL
        ]
        $ [ sfl (getHeightFromPercent 2) (getWidthFromPercent 20) Color.grey900
          ]
    ]

sfl :: forall a. Int -> Int -> String -> Layout a
sfl height' width' color' =
  shimmerFrameLayout
  [ cornerRadius 50.0
  , stroke $ "1," <> Color.grey900
  , background color'
  , height $ V height'
  , width $ V width'
  ][ linearLayout
      [ cornerRadius 50.0
      , stroke $ "1," <> Color.grey900
      , height $ V height'
      , width $ V width'
      ][]
  ]

tabList :: Array TabList
tabList =
  [ { string: DAILY
    , "type": TAB_DAILY
    }
  , { string: WEEKLY
    , "type": TAB_WEEKLY
    }
  ]

