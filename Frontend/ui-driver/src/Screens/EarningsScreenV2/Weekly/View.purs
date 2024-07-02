module Screens.EarningsScreen.Weekly.View where

import Common.Types.App
import Data.Maybe
import Effect
import Prelude
import PrestoDOM hiding (tabLayout)
import Screens.EarningsScreen.Weekly.Controller
import Screens.EarningsScreen.ScreenData
import Screens.EarningsScreen.Common.Types
import Screens.EarningsScreen.Common.View
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Commons
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import Helpers.Utils
import PrestoDOM.Types.DomAttributes (__IS_ANDROID)
import Data.Array
import Data.FoldableWithIndex
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Mobility.Prelude
import JBridge (getHeightFromPercent, getWidthFromPercent)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Locale.Utils
import Data.Int
import Data.String as DS

screen :: State -> Screen Action State ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "EarningsScreenV2"
  , globalEvents: [ (\push -> pure $ pure unit) ]
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    ]
    $ [ defaultLayout push state
      -- , rideDistanceInfoPopUp push state
      ]

headerLayout :: forall w. (Action -> Effect Unit) -> State -> Layout w
headerLayout state push =
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
          ]
        <> FontStyle.subHeading3 TypoGraphy
    ]

defaultLayout :: forall w. (Action -> Effect Unit) -> State -> Layout w
defaultLayout push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    ]
    [ headerLayout push state
    , earnignsTopView push state
    , weeklyEarningContentView push state
    ]

weeklyEarningContentView push state =
  linearLayout
    [ weight 1.0
    , width MATCH_PARENT
    ]
    [ scrollView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , scrollBarY false
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [ rideHistoryView push state
            , earningSummary push state
            ]
        ]
    ]

earnignsTopView :: forall w. (Action -> Effect Unit) -> State -> Layout w
earnignsTopView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 24
    , gradient $ Linear (if EHC.os == "IOS" then 270.0 else 90.0) [ "#E0D1FF", "#E0D1FF", "#F9F6FF" ]
    , orientation VERTICAL
    , onClick push $ const ToggleInfoView
    ]
    [ tabLayout push ChangeTab TAB_WEEKLY
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , margin $ MarginTop 32
        ]
        [ linearLayout
            [ weight 1.0
            , height MATCH_PARENT
            , gravity CENTER
            ]
            [ imageView
                [ height $ V 16
                , width $ V 16
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER
            ]
            [ dateSelectionView push state
            , textView
                $ [ text "$384"
                  , margin $ MarginTop 14
                  , color Color.black800
                  ]
                <> FontStyle.priceFont TypoGraphy
            , textView
                $ [ text "$72 in Tips Included"
                  , margin $ MarginTop 14
                  , color Color.black800
                  ]
                <> FontStyle.subHeading2 TypoGraphy
            ]
        , linearLayout
            [ height MATCH_PARENT
            , weight 1.0
            , gravity CENTER
            ]
            [ imageView
                [ height $ V 16
                , width $ V 16
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
                ]
            ]
        ]
    , ridesStatsView push state
    ]

dateSelectionView :: forall w. (Action -> Effect Unit) -> State -> Layout w
dateSelectionView push state =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , padding $ Padding 15 4 15 4
    , gravity CENTER_VERTICAL
    , background "#66FFFFFF"
    , cornerRadius 12.0
    ]
    [ imageView
        [ height $ V 16
        , width $ V 16
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_calendar"
        , margin $ MarginRight 6
        ]
    , textView
        $ [ text "Today"
          , color Color.black800
          , margin $ Margin 0 0 6 (if __IS_ANDROID then 2 else 0)
          ]
        <> FontStyle.tags TypoGraphy
    , imageView
        [ height $ V 12
        , width $ V 12
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_down"
        , margin $ MarginLeft 4
        ]
    ]

ridesStatsView :: forall w. (Action -> Effect Unit) -> State -> Layout w
ridesStatsView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.white900
    , margin $ MarginTop 32
    , cornerRadius 12.0
    , padding $ Padding 10 10 10 10
    ]
    ( foldlWithIndex
        ( \idx acc item ->
            let
              islastIndex = idx /= 2
            in
              acc
                <> ( [ linearLayout
                        [ height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , padding $ PaddingHorizontal 18 (if islastIndex then 18 else 0)
                        , gravity CENTER
                        ]
                        $ [ linearLayout
                              [ height WRAP_CONTENT
                              , gravity CENTER
                              , width WRAP_CONTENT
                              , orientation VERTICAL
                              ]
                              [ textView
                                  $ [ text item.title
                                    , color Color.black800
                                    ]
                                  <> FontStyle.tags TypoGraphy
                              , textView
                                  $ [ text item.value
                                    , margin $ MarginTop 4
                                    , color Color.black800
                                    ]
                                  <> FontStyle.tags TypoGraphy
                              ]
                          ]
                    ]
                  )
                <> ( if islastIndex then
                      [ linearLayout
                          [ height WRAP_CONTENT
                          , weight 1.0
                          , gravity CENTER
                          ]
                          [ linearLayout
                              [ width $ V 2
                              , height $ V 30
                              , background Color.grey900
                              ]
                              []
                          ]
                      ]
                    else
                      []
                  )
        )
        []
        [ { title: "Rides", value: "12" }, { title: "Total Ride Dist", value: "12" }, { title: "Total Ride Time", value: "12" } ]
    )

earningsInfoView :: forall w. (Action -> Effect Unit) -> State -> Layout w
earningsInfoView push state =
  PrestoAnim.animationSet
    [ Anim.expandWithDuration 100 (state.props.showInfoView)
    , Anim.collapseWithDuration 100 (not state.props.showInfoView)
    ]
    $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background "#F9F6FF"
        , margin $ Margin 16 16 16 0
        , cornerRadius 12.0
        , padding $ Padding 16 12 16 12
        , gravity CENTER_VERTICAL
        , enableAnimateOnGone true
        , visibility $ boolToVisibility state.props.showInfoView
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            , orientation VERTICAL
            ]
            [ textView
                $ [ text "Your daily earnings will be transferred to your account in 4 days from the date of earning"
                  , color Color.black700
                  ]
                <> FontStyle.body3 TypoGraphy
            , textView
                $ [ text "Learn more"
                  , color Color.purple700
                  ]
                <> FontStyle.body3 TypoGraphy
            ]
        , imageView
            [ height $ V 16
            , width $ V 16
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
            , margin $ MarginLeft 10
            , onClick push $ const ToggleInfoView
            ]
        ]

rideHistoryView :: forall w. (Action -> Effect Unit) -> State -> Layout w
rideHistoryView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , stroke $ "1," <> Color.grey900
    , padding $ Padding 16 12 16 12
    , cornerRadius 8.0
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , alpha 0.4
            ]
            [ imageView
                [ height $ V 24
                , width $ V 24
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
                ]
            ]
        , textView
            $ [ height $ WRAP_CONTENT
              , weight 1.0
              , text $ "May 1 - May 7"
              , gravity CENTER
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , rotation 180.0
            , alpha 0.4
            ]
            [ imageView
                [ height $ V 24
                , width $ V 24
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
                ]
            ]
        ]
    , barGraphView push state
    ]

barGraphView :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
barGraphView push state =
  let
    currWeekMaxEarning = if state.props.currentWeekMaxEarning > 0 then state.props.currentWeekMaxEarning else 1500
  in
    relativeLayout
      [ height $ V 170
      , width MATCH_PARENT
      ]
      [ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , gravity CENTER
          ]
          [ dottedLineView push 4 currWeekMaxEarning
          , dottedLineView push 37 ((currWeekMaxEarning * 2) / 3)
          , dottedLineView push 70 (currWeekMaxEarning / 3)
          , linearLayout
              [ width MATCH_PARENT
              , height $ WRAP_CONTENT
              -- , background Color.grey900
              , orientation VERTICAL
              ]
              [ linearLayout
                  [ height $ V 2
                  , width MATCH_PARENT
                  , background Color.grey900
                  ]
                  []
              , textView
                  $ [ height $ WRAP_CONTENT
                    , width $ MATCH_PARENT
                    , text $ "12"
                    , visibility INVISIBLE
                    ]
                  <> FontStyle.body3 TypoGraphy
              , textView
                  $ [ height $ WRAP_CONTENT
                    , width $ MATCH_PARENT
                    , text $ "MON"
                    , visibility INVISIBLE
                    ]
                  <> FontStyle.body3 TypoGraphy
              ]
          ]
      , linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , weight 1.0
          , background Color.transparent
          , orientation HORIZONTAL
          , gravity BOTTOM
          -- , padding $ PaddingTop $ getHeightFromPercent 1 TODO: FIX MAX Length
          ]
          $ [ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , weight 5.0
                , background Color.transparent
                , orientation HORIZONTAL
                , gravity BOTTOM
                ]
                (mapWithIndex (\index item -> (barView push index item state (if item.noOfRides > 0 then VISIBLE else INVISIBLE))) state.props.currWeekData)
            , linearLayout
                [ weight 1.0
                , width MATCH_PARENT
                , gravity CENTER
                ]
                [ textView
                    $ [ height WRAP_CONTENT
                      , width WRAP_CONTENT
                      , margin $ MarginLeft 4
                      , visibility INVISIBLE
                      , text $ "₹1200"
                      ]
                    <> FontStyle.paragraphText TypoGraphy
                ]
            ]
      ]

dottedLineView :: forall w. (Action -> Effect Unit) -> Int -> Int -> PrestoDOM (Effect Unit) w
dottedLineView push margintop earnings =
  linearLayout
    [ weight 1.0
    , width MATCH_PARENT
    , gravity CENTER
    ]
    [ imageView
        [ height $ V 2
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_dotted_line"
        , weight 5.0
        ]
    , textView
        $ [ height $ WRAP_CONTENT
          , width $ WRAP_CONTENT
          , weight 1.0
          , margin $ MarginLeft 4
          , text $ "$" <> (show earnings)
          ]
        <> FontStyle.paragraphText TypoGraphy
    ]

barView :: forall w. (Action -> Effect Unit) -> Int -> ST.WeeklyEarning -> State -> Visibility -> PrestoDOM (Effect Unit) w
barView push index item state vis =
  let
    selectedIndex = state.props.selectedBarIndex

    setMargin = case index of
      0 -> MarginHorizontal 3 6
      6 -> MarginHorizontal 3 (EHC.screenWidth unit / 8)
      _ -> MarginHorizontal 6 6
  in
    linearLayout
      [ weight 1.0
      , height MATCH_PARENT
      -- , margin setMargin
      , orientation VERTICAL
      , alignParentBottom "true,-1"
      , gravity BOTTOM
      , visibility vis
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , gravity CENTER
          ]
          [ PrestoAnim.animationSet
              [ Anim.expandWithDuration 1000 true
              ]
              $ linearLayout
                  [ height $ V if item.percentLength > 0.0 then (ceil item.percentLength) else 1
                  , width $ V $ getWidthFromPercent 7
                  , background if item.percentLength > 20.0 then Color.purple700 else Color.red900
                  , cornerRadius 4.0
                  , pivotY 1.0
                  ]
                  []
          ]
      , textView
          $ [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , text $ item.rideDate
            , color Color.black700
            , gravity CENTER
            ]
          <> FontStyle.body3 TypoGraphy
      , textView
          $ [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , text $ "MON"
            , gravity CENTER
            , singleLine true
            , color Color.black700
            , margin $ if (getLanguageLocale languageKey == "KN_IN") then MarginBottom 0 else MarginBottom 2
            ]
          <> FontStyle.body3 TypoGraphy
      ]

rideDistanceInfoPopUp :: forall w. (Action -> Effect Unit) -> State -> Layout w
rideDistanceInfoPopUp push state =
  PrestoAnim.animationSet
    [ Anim.fadeInWithDuration 100 (state.props.rideDistanceInfoPopUp)
    ]
    $ linearLayout
        [ width MATCH_PARENT
        , height $ V $ EHC.screenHeight unit
        , background Color.blackLessTrans
        , onClick push $ const RemovePopup
        , gravity BOTTOM
        , orientation VERTICAL
        , visibility $ boolToVisibility $ state.props.rideDistanceInfoPopUp
        ]
        [ linearLayout
            [ height $ V $ getHeightFromPercent 30
            , width MATCH_PARENT
            , background Color.white900
            , orientation VERTICAL
            , padding $ Padding 16 20 16 (EHC.safeMarginBottomWithDefault 16)
            , cornerRadii $ Corners 24.0 true true false false
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                ]
                [ textView
                    $ [ text "Ride Distance"
                      , color Color.black900
                      , margin $ MarginBottom 4
                      , weight 1.0
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                , linearLayout
                    [ height $ V 24
                    , width $ V 24
                    , gravity CENTER
                    , onClick push $ const RemovePopup
                    ]
                    [ imageView
                        [ height $ V 16
                        , width $ V 16
                        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
                        ]
                    ]
                ]
            , linearLayout
                [ height $ V 1
                , width MATCH_PARENT
                , margin $ MarginVertical 16 10
                , background Color.grey900
                ]
                []
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                ( map
                    ( \item ->
                        linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , margin $ MarginVertical 10 10
                          ]
                          [ textView
                              $ [ text item.text
                                , color Color.black800
                                , weight 1.0
                                ]
                              <> FontStyle.body6 TypoGraphy
                          , textView
                              $ [ text item.value
                                , color Color.black800
                                ]
                              <> FontStyle.body6 TypoGraphy
                          ]
                    )
                    [ { text: "The Pickup distance"
                      , value: "10 mi"
                      }
                    , { text: "Ride Dist."
                      , value: "60 mi"
                      }
                    ]
                )
            , linearLayout
                [ height $ V 1
                , width MATCH_PARENT
                , margin $ MarginTop 10
                , background Color.grey900
                ]
                []
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , margin $ MarginTop 20
                , gravity CENTER
                ]
                [ imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
                    , height $ V 16
                    , width $ V 16
                    , margin $ MarginRight 5
                    ]
                , textView
                    $ [ text "The pickup distance is the distance you travelled to the pickup point."
                      , color Color.black800
                      ]
                    <> FontStyle.paragraphText TypoGraphy
                ]
            ]
        ]

earningSummary push state =
  let
    showAdj = state.data.adjustmentRotation == 180.0
  in
    linearLayout
      [ height $ WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ MarginHorizontal 16 16
      , padding $ Padding 16 16 16 16
      , stroke $ "1," <> Color.grey900
      , cornerRadius 8.0
      ]
      [ textView
          $ [ height $ WRAP_CONTENT
            , width WRAP_CONTENT
            , text "Earnings Summary"
            , color Color.black800
            ]
          <> FontStyle.subHeading1 TypoGraphy
      , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 0) [] true
      , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 17) [] true
      , dropDown push "Adjustments" state.data.adjustmentRotation state.data.prevAdjustmentRotation ShowAdjustments
      , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 13) [ Anim.expandWithDuration 150 showAdj, Anim.collapseWithDuration 150 (not showAdj) ] showAdj
      , dropDown push "Tips" 270.0 270.0 ShowTips
      -- , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 0) [] Map Over Tips
      -- , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 0) []
      -- , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 0) []
      -- , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 0) []
      -- , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 0) []
      -- , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 0) []
      , linearLayout
          [ height $ V 1
          , background Color.grey900
          , width MATCH_PARENT
          , margin $ MarginTop 17
          ]
          []
      , tableViewCell state (FontStyle.body22 TypoGraphy) Color.green900 (MarginTop 17) [] true
      ]

dropDown push content rotate prevRotate action =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , margin $ MarginTop 17
    , onClick push $ const action
    ]
    [ textView
        $ [ height $ WRAP_CONTENT
          , weight 1.0
          , text $ content
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , PrestoAnim.animationSet
        ( if rotate == 270.0 then
            []
          else
            [ Anim.rotateToDegWithDuration 150 (ceil prevRotate) (ceil rotate) ]
        )
        $ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , alpha 0.4
            ]
            [ imageView
                [ height $ V 10
                , width $ V 10
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_down"
                ]
            ]
    ]

tableViewCell state fontStyle col marg anim vis =
  PrestoAnim.animationSet anim
    $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin marg
        , visibility $ boolToVisibility vis
        , enableAnimateOnGone true
        ]
        [ textView
            $ [ text "Passenger Payment"
              , color col
              , weight 1.0
              , height WRAP_CONTENT
              ]
            <> fontStyle
        , textView
            $ [ text "Passenger Payment"
              , color col
              ]
            <> fontStyle
        ]
