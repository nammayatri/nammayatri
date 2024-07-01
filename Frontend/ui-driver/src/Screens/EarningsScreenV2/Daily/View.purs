module Screens.EarningsScreen.Daily.View where

import Common.Types.App
import Data.Array
import Data.FoldableWithIndex
import Data.Maybe
import Debug
import Effect
import Helpers.Utils
import Mobility.Prelude
import Prelude
import PrestoDOM hiding (tabLayout)
import Screens.EarningsScreen.Common.Types
import Screens.EarningsScreen.Common.View
import Screens.EarningsScreen.Controller
import Screens.EarningsScreen.ScreenData
import Services.API

import Animation as Anim
import Effect.Aff (launchAff)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (flowRunner)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.API as API
import JBridge (getHeightFromPercent, getWidthFromPercent)
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Types.DomAttributes (__IS_ANDROID)
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: State -> Screen Action State ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "EarningsScreenV2"
  , globalEvents: [ (\push -> fetchRideSummary push initialState) ]
  , eval
  }

fetchRideSummary :: (Action -> Effect Unit) -> State -> Effect (Effect Unit)
fetchRideSummary push state = do
  void $ launchAff $ flowRunner defaultGlobalState $ do
    let currentDate = getcurrentdate ""
    resp <-  API.callApi (GetRidesSummaryListReq ["2024-06-25"])
    let _ = spy "GetRidesSummaryListResp" resp
    pure unit
  pure $ pure unit
    -- liftFlowBT $ push $ RideSummaryAPIResponseAction rideSummaryResp.list (spy "printing currentDate" currentDate) datesList

view :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    ] $
    [ defaultLayout push state
    , rideDistanceInfoPopUp push state
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
    , earningsInfoView push state
    , rideHistoryView push state
    ]

earnignsTopView :: forall w. (Action -> Effect Unit) -> State -> Layout w
earnignsTopView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 24
    , gradient $ Linear (if EHC.os == "IOS" then 270.0 else 90.0) [ "#E0D1FF", "#E0D1FF", "#F9F6FF" ]
    , orientation VERTICAL
    ]
    [ tabLayout push ChangeTab TAB_DAILY
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
    , weight 1.0
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    ]
    [ textView
        $ [ text "Ride History"
          , color Color.black800
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
        [ weight 1.0
        , width MATCH_PARENT
        , margin $ MarginTop 12
        ]
        [ scrollView
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , scrollBarY false
            ]
            [ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                ]
                ( map
                    ( \item -> rideComponent item)
                    listDatas
                )
            ]
        ]
    ]

listDatas :: Array RideComponent
listDatas =
  [ { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  , { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  , { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  , { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  , { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  , { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
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
