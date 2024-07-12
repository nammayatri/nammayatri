module Screens.EarningsScreen.History.View where

import Prelude
import Screens.EarningsScreen.ScreenData
import Common.Types.App
import Data.Maybe
import Effect
import Prelude
import PrestoDOM hiding (tabLayout)
import Screens.EarningsScreen.History.Controller
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
import Prelude
import PrestoDOM
import Effect
import Data.Maybe
import PrestoDOM.List as List
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple
import Effect.Aff (launchAff)
import Types.App

screen :: State -> HistoryScreen -> Screen Action State ScreenOutput
screen initialState screenType =
  { initialState
  , view: getView screenType
  , name: "HistoryScreen"
  , globalEvents:
      [ ( \push -> do
            case screenType of
              Ride ->  
                case initialState.data.rideListItem of
                  Nothing -> do
                    void $ launchAff $ flowRunner defaultGlobalState $ do
                      listItem <- List.preComputeListItem $ historyHolder push initialState
                      liftFlow $ push $ SetRideListItem listItem
                  _ -> pure unit
              Payout -> 
                case initialState.data.payoutListItem of
                  Nothing -> do 
                    void $ launchAff $ flowRunner defaultGlobalState $ do
                      listItem <- List.preComputeListItem $ historyHolder push initialState
                      liftFlow $ push $ SetPayoutListItem listItem
                  _ -> pure unit
            pure $ pure unit
        )
      ]
  , eval
  }

getView :: forall w. HistoryScreen -> ((Action -> Effect Unit) -> State -> Layout w)
getView = case _ of
  Ride -> wrapWithKeyedLayout rideView "Ride" 
  Payout -> wrapWithKeyedLayout payoutView "Payout" 

wrapWithKeyedLayout :: forall w. ((Action -> Effect Unit) -> State -> Layout w) -> String -> (Action -> Effect Unit) -> State -> Layout w
wrapWithKeyedLayout dom key push state = 
  Keyed.linearLayout
  [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push $ const BackPressed
  ][ Tuple key $ dom push state
  ]

rideView :: forall w. (Action -> Effect Unit) -> State -> Layout w
rideView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ]
    $ [ headerLayout push state (getString RIDE_HISTORY)
      , navigateBtwWeeksView push state
      ]
    <> case state.data.rideListItem of
        Just item ->
          [ List.list
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , List.listItem item
              , List.listDataV2 dummyProps
              , scrollBarY false
              ]
          ]
        Nothing -> []

payoutView :: forall w. (Action -> Effect Unit) -> State -> Layout w
payoutView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ]
    $ [ headerLayout push state "Payout History"
      , navigateBtwWeeksView push state
      ]
    <> case state.data.payoutListItem of
        Just item ->
          [ List.list
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , List.listItem item
              , List.listDataV2 dummyPayoutProps
              , scrollBarY false
              ]
          ]
        Nothing -> []

headerLayout :: forall w. (Action -> Effect Unit) -> State -> String -> Layout w
headerLayout push state headerText =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , padding $ Padding 10 (safeMarginTopWithDefault 13) 10 13
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , onClick push $ const BackPressed
            , rippleColor Color.rippleShade
            , cornerRadius 6.0
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text headerText
              , margin $ MarginLeft 10
              , padding $ PaddingBottom 2
              , color Color.black900
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

navigateBtwWeeksView :: forall w. (Action -> Effect Unit) -> State -> Layout w
navigateBtwWeeksView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ Margin 20 16 20 16
    ]
    [ imageView 
        [ height $ V 24
        , width $ V 24
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
        ]
    , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ "May 1 - May 7"
        , gravity CENTER
        , weight 1.0
        ] <> FontStyle.subHeading1 TypoGraphy
    , imageView
        [ height $ V 24
        , width $ V 24
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
        , rotation 180.0
        ]
    ]

historyHolder :: forall w. (Action -> Effect Unit) -> State -> Layout w
historyHolder push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ Padding 12 12 12 12
        , margin $ Margin 16 8 16 8
        , stroke $ "1," <> Color.grey900
        , gravity CENTER
        , cornerRadius 8.0
        ]
        [ imageView
            [ height $ V 24
            , width $ V 25
            , List.imageUrlHolder "vehicleImage"
            , List.visibilityHolder "vehicleImageVisibility"
            ]
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , margin $ Margin 8 4 0 0
            ]
            [ textView
                $ [ List.textHolder "serviceTier"
                  , color Color.black900
                  ]
                <> FontStyle.paragraphText TypoGraphy
            , linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , gravity CENTER_VERTICAL
                ]
                [ textView
                    $ [ List.textHolder "rideDate"
                      , color Color.black700
                      ]
                    <> FontStyle.body3 TypoGraphy
                , linearLayout
                    [ height $ V 3
                    , width $ V 3
                    , cornerRadius 1.5
                    , background Color.black700
                    , margin $ MarginHorizontal 6 6
                    , List.visibilityHolder "vehicleImageVisibility"
                    ]
                    []
                , textView
                    $ [ List.textHolder "rideTime"
                      , List.visibilityHolder "vehicleImageVisibility"
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
                  $ [ List.textHolder "rideFare"
                    , List.colorHolder "rideFareColor"
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
                        , List.visibilityHolder $ "tagVisibility" <> tagItem.type
                        ]
                        [ textView
                            $ [ List.textHolder $ "tagText" <> tagItem.type
                              , color tagItem.color
                              ]
                            <> FontStyle.body17 TypoGraphy
                        ]
                  )
              )
                tagConfigList
        ]
    ]

tagConfigList =
  [ { background: "#F6F1FF"
    , color: Color.purple700
    , "type": "Penality"
    }
  , { background: "#FCEDED"
    , color: Color.red900
    , "type": "Cancellation"
    }
  , { background: "#1A3AB27C"
    , color: Color.black800
    , "type": "Tips"
    }
  ]
