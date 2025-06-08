{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverEarningsScreenV2.View where

import Common.Types.App
import Data.Maybe
import Debug
import Helpers.Utils
import JBridge
import Locale.Utils
import Mobility.Prelude
import Prelude
import Screens.DriverEarningsScreenV2.ComponentConfig

import Animation (fadeIn, translateInYAnim)
import Animation as Anim
import Animation.Config (Direction(..), animConfig)
import Common.Types.App (YoutubeData(..))
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Components.Calendar.View as Calendar
import Components.ErrorModal as ErrorModal
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.GenericHeader.View as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.View as PrimaryButton
import Components.RequestInfoCard as RequestInfoCard
import Control.Monad.Except (runExceptT, runExcept)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (length, (..), foldl, filter, (!!), null, last, mapWithIndex, any, head)
import Data.Either (Either(..), either)
import RemoteConfig.Utils
import Data.Function.Uncurried (runFn1, runFn2, runFn5)
import Data.Int (ceil, floor, fromNumber, toNumber, fromString)
import Data.String as DS
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn7)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (getCurrentUTC, flowRunner, getFormattedDate, getNewIDWithTag, screenHeight, getVideoID, getDayName, safeMarginBottom, screenWidth, convertUTCtoISC, liftFlow, formatCurrencyWithCommas, getPastDateFromDate, getPastMonthsFromDate, getFutureDate)
import Engineering.Helpers.Utils (loaderText, toggleLoader, getFixedTwoDecimals, getColorWithOpacity)
import Font.Size as FontSize
import Font.Style as FontStyle
import Foreign.Generic (decodeJSON)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Presto.Core.Types.Language.Flow (doAff, Flow)
import PrestoDOM (textFromHtml, scrollView, frameLayout, shimmerFrameLayout, layoutGravity, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, lottieAnimationView, alignParentBottom, background, calendar, clickable, color, cornerRadius, fontSize, fontStyle, gravity, height, horizontalScrollView, id, imageView, imageWithFallback, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, relativeLayout, scrollBarX, scrollBarY, stroke, swipeRefreshLayout, text, textSize, textView, visibility, weight, width, onAnimationEnd, alpha, singleLine, rippleColor)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens as ScreenNames
import Screens.DriverEarningsScreenV2.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.API (GetRidesHistoryResp(..), GetRidesSummaryListResp(..), DriverProfileSummaryRes(..))
import Services.API as API
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Timers (startTimer)
import Locale.Utils
import Effect.Uncurried (runEffectFn3)
import Types.App (defaultGlobalState, GlobalState(..))

screen :: ST.DriverEarningsScreenState -> Screen Action ST.DriverEarningsScreenState ScreenOutput
screen initialState =
  { initialState: initialState
  , view: view
  , name: "DriverEarningsScreenV2"
  , globalEvents:
      [ globalOnScroll "DriverEarningsScreenV2"
      , ( \push -> do
            _ <-
              launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT
                $ do
                    when (not initialState.data.anyRidesAssignedEver)
                      $ do
                          profileSummaryResponse <- lift $ lift $ Remote.driverProfileSummary ""
                          let
                            anyRidesAssignedEver = either (\_ -> false) (\(DriverProfileSummaryRes profileSummaryResponse) -> profileSummaryResponse.totalRidesAssigned > 0) profileSummaryResponse
                          liftFlowBT $ push $ UpdateRidesEver anyRidesAssignedEver
                    case initialState.props.subView of
                      ST.ALL_TIME_EARNINGS_VIEW -> pure unit
                      _ -> do
                        let dates = if DS.null initialState.props.toDate then getDates initialState.props.subView initialState else {fromDate: initialState.props.fromDate, toDate: initialState.props.toDate}
                            period = getPeriodFromSubView initialState.props.subView
                        (API.EarningPeriodStatsRes earningPeriodStatsRes) <- Remote.getEarningPeriodStatsBT dates.fromDate dates.toDate (show period)
                        liftFlowBT $ push $ EarningPeriodStatsAPIResponseAction earningPeriodStatsRes.earnings dates.fromDate dates.toDate
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "DriverEarningsScreenV2State action" action
          let
            _ = spy "DriverEarningsScreenV2State state" state
          eval action state
      )
  }

getPeriodFromSubView :: ST.DriverEarningsSubView -> API.EarningType
getPeriodFromSubView subView =
  case subView of
    ST.EARNINGS_VIEW -> API.DAILY
    ST.WEEKLY_EARNINGS_VIEW -> API.WEEKLY
    ST.MONTHLY_EARNINGS_VIEW -> API.MONTHLY
    _ -> API.DAILY

getDates :: ST.DriverEarningsSubView -> ST.DriverEarningsScreenState -> {fromDate :: String, toDate :: String}
getDates subView state =
  let todaysDate = getcurrentdate ""
  in
  case subView of
    ST.EARNINGS_VIEW -> {fromDate: (getPastDateFromDate todaysDate 6), toDate: todaysDate}
    ST.WEEKLY_EARNINGS_VIEW -> do
      let dayOfWeek = getDayOfWeek (getDayName todaysDate)
      {fromDate: (getPastDateFromDate todaysDate (42 + dayOfWeek)), toDate: todaysDate}
    ST.MONTHLY_EARNINGS_VIEW -> do
      let pastMonths = getPastMonthsFromDate todaysDate 6
          fromDate = maybe "2025-01-01" (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") (head pastMonths)
      {fromDate: fromDate, toDate: todaysDate}
    _ -> {fromDate: todaysDate, toDate: todaysDate}

view :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    , background Color.white900
    ]
    [ frameLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background if state.props.subView == ST.FAQ_QUESTON_VIEW then Color.white900 else Color.blue600
        ]
        [ Anim.screenAnimationFadeInOut
            $ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ headerView push state
                , if not state.data.anyRidesAssignedEver then
                    noRideHistoryView push state
                  else
                    scrollView
                      [ height MATCH_PARENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      ]
                      [ if state.props.showShimmer then shimmerView push state else Keyed.relativeLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , orientation VERTICAL
                          ]
                          $ []
                          <> (if state.props.subView == ST.EARNINGS_VIEW then [ Tuple "Earnings" $ earningsView push state ] else [])
                          <> (if state.props.subView == ST.WEEKLY_EARNINGS_VIEW then [ Tuple "WeeklyEarnings" $ earningsView push state ] else [])
                          <> (if state.props.subView == ST.MONTHLY_EARNINGS_VIEW then [ Tuple "MonthlyEarnings" $ earningsView push state ] else [])
                          <> (if state.props.subView == ST.ALL_TIME_EARNINGS_VIEW then [ Tuple "AllTimeEarnings" $ earningsView push state ] else [])
                      ]
                ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , visibility $ boolToVisibility (any (_ == state.props.subView) [ST.EARNINGS_VIEW, ST.WEEKLY_EARNINGS_VIEW, ST.MONTHLY_EARNINGS_VIEW, ST.ALL_TIME_EARNINGS_VIEW])
        ]
        [ BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.DRIVER_EARNINGS_SCREEN state.data.config.bottomNavConfig) ]
    ]
  where
  cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)

headerView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
headerView push state =
  linearLayout 
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , background Color.white900
  , padding $ Padding 16 16 16 16
  , margin $ MarginBottom 24
  ]
  [ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 8 8 8 8
    , background Color.white900
    ]
    [ textView $ 
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString EARNINGS
      , color Color.black900
      ] <> FontStyle.h3 TypoGraphy
    ]
  , tabView push state
  ]

tabView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
tabView push state =
  let
    cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , cornerRadius 24.0
      , background Color.grey700
      , padding $ Padding 4 4 4 4
      , margin $ Margin 8 8 8 8
      , gravity CENTER
      ]
      [ tabItem push (state.props.subView == ST.EARNINGS_VIEW) (getString DAILY) ST.EARNINGS_VIEW
      , tabItem push (state.props.subView == ST.WEEKLY_EARNINGS_VIEW) (getString WEEKLY) ST.WEEKLY_EARNINGS_VIEW
      , tabItem push (state.props.subView == ST.MONTHLY_EARNINGS_VIEW) (getString MONTHLY) ST.MONTHLY_EARNINGS_VIEW
      , tabItem push (state.props.subView == ST.ALL_TIME_EARNINGS_VIEW) (getString ALL_TIME) ST.ALL_TIME_EARNINGS_VIEW
      ]

tabItem :: forall w. (Action -> Effect Unit) -> Boolean -> String -> ST.DriverEarningsSubView -> PrestoDOM (Effect Unit) w
tabItem push isActive text' subView =
    linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , padding $ PaddingVertical 6 8
      , weight 1.0
      , background if isActive then Color.blue900 else Color.grey700
      , gravity CENTER
      , cornerRadius 24.0
      , onClick push $ const $ ChangeTab subView
      ]
      [ textView
          $ [ height WRAP_CONTENT
            , text $ text'
            , color if isActive then Color.white900 else Color.black700
            , margin $ if (getLanguageLocale languageKey == "KN_IN") then MarginTop 4 else MarginTop 0
            ]
          <> FontStyle.tags TypoGraphy
      ]

earningsView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
earningsView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    , padding $ Padding 16 0 16 70
    ]
    [ totalEarningsView push state
    , earningsSummaryView push state
    ]


totalEarningsView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
totalEarningsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , padding $ Padding 20 20 20 16
    , cornerRadius 12.0
    , stroke $ "1," <> Color.grey900
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , gravity CENTER
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , weight 1.0
            , gravity LEFT
            , visibility $ boolToVisibility $ state.props.subView /= ST.ALL_TIME_EARNINGS_VIEW
            ]
            [ imageView
                $ [ width (V 32)
                  , height (V 32)
                  , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left_black"
                  , onClick push $ const $ LeftChevronClicked
                  ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER
            ]
            [ textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ getDateRangeText state.props.subView state.props.totalEarningsData
                  ]
                <> FontStyle.paragraphText TypoGraphy
            , textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ "₹" <> (formatCurrencyWithCommas (show state.props.totalEarningsData.totalEarnings))
                  , color Color.black900
                  ]
                <> FontStyle.priceFont TypoGraphy
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity RIGHT
            , weight 1.0
            , visibility $ boolToVisibility $ state.props.subView /= ST.ALL_TIME_EARNINGS_VIEW
            ]
            [ imageView
                $ [ width $ V 32
                  , height $ V 32
                  , imageWithFallback $ fetchImage FF_ASSET $ if state.props.graphIndex < 0 then "ny_ic_chevron_right_grey" else "ny_ic_chevron_right_black"
                  , gravity RIGHT
                  , onClick push $ const $ RightChevronClicked
                  , clickable $ state.props.graphIndex < 0
                  ]
            ]
        ]
    , barGraphView push state
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , margin $ if state.props.subView == ST.MONTHLY_EARNINGS_VIEW then MarginTop 0 else MarginTop 16
        , background Color.grey700
        ]
        []
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , padding $ PaddingVertical 15 15
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , gravity CENTER
            , width $ V (((screenWidth unit) - 75) / 2)
            ]
            [ textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ getString RIDES
                  , margin $ MarginRight 8
                  ]
                <> FontStyle.paragraphText TypoGraphy
            , textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ show $ state.props.totalEarningsData.totalRides
                  ]
                <> FontStyle.h2 TypoGraphy
            ]
        , linearLayout
            [ height $ V 24
            , width $ V 2
            , background Color.grey900
            ]
            []
        , linearLayout
            [ height WRAP_CONTENT
            , gravity CENTER
            , width $ V (((screenWidth unit) - 75) / 2)
            ]
            [ textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ getString DISTANCE
                  , margin $ MarginRight 8
                  ]
                <> FontStyle.paragraphText TypoGraphy
            , textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ show (state.props.totalEarningsData.totalDistanceTravelled / 1000) <> " km"
                  ]
                <> FontStyle.h2 TypoGraphy
            ]
        ]
    ]
    where getDateRangeText subView totalEarningsData =
            case subView of
              ST.EARNINGS_VIEW -> if DS.null totalEarningsData.toDate then convertUTCtoISC totalEarningsData.fromDate "DD MMM, YYYY" else convertUTCtoISC totalEarningsData.fromDate "DD MMM" <> " - " <> convertUTCtoISC totalEarningsData.toDate "DD MMM, YYYY"
              ST.WEEKLY_EARNINGS_VIEW ->
                if DS.null totalEarningsData.toDate
                  then
                    convertUTCtoISC totalEarningsData.fromDate "DD MMM" <> " - " <> convertUTCtoISC (getFutureDate totalEarningsData.fromDate 6) "DD MMM, YYYY"
                  else
                    convertUTCtoISC totalEarningsData.fromDate "DD MMM" <> " - " <> convertUTCtoISC (getFutureDate totalEarningsData.toDate 6) "DD MMM, YYYY"
              ST.MONTHLY_EARNINGS_VIEW -> convertUTCtoISC totalEarningsData.fromDate "MMM, YYYY" <> (if DS.null totalEarningsData.toDate then "" else " - " <> convertUTCtoISC totalEarningsData.toDate "MMM, YYYY")
              _ -> "yyyy-mm-dd"


horizontalLine :: forall w. PrestoDOM (Effect Unit) w
horizontalLine =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    , gravity CENTER
    ]
    []

helpButton :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
helpButton push =
  linearLayout
    [ gravity RIGHT
    , weight 1.0
    , width WRAP_CONTENT
    , onClick push $ const $ ChangeTab ST.FAQ_VIEW
    ]
    [ imageView
        $ [ width (V 22)
          , height (V 22)
          , imageWithFallback $ fetchImage FF_ASSET "ny_ic_help_circle_blue,"
          , padding $ Padding 0 4 4 0
          , gravity CENTER_VERTICAL
          ]
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString HELP_FAQ
          , color $ Color.blue900
          ]
        <> FontStyle.subHeading2 TypoGraphy
    ]

separatorView :: forall w. Boolean -> ST.DriverEarningsSubView -> PrestoDOM (Effect Unit) w
separatorView isVisible subView =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background $ if any (_ == subView) [ ST.FAQ_VIEW, ST.FAQ_QUESTON_VIEW ] then Color.grey900 else Color.grey700
    , visibility $ boolToVisibility isVisible
    ]
    []

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = linearLayout [ visibility GONE ] []

noItemsView :: forall w. ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
noItemsView state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , padding $ Padding 16 32 16 32
    , cornerRadius 12.0
    , background Color.white900
    , stroke $ "1," <> Color.grey900
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , gravity CENTER
          , text $ fromMaybe "" (head $ getNotItemsViewText state)
          , color Color.black900
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , textView
        $ [ width WRAP_CONTENT
          , gravity CENTER
          , text $ fromMaybe "" (last $ getNotItemsViewText state)
          , color Color.black700
          ]
        <> FontStyle.paragraphText TypoGraphy
    ]

earningsSummaryView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
earningsSummaryView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 24
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin $ Margin 8 0 8 8
        , padding $ Padding 16 12 16 12
        , background $ getColorWithOpacity 8 Color.blue900
        , cornerRadius 10.0
        ]
        [ textView
            $ [ text $ getString SUBSCRIPTION_AMOUNT_SPENT
              , weight 1.0
              , color Color.black700
              ]
            <> FontStyle.paragraphText TypoGraphy
        , textView
            $ [ text $ "₹" <> formatCurrencyWithCommas "2000"
              , color Color.black800
              ]
            <> FontStyle.h2 TypoGraphy
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin $ Margin 8 8 8 8
        , padding $ Padding 16 12 16 12
        , background $ getColorWithOpacity 8 Color.blue900
        , cornerRadius 10.0
        ]
        [ textView
            $ [ text $ getString RIDE_BOOST_RECEIVED
              , weight 1.0
              , color Color.black700
              ]
            <> FontStyle.paragraphText TypoGraphy
        , textView
            $ [ text $ "₹" <> formatCurrencyWithCommas (show state.props.totalEarningsData.tipAmount)
              , color Color.black800
              ]
            <> FontStyle.h2 TypoGraphy
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin $ Margin 8 8 8 8
        , padding $ Padding 16 12 16 12
        , background $ getColorWithOpacity 8 Color.blue900
        , cornerRadius 10.0
        ]
        [ textView
            $ [ text $ getString CANCELLATION_CHARGES_RECEIVED
              , weight 1.0
              , color Color.black700
              ]
            <> FontStyle.paragraphText TypoGraphy
        , textView
            $ [ text $ "₹" <> formatCurrencyWithCommas (show state.props.totalEarningsData.cancellationCharges)
              , color Color.black800
              ]
            <> FontStyle.h2 TypoGraphy
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin $ Margin 8 8 8 16
        , padding $ Padding 16 12 16 12
        , background $ getColorWithOpacity 8 Color.blue900
        , cornerRadius 10.0
        ]
        [ textView
            $ [ text $ getString EARNINGS_PER_KM
              , weight 1.0
              , color Color.black700
              ]
            <> FontStyle.paragraphText TypoGraphy
        , textView
            $ [ text $ "₹" <> (formatCurrencyWithCommas (show $ state.props.totalEarningsData.totalEarnings / state.props.totalEarningsData.totalDistanceTravelled)) <> "/km"
              , color Color.black800
              ]
            <> FontStyle.h2 TypoGraphy
        ]
    ]

dottedLineView :: forall w. (Action -> Effect Unit) -> Int -> Int -> PrestoDOM (Effect Unit) w
dottedLineView push margintop earnings =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginTop margintop
    , gravity CENTER
    ]
    [ imageView
        [ height $ V 2
        , width MATCH_PARENT
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_dotted_line"
        , weight 1.0
        ]
    , textView
        $ [ height $ WRAP_CONTENT
          , width $ WRAP_CONTENT
          , gravity RIGHT
          , text $ "₹" <> formatCurrencyWithCommas (show earnings)
          ]
        <> FontStyle.paragraphText TypoGraphy
    ]

barGraphView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
barGraphView push state =
  let
    currWeekMaxEarning = if state.props.currentWeekMaxEarning > 0 then state.props.currentWeekMaxEarning else 1500
  in
    relativeLayout
      [ height $ V 150
      , width MATCH_PARENT
      , visibility $ boolToVisibility $ any (_ == state.props.subView) [ST.EARNINGS_VIEW, ST.WEEKLY_EARNINGS_VIEW, ST.MONTHLY_EARNINGS_VIEW]
      ]
      [ dottedLineView push 7 currWeekMaxEarning
      , dottedLineView push 40 ((currWeekMaxEarning * 2) / 3)
      , dottedLineView push 73 (currWeekMaxEarning / 3)
      , linearLayout
          [ height $ V 2
          , width MATCH_PARENT
          , background Color.grey900
          , margin $ MarginTop 114
          ]
          []
      , linearLayout
          [ height $ V 150
          , width MATCH_PARENT
          , background Color.transparent
          , orientation HORIZONTAL
          , gravity BOTTOM
          ]
          (mapWithIndex (\index item -> (barView push index item state)) state.props.currWeekData)
      ]

barView :: forall w. (Action -> Effect Unit) -> Int -> ST.WeeklyEarning -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
barView push index item state =
  let
    selectedIndex = state.props.selectedBarIndex

    setMargin = case index of
      0 -> MarginHorizontal 3 6
      6 -> MarginHorizontal 3 (screenWidth unit / 8)
      _ -> MarginHorizontal 6 6
  in
    linearLayout
      [ height $ WRAP_CONTENT
      , width $ WRAP_CONTENT
      , margin setMargin
      , weight 1.0
      , orientation VERTICAL
      ]
      [ linearLayout
          [ height $ WRAP_CONTENT
          , width $ WRAP_CONTENT
          , onClick push $ const $ BarViewSelected index
          ]
          [ PrestoAnim.animationSet [ Anim.translateInYAnim $ animConfig { duration = 1000 + (ceil item.percentLength), fromY = 200 + (ceil item.percentLength) } ]
              $ linearLayout
                  [ height $ WRAP_CONTENT
                  , width $ WRAP_CONTENT
                  ]
                  [ linearLayout
                      [ height $ V $ if item.percentLength > 0.0 then (ceil item.percentLength) else 1
                      , width $ V (screenWidth unit / 16)
                      , background if selectedIndex < 0 || selectedIndex == index then Color.blue900 else getColorWithOpacity 16 Color.blue900
                      , cornerRadius 4.0
                      ]
                      []
                  ]
          ]
      , textView
          $ [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , text $ DS.drop 8 item.rideDate
            , visibility $ boolToVisibility $ any (_ == state.props.subView) [ST.EARNINGS_VIEW, ST.WEEKLY_EARNINGS_VIEW]
            , gravity CENTER
            ]
          <> FontStyle.paragraphText TypoGraphy
      , textView
          $ [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , text $ convertUTCtoISC item.rideDate "MMM"
            , gravity CENTER
            , singleLine true
            , margin $ if state.props.subView == ST.MONTHLY_EARNINGS_VIEW then MarginTop 2 else if (getLanguageLocale languageKey == "KN_IN") then MarginBottom 0 else MarginBottom 2
            ]
          <> FontStyle.body9 TypoGraphy
      , textView
          $ [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , text $ DS.drop 8 item.rideDate
            , visibility $ if state.props.subView == ST.MONTHLY_EARNINGS_VIEW then INVISIBLE else GONE
            , gravity CENTER
            ]
          <> FontStyle.paragraphText TypoGraphy
      ]

getDailyEarnings :: Array ST.CoinHistoryItem -> Int
getDailyEarnings list =
  foldl
    ( \acc record -> case record.earnings of
        Just x -> acc + x
        Nothing -> acc
    )
    0
    list

tagview :: String -> forall w. PrestoDOM (Effect Unit) w
tagview name =
  imageView
    [ width (V 16)
    , height (V 16)
    , imageWithFallback name
    , margin $ Margin 4 4 4 3
    ]

noRideHistoryView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
noRideHistoryView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity CENTER
    , padding (PaddingBottom safeMarginBottom)
    ]
    [ ErrorModal.view (push <<< ErrorModalActionController) (errorModalConfig state) ]

getNotItemsViewText :: ST.DriverEarningsScreenState -> Array String
getNotItemsViewText state = case state.props.subView of
  ST.EARNINGS_VIEW -> [ getString NO_RIDES, getString YOU_DID_NOT_TAKE_ANY_RIDES_ON_PREFIX <> " " <> convertUTCtoISC state.props.date "DD MMM, YYYY" <> " " <> getString YOU_DID_NOT_TAKE_ANY_RIDES_ON_SUFFIX ]
  _ -> [ getString NO_POINTS_USED, getString USE_THEM_BEFORE_THEY_EXPIRE ]

shimmerView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
shimmerView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.blue600
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , cornerRadius 12.0
            , orientation VERTICAL
            , background Color.white900
            , margin $ Margin 16 0 16 6
            ]
            [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ]
              [ totalEarningsShimmerView push state
              ]
            ]
        , shimmerFrameLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.blue600
          ][ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , margin $ MarginTop 12
              ]
              ( map
                  ( \_ ->
                      linearLayout
                        [ width MATCH_PARENT
                        , height (V 50)
                        , orientation VERTICAL
                        , margin $ Margin 8 10 8 10
                        , background Color.greyDark
                        , cornerRadius 8.0
                        ]
                        []
                  )
                  (1 .. 4)
              )
          ]
        ]
    ]


totalEarningsShimmerView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
totalEarningsShimmerView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , padding $ Padding 20 20 20 0
    , cornerRadius 12.0
    , stroke $ "1," <> Color.grey900
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , gravity CENTER
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , weight 1.0
            , gravity LEFT
            , visibility $ boolToVisibility $ state.props.subView /= ST.ALL_TIME_EARNINGS_VIEW
            ]
            [ shimmerFrameLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                ]
                [ linearLayout
                  [ width (V 32)
                  , height (V 32)
                  , background Color.greyDark
                  , cornerRadius 3.0
                  ][]
                ]
            ]
        , shimmerFrameLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER
            ]
            [ linearLayout
                  [ height $ V 15
                  , width $ V 100
                  , background Color.greyDark
                  , margin $ MarginBottom 8
                  , cornerRadius 3.0
                  ][]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity RIGHT
            , weight 1.0
            , visibility $ boolToVisibility $ state.props.subView /= ST.ALL_TIME_EARNINGS_VIEW
            ]
            [ shimmerFrameLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                ]
                [ linearLayout
                  [ width $ V 32
                  , height $ V 32
                  , background Color.greyDark
                  , cornerRadius 3.0
                  ][] 
                ]
            ]
        ]
    , barGraphShimmerView push state
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , margin $ if state.props.subView == ST.MONTHLY_EARNINGS_VIEW then MarginTop 0 else MarginTop 16
        , background Color.grey700
        ]
        []
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , padding $ PaddingVertical 15 15
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , gravity CENTER
            , width $ V (((screenWidth unit) - 75) / 2)
            , gravity CENTER_VERTICAL
            ]
            [ textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ getString RIDES
                  , margin $ MarginRight 8
                  ]
                <> FontStyle.paragraphText TypoGraphy
            , shimmerFrameLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                ]
                [ linearLayout
                  [ height $ V 10
                  , width $ V 15
                  , background Color.greyDark
                  , cornerRadius 3.0
                  ][]
                ]
            ]
        , linearLayout
            [ height $ V 24
            , width $ V 2
            , background Color.grey900
            ]
            []
        , linearLayout
            [ height WRAP_CONTENT
            , gravity CENTER
            , width $ V (((screenWidth unit) - 75) / 2)
            , gravity CENTER_VERTICAL
            ]
            [ textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ getString DISTANCE
                  , margin $ MarginRight 8
                  ]
                <> FontStyle.paragraphText TypoGraphy
            , shimmerFrameLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                ]
                [ linearLayout
                  [ height $ V 10
                  , width $ V 15
                  , background Color.greyDark
                  , cornerRadius 3.0
                  ][]
                ]
            ]
        ]
    ]


barGraphShimmerView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
barGraphShimmerView push state =
    relativeLayout
      [ height $ V 150
      , width MATCH_PARENT
      , visibility $ boolToVisibility $ any (_ == state.props.subView) [ST.EARNINGS_VIEW, ST.WEEKLY_EARNINGS_VIEW, ST.MONTHLY_EARNINGS_VIEW]
      ]
      [ dottedLineShimmerView push 7 
      , dottedLineShimmerView push 40
      , dottedLineShimmerView push 73
      , linearLayout
          [ height $ V 2
          , width MATCH_PARENT
          , background Color.grey900
          , margin $ MarginTop 114
          ]
          []
      , linearLayout
          [ height $ V 150
          , width MATCH_PARENT
          , background Color.transparent
          , orientation HORIZONTAL
          , gravity BOTTOM
          ]
          (mapWithIndex (\index item -> (barShimmerView push index item state)) [12, 50, 100, 80, 20, 70, 5])
      ]

dottedLineShimmerView :: forall w. (Action -> Effect Unit) -> Int -> PrestoDOM (Effect Unit) w
dottedLineShimmerView push margintop =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginTop margintop
    , gravity CENTER
    ]
    [ imageView
        [ height $ V 2
        , width MATCH_PARENT
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_dotted_line"
        , weight 1.0
        ]
    , shimmerFrameLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        ]
        [ linearLayout
          [ height $ V 12
          , width $ V 20
          , background Color.greyDark
          , gravity RIGHT
          ][]
        ]
    ]

barShimmerView :: forall w. (Action -> Effect Unit) -> Int -> Int -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
barShimmerView push index length state =
  let
    setMargin = case index of
      0 -> MarginHorizontal 3 6
      6 -> MarginHorizontal 3 (screenWidth unit / 8)
      _ -> MarginHorizontal 6 6
  in
    linearLayout
      [ height $ WRAP_CONTENT
      , width $ WRAP_CONTENT
      , margin setMargin
      , weight 1.0
      , orientation VERTICAL
      ]
      [ linearLayout
          [ height $ WRAP_CONTENT
          , width $ WRAP_CONTENT
          ]
          [ shimmerFrameLayout
            [ height $ WRAP_CONTENT
            , width $ WRAP_CONTENT
            ]
            [ linearLayout
                [ height $ V $ length
                , width $ V (screenWidth unit / 16)
                , background Color.greyDark
                , cornerRadius 4.0
                ][]
            ]
          ]
      , textView
          $ [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , text $ "12"
            , visibility INVISIBLE
            , gravity CENTER
            ]
          <> FontStyle.paragraphText TypoGraphy
      , textView
          $ [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , text $ "12"
            , visibility  INVISIBLE
            , gravity CENTER
            ]
          <> FontStyle.paragraphText TypoGraphy
      ]