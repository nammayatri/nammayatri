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
import Engineering.Helpers.Commons (getCurrentUTC, flowRunner, getFormattedDate, getNewIDWithTag, screenHeight, getVideoID, getDayName, safeMarginBottom, screenWidth, convertUTCtoISC, liftFlow, formatCurrencyWithCommas)
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
import Screens.DriverEarningsScreenV2.Controller (Action(..), ScreenOutput, eval, fetchWeekyEarningData, dummyQuestions)
import Screens.DriverEarningsScreen.ScreenData (dummyDateItem)
import Screens.Types (DriverEarningsScreenState)
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
                      ST.EARNINGS_VIEW -> do
                        if initialState.props.callRideSummaryApi then do
                          let
                            currentDate = getcurrentdate ""

                            datesList = getDatesList currentDate initialState
                          (GetRidesSummaryListResp rideSummaryResp) <- Remote.getRideSummaryListReqBT datesList
                          liftFlowBT $ push $ RideSummaryAPIResponseAction rideSummaryResp.list currentDate datesList
                        else
                          pure unit
                        (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "100" "0" "false" "null" (convertUTCtoISC initialState.props.date "YYYY-MM-DD")
                        liftFlowBT $ push $ RideHistoryAPIResponseAction rideHistoryResponse.list
                      _ -> do
                        -- liftFlowBT $ push $ FaqViewAction
                        pure unit
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

getDatesList :: String -> ST.DriverEarningsScreenState -> Array String
getDatesList todaysDate state =
  let
    storedRideSummaryData = fromMaybe [] (fetchWeekyEarningData RIDE_SUMMARY_DATA)
  in
    map (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") $ getListofPastDays storedRideSummaryData
  where
  getListofPastDays storedRideSummaryData =
    if not null storedRideSummaryData then
      maybe pastDays (\x -> getPastDays $ (runFn2 differenceBetweenTwoUTC todaysDate x.rideDate) / (24 * 60 * 60)) (last storedRideSummaryData)
    else
      pastDays

  pastDays = getPastDays (22 + getDayOfWeek (getDayName todaysDate))

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
                -- , visibility $ boolToVisibility (not state.props.showShimmer) -- TODO: Handle shimmer part
                ]
                [ headerView push state
                  -- GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
                  -- case state.props.subView of
                  --   _ | any (_ == state.props.subView) [ ST.FAQ_VIEW, ST.FAQ_QUESTON_VIEW ] || (not (state.data.config.feature.enableYatriCoins && cityConfig.enableYatriCoins)) -> GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
                  --   _ -> linearLayout [] []
                -- , if any (_ == state.props.subView) [ ST.FAQ_VIEW, ST.FAQ_QUESTON_VIEW ] then separatorView true state.props.subView else linearLayout [] []
                , if not state.data.anyRidesAssignedEver then
                    noRideHistoryView push state
                  else
                    scrollView
                      [ height MATCH_PARENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      ]
                      [ Keyed.relativeLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , orientation VERTICAL
                          ]
                          $ []
                          <> (if state.props.subView == ST.EARNINGS_VIEW then [ Tuple "Earnings" $ earningsView push state ] else [])
                          <> (if state.props.subView == ST.WEEKLY_EARNINGS_VIEW then [ Tuple "WeeklyEarnings" $ earningsView push state ] else [])
                          <> (if state.props.subView == ST.MONTHLY_EARNINGS_VIEW then [ Tuple "MonthlyEarnings" $ earningsView push state ] else [])
                          <> (if state.props.subView == ST.ALL_TIME_EARNINGS_VIEW then [ Tuple "AllTimeEarnings" $ earningsView push state ] else [])
                          <> (if state.props.subView == ST.FAQ_VIEW then [ Tuple "FAQView" $ faqView push state ] else [])
                          <> (if state.props.subView == ST.FAQ_QUESTON_VIEW then [ Tuple "faqQuestionView" $ faqQuestionView push state ] else [])
                      ]
                ]
        -- , if state.props.showShimmer then shimmerView push state else dummyView -- TODO: Handle shimmer part
        ]
    , if state.props.calendarState.calendarPopup then Calendar.view (push <<< CalendarAC) (calendarConfig state) else dummyView
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
      [ tabItem push (state.props.subView == ST.EARNINGS_VIEW) "Daily" ST.EARNINGS_VIEW
      , tabItem push (state.props.subView == ST.WEEKLY_EARNINGS_VIEW) "Weekly" ST.WEEKLY_EARNINGS_VIEW
      , tabItem push (state.props.subView == ST.MONTHLY_EARNINGS_VIEW) "Monthly" ST.MONTHLY_EARNINGS_VIEW
      , tabItem push (state.props.subView == ST.ALL_TIME_EARNINGS_VIEW) "All-time" ST.ALL_TIME_EARNINGS_VIEW
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

shimmerView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
shimmerView push state =
  shimmerFrameLayout
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
            [ height (V 350)
            , width MATCH_PARENT
            , background Color.greyDark
            , cornerRadius 12.0
            , orientation VERTICAL
            ]
            []
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , margin $ MarginTop 24
            ]
            ( map
                ( \_ ->
                    linearLayout
                      [ width MATCH_PARENT
                      , height (V 60)
                      , orientation VERTICAL
                      , margin $ MarginVertical 10 10
                      , background Color.greyDark
                      , cornerRadius 12.0
                      ]
                      []
                )
                (1 .. 3)
            )
        ]
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
            ]
            [ imageView
                $ [ width (V 32)
                  , height (V 32)
                  , imageWithFallback $ fetchImage FF_ASSET $ if state.props.weekIndex == 0 then "ny_ic_chevron_left_grey" else "ny_ic_chevron_left_black"
                  , onClick push $ const $ LeftChevronClicked state.props.weekIndex
                  , clickable $ state.props.weekIndex > 0
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
                  , text $ convertUTCtoISC state.props.totalEarningsData.fromDate "DD MMM"
                      <> ( if DS.null state.props.totalEarningsData.toDate then
                            ""
                          else
                            " - " <> convertUTCtoISC state.props.totalEarningsData.toDate "DD MMM"
                        )
                      <> ", "
                      <> getString EARNINGS
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
            ]
            [ imageView
                $ [ width $ V 32
                  , height $ V 32
                  , imageWithFallback $ fetchImage FF_ASSET $ if state.props.weekIndex == 3 then "ny_ic_chevron_right_grey" else "ny_ic_chevron_right_black"
                  , gravity RIGHT
                  , onClick push $ const $ RightChevronClicked state.props.weekIndex
                  , clickable $ state.props.weekIndex < 3
                  ]
            ]
        ]
    , barGraphView push state
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , margin $ MarginTop 16
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

faqView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
faqView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.blue600
    ]
    [ textView
        $ [ text $ getString LEARN_ABOUT_YATRI_POINTS
          , color Color.black700
          , margin $ Margin 16 12 0 12
          ]
        <> FontStyle.subHeading2 TypoGraphy
    , questionsListView push (dummyQuestions Language) state
    ]

questionsListView :: forall w. (Action -> Effect Unit) -> Array ST.FaqQuestions -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
questionsListView push questionsList state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background $ Color.white900
    ]
    (map (\item -> individualQuestionListView push item state) questionsList)

individualQuestionListView :: forall w. (Action -> Effect Unit) -> ST.FaqQuestions -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
individualQuestionListView push faqQuestion state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (PaddingHorizontal 16 16)
    , onClick push $ const $ OpenFaqQuestion faqQuestion
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , weight 1.0
            , gravity CENTER_VERTICAL
            ]
            [ textView
                ( [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , text faqQuestion.question
                  , padding (PaddingVertical 20 20)
                  , color Color.black900
                  , margin $ MarginRight 16
                  ]
                    <> FontStyle.subHeading2 TypoGraphy
                )
            ]
        , linearLayout
            [ height MATCH_PARENT
            , width $ V 20
            , gravity CENTER
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_grey,"
                , height $ V 18
                , width $ V 18
                , margin $ MarginHorizontal 10 8
                ]
            ]
        ]
    , horizontalLine
    ]

faqQuestionView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
faqQuestionView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , padding (Padding 16 16 16 16)
    , visibility $ boolToVisibility (state.props.subView == ST.FAQ_QUESTON_VIEW)
    ]
    [ textView
        ( [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text state.props.individualQuestion.question
          , color Color.black900
          , margin $ MarginBottom 20
          ]
            <> FontStyle.h1 TypoGraphy
        )
    , answersListView push state.props.individualQuestion.answer state
    , tableView state
    ]

tableView :: forall w. ST.DriverEarningsScreenState ->  PrestoDOM (Effect Unit) w
tableView state  =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 5.0
    , stroke $ "1," <> Color.grey900
    , orientation VERTICAL
    , visibility $ boolToVisibility $ state.props.individualQuestion.tag == ST.HowEarnLosePoints
    ] $ mapWithIndex (\index item -> tableItemView item index state) (fromMaybe [] state.data.coinInfoRes)

tableItemView :: forall w. API.CoinInfo -> Int -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
tableItemView (API.CoinInfo item) index state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][ linearLayout
            [
              height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ textView
              ( [ height WRAP_CONTENT
                , width $ V (((screenWidth unit - 10) * 55) / 100)
                , text item.title
                , color Color.black900
                , padding $ if index == 0 then Padding 16 12 16 12 else Padding 16 8 16 8
                ]
                  <> if index == 0 then FontStyle.h2 TypoGraphy else FontStyle.subHeading2 TypoGraphy
              )
            , textView 
                ( [
                    height WRAP_CONTENT
                  , width $ V (((screenWidth unit - 10) * 55) / 100)
                  , text item.description
                  , color Color.black700
                  , padding $ Padding 16 0 16 8
                  , visibility $ boolToVisibility $ not $ DS.null item.description
                  ] <> FontStyle.body1 TypoGraphy
                )
            ]
          , linearLayout
              [ height MATCH_PARENT
              , width $ V 1
              , background Color.grey900
              ]
              []
          , linearLayout
              [ height MATCH_PARENT
              , width $ V (((screenWidth unit - 10) * 45) / 100)
              , padding $ if index == 0 then Padding 16 12 16 12 else Padding 16 8 16 8
              , gravity CENTER_VERTICAL
              ]
              [ textView
                  ( [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , text $ if index /= 0 then  (if item.coins > 0 then "+" else "-") <> (show item.coins) else getString YATRI_POINTS_STR
                    , color Color.black900
                    ]
                      <> if index == 0 then FontStyle.h2 TypoGraphy else FontStyle.subHeading2 TypoGraphy
                  )
              , imageView
                  $ [ width (V 20)
                    , height (V 20)
                    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_yatri_coin"
                    , padding $ Padding 4 4 0 0
                    , visibility $ boolToVisibility (index == 0)
                    ]
              ]
          ]
    , separatorView (not (index == (length $ fromMaybe [] state.data.coinInfoRes) - 1)) state.props.subView
    ]

faqVideoView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
faqVideoView push state =
  if isJust state.props.individualQuestion.videoLink then
    linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , background Color.red600
      , gravity CENTER
      , margin $ MarginBottom 4
      , id $ getNewIDWithTag "faqVideo"
      , afterRender
          ( \action -> do
              let
                id = getNewIDWithTag "faqVideo"
              pure $ runFn5 setYoutubePlayer (youtubeData state "VIDEO") id (show ST.PLAY) push YoutubeVideoStatus
          )
          (const NoAction)
      ]
      []
  else
    linearLayout [] []

youtubeData :: ST.DriverEarningsScreenState -> String -> YoutubeData
youtubeData state mediaType =
  { videoTitle: "title"
  , setVideoTitle: false
  , showMenuButton: false
  , showDuration: true
  , showSeekBar: true
  , videoId: maybe "" (\x -> getVideoID x) state.props.individualQuestion.videoLink
  , videoType: "VIDEO"
  , videoHeight: 200
  , showFullScreen: false
  , hideFullScreenButton : false
  }

answersListView :: forall w. (Action -> Effect Unit) -> Array ST.AnswerConfig -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
answersListView push answersList state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    $ [ faqVideoView push state ]
    <> (map (\item -> if length answersList > 1 then singleAnswerView push item state else multipleAnswerView push item state) answersList)

singleAnswerView :: forall w. (Action -> Effect Unit) -> ST.AnswerConfig -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
singleAnswerView push answer state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding $ PaddingVertical 10 10
    ]
    [ textView
        ( [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text "•"
          , margin $ MarginRight 8
          , color Color.black900
          ]
            <> FontStyle.subHeading2 TypoGraphy
        )
    , ansWithHyperLinkView push answer state
    ]

multipleAnswerView :: forall w. (Action -> Effect Unit) -> ST.AnswerConfig -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
multipleAnswerView push answer state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding $ PaddingVertical 12 12
    ]
    [ ansWithHyperLinkView push answer state]


ansWithHyperLinkView :: forall w. (Action -> Effect Unit) -> ST.AnswerConfig -> ST.DriverEarningsScreenState ->  PrestoDOM (Effect Unit) w
ansWithHyperLinkView push answer state =
  let hyperLinkUrl = fromMaybe "" answer.hyperLinkUrl
      hyperLinkText = fromMaybe "" answer.hyperLinkText
  in
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ textView
        ( [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text answer.answer
          , color Color.black900
          ]
            <> FontStyle.subHeading2 TypoGraphy
        ),
        textView
         ( [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text hyperLinkText
          , color Color.blue900
          , gravity CENTER
          , margin $ Margin 0 0 0 40
          , visibility $ boolToVisibility $ not ( DS.null hyperLinkUrl || DS.null hyperLinkText)
          , onClick (\_ -> openUrlInApp $ hyperLinkUrl) (const unit)
          ] <> FontStyle.subHeading2 TypoGraphy
        )
    ]

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

-- calendarView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
-- calendarView push state =
--   let
--     selectedDate = state.props.date

--     dateToShow =
--       if DS.null selectedDate || convertUTCtoISC selectedDate "YYYY-MM-DD" == getcurrentdate "" then
--         getString TODAY
--       else
--         convertUTCtoISC selectedDate "Do MMM"
--   in
--     linearLayout
--       [ width WRAP_CONTENT
--       , height WRAP_CONTENT
--       , cornerRadius 100.0
--       , background Color.white900
--       , gravity CENTER_VERTICAL
--       , stroke $ "1," <> Color.grey900
--       , padding $ Padding 12 4 12 4
--       , onClick push $ const $ ShowCalendarPopup
--       ]
--       [ imageView
--           [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_calendar_unfilled_blue,"
--           , height $ V 16
--           , width $ V 16
--           , margin $ MarginRight 4
--           ]
--       , textView
--           $ [ text dateToShow
--             , color Color.black700
--             ]
--           <> FontStyle.tags TypoGraphy
--       , imageView
--           [ imageWithFallback
--               $ fetchImage FF_ASSET
--                   if state.props.calendarState.calendarPopup then
--                     "ny_ic_chevron_up"
--                   else
--                     "ny_ic_chevron_down"
--           , height $ V 12
--           , width $ V 12
--           , margin $ MarginLeft 6
--           ]
--       ]

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
            $ [ text $ "Subscription amount spent"
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
            $ [ text $ "Ride boost received"
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
            $ [ text $ "Cancellation charges received"
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
        , margin $ Margin 8 8 8 16
        , padding $ Padding 16 12 16 12
        , background $ getColorWithOpacity 8 Color.blue900
        , cornerRadius 10.0
        ]
        [ textView
            $ [ text $ "Earnings per km"
              , weight 1.0
              , color Color.black700
              ]
            <> FontStyle.paragraphText TypoGraphy
        , textView
            $ [ text $ "₹" <> (formatCurrencyWithCommas "2000") <> "/km"
              , color Color.black800
              ]
            <> FontStyle.h2 TypoGraphy
        ]
    ]

-- transactionViewForEarnings :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
-- transactionViewForEarnings push state =
--   linearLayout
--     [ width MATCH_PARENT
--     , height WRAP_CONTENT
--     , orientation VERTICAL
--     , margin $ MarginTop 24
--     ]
--     [ linearLayout
--         [ width MATCH_PARENT
--         , height WRAP_CONTENT
--         , orientation HORIZONTAL
--         , margin $ Margin 8 0 8 8
--         ]
--         [ textView
--             $ [ text $ getString RIDE_HISTORY
--               , weight 1.0
--               , color Color.black800
--               ]
--             <> FontStyle.h2 TypoGraphy
--         , calendarView push state
--         ]
--     , historyViewForEarnings push state
--     ]

-- historyViewForEarnings :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
-- historyViewForEarnings push state =
--   let
--     historyItems = state.data.earningHistoryItems
--   in
--     if null historyItems then
--       noItemsView state
--     else
--       linearLayout
--         [ width MATCH_PARENT
--         , height WRAP_CONTENT
--         , orientation VERTICAL
--         , background Color.white900
--         , cornerRadius 12.0
--         ]
--         [ linearLayout
--             [ width MATCH_PARENT
--             , height WRAP_CONTENT
--             , orientation HORIZONTAL
--             , padding $ Padding 16 12 16 12
--             , background Color.atlantisGreen
--             , cornerRadii $ Corners 12.0 true true false false
--             ]
--             [ textView
--                 $ [ text $ getString RIDES
--                   , weight 0.0
--                   , color Color.black700
--                   , margin $ MarginRight 8
--                   ]
--                 <> FontStyle.paragraphText TypoGraphy
--             , textView
--                 $ [ text $ show (length (filter (\item -> item.status == Just "COMPLETED") state.data.earningHistoryItems))
--                   , color Color.black800
--                   , weight 1.0
--                   ]
--                 <> FontStyle.h2 TypoGraphy
--             , textView
--                 $ [ text $ getString EARNINGS
--                   , color Color.black700
--                   , margin $ MarginRight 8
--                   ]
--                 <> FontStyle.paragraphText TypoGraphy
--             , textView
--                 $ [ text $ "₹" <> formatCurrencyWithCommas (show $ getDailyEarnings state.data.earningHistoryItems)
--                   , color Color.black800
--                   ]
--                 <> FontStyle.h2 TypoGraphy
--             ]
--         , linearLayout
--             [ width MATCH_PARENT
--             , height WRAP_CONTENT
--             , orientation VERTICAL
--             ]
--             (mapWithIndex (\index item -> historyViewItemForEarnings push item state index) state.data.earningHistoryItems)
--         ]

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
            , gravity CENTER
            ]
          <> FontStyle.paragraphText TypoGraphy
      , textView
          $ [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , text $ (fromMaybe "" (state.props.weekDay !! index))
            , gravity CENTER
            , singleLine true
            , margin $ if (getLanguageLocale languageKey == "KN_IN") then MarginBottom 0 else MarginBottom 2
            ]
          <> FontStyle.body9 TypoGraphy
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

-- historyViewItemForEarnings :: forall w. (Action -> Effect Unit) -> ST.CoinHistoryItem -> ST.DriverEarningsScreenState -> Int -> PrestoDOM (Effect Unit) w
-- historyViewItemForEarnings push item state index =
--   let
--     rideStatus = fromMaybe "" item.status

--     color' = if rideStatus /= "CANCELLED" then Color.green900 else Color.red

--     earnings = fromMaybe 0 item.earnings
--   in
--     linearLayout
--       [ height WRAP_CONTENT
--       , width MATCH_PARENT
--       , orientation VERTICAL
--       ]
--       [ linearLayout
--           [ height WRAP_CONTENT
--           , width MATCH_PARENT
--           , gravity CENTER_VERTICAL
--           , padding $ Padding 16 12 16 10
--           , onClick push $ const $ OpenTripDetails index
--           ]
--           [ linearLayout
--               [ height WRAP_CONTENT
--               , width WRAP_CONTENT
--               , weight 1.0
--               , orientation VERTICAL
--               ]
--               [ textView
--                   $ [ text $ getString DESTINATION <> ":" <> DS.take 30 (fromMaybe "" item.destination) <> "..."
--                     , color Color.black900
--                     ]
--                   <> FontStyle.body3 TypoGraphy
--               , textView
--                   $ [ text $ convertUTCtoISC item.timestamp "DD/MM/YYYY" <> "  " <> "•" <> "  " <> convertUTCtoISC item.timestamp "h:mm A"
--                     , color Color.black700
--                     , margin $ MarginVertical 4 6
--                     ]
--                   <> FontStyle.captions TypoGraphy
--               , linearLayout
--                 [ width WRAP_CONTENT
--                 , height WRAP_CONTENT
--                 ][  imageView
--                     [ width $ V 20
--                     , height $ V 20
--                     , margin $ MarginRight 5
--                     , imageWithFallback $ getVehicleVariantImage item.vehicleVariant
--                     ]
--                   , textView $
--                     [ color Color.black700
--                     , background $ Color.grey900
--                     , text $ getString (BOOKING_FROM item.bapName)
--                     , padding $ Padding 10 4 10 4
--                     , cornerRadius 26.0
--                     , visibility $ boolToVisibility $ not item.isValueAddNP
--                     , margin $ MarginRight 5
--                     ] <> FontStyle.body17 TypoGraphy
--                   , linearLayout
--                     [ height WRAP_CONTENT
--                     , width WRAP_CONTENT
--                     , background $ Color.grey900
--                     , cornerRadius 26.0
--                     , padding $ PaddingHorizontal 5 5
--                     ] (map (\name -> (tagview name)) item.tagImages)
--                 ]
--               ]
--           , linearLayout
--               [ height WRAP_CONTENT
--               , width WRAP_CONTENT
--               , gravity CENTER
--               , background if rideStatus == "CANCELLED" then Color.red600 else Color.white900
--               , cornerRadius 100.0
--               , padding $ Padding 11 3 11 6
--               ]
--               [ textView
--                   $ [ text $ if rideStatus /= "CANCELLED" then "₹" <> formatCurrencyWithCommas (show earnings) else getString CANCELLED_
--                     , height WRAP_CONTENT
--                     , width WRAP_CONTENT
--                     , color color'
--                     ]
--                   <> FontStyle.body6 TypoGraphy
--               ]
--           ]
--       , separatorView true state.props.subView
--       ]

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
