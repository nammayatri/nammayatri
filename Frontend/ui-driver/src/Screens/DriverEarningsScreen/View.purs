{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverEarningsScreen.View where

import Common.Types.App
import Debug
import Screens.DriverEarningsScreen.ComponentConfig
import Engineering.Helpers.BackTrack (liftFlowBT)
import Animation (fadeIn, translateInYAnim)
import Animation as Anim
import Animation.Config (Direction(..), animConfig)
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
import Data.Array (length, (..), foldl, filter, (!!), null, last, mapWithIndex, any)
import Data.Either (Either(..), either)
import Data.Function.Uncurried (runFn1)
import Data.Int (ceil, floor, fromNumber, toNumber, fromString)
import Data.Maybe
import Data.Tuple as DT
import Data.String as DS
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn7)
import Engineering.Helpers.Commons (getCurrentUTC, flowRunner, getFormattedDate, getNewIDWithTag, screenHeight, getVideoID, getDayName, daysBetweenDates, safeMarginBottom, screenWidth, convertUTCtoISC, countDown, liftFlow, formatCurrencyWithCommas)
import Engineering.Helpers.Utils (loaderText, toggleLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Foreign.Generic (decodeJSON)
import Helpers.Utils (FetchImageFrom(..), toStringJSON, convertUTCtoISC, getFixedTwoDecimals, renderSlider, getcurrentdate, getPastDays, getDayOfWeek, generateUniqueId, addMediaPlayer, fetchImage, FetchImageFrom(..))
import JBridge (startLottieProcess, lottieAnimationConfig, getLayoutBounds, setYoutubePlayer)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (when, Unit, ($), (<$>), (||), const, (==), (<<<), void, bind, pure, unit, discard, show, not, map, (&&), ($), (<$>), (<>), (<<<), (==), (/), (>), (-), (/=), (<), (*), (>=), const, (||),  max, (+), class Eq)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (textFromHtml, scrollView, frameLayout, shimmerFrameLayout, layoutGravity, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, lottieAnimationView, alignParentBottom, background, calendar, clickable, color, cornerRadius, fontSize, fontStyle, gravity, height, horizontalScrollView, id, imageView, imageWithFallback, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, relativeLayout, scrollBarX, scrollBarY, stroke, swipeRefreshLayout, text, textSize, textView, visibility, weight, width, onAnimationEnd, alpha, singleLine)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Services.API (GetRidesHistoryResp(..), GetRidesSummaryListResp(..), DriverProfileSummaryRes(..))
import Screens as ScreenNames
import Screens.DriverEarningsScreen.Controller (Action(..), ScreenOutput, eval, fetchWeekyEarningData, dummyQuestions)
import Screens.DriverEarningsScreen.ScreenData (dummyDateItem)
import Screens.Types (DriverEarningsScreenState)
import Screens.Types as ST
import Services.API as API
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Data.Tuple (Tuple(..))
import Common.Types.App (YoutubeData(..))
import Data.Function.Uncurried (runFn3)
import PrestoDOM.Elements.Keyed as Keyed

screen :: ST.DriverEarningsScreenState -> Screen Action ST.DriverEarningsScreenState ScreenOutput
screen initialState =
  {
    initialState : initialState
  , view : view
  , name : "DriverEarningsScreen"
  , globalEvents : [
    globalOnScroll "DriverEarningsScreen",
        ( \push -> do
            _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
              case initialState.props.subView of
                ST.EARNINGS_VIEW -> do
                  when (not initialState.data.anyRidesAssignedEver) $ do
                    profileSummaryResponse <-  lift $ lift $ Remote.driverProfileSummary ""
                    let anyRidesAssignedEver = either (\_ -> false) (\(DriverProfileSummaryRes profileSummaryResponse) -> profileSummaryResponse.totalRidesAssigned > 0) profileSummaryResponse
                    liftFlowBT $ push $ UpdateRidesEver anyRidesAssignedEver
                  (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "100" "0" "false" "null" (convertUTCtoISC initialState.props.date "YYYY-MM-DD")
                  liftFlowBT $ push $ RideHistoryAPIResponseAction rideHistoryResponse.list
                  let currentDate = getcurrentdate ""
                      datesList = getDatesList currentDate initialState
                  (GetRidesSummaryListResp rideSummaryResp) <- Remote.getRideSummaryListReqBT datesList
                  liftFlowBT $ push $ RideSummaryAPIResponseAction rideSummaryResp.list currentDate datesList
                ST.YATRI_COINS_VIEW -> do
                  coinTransactionRes <- Remote.getCoinTransactionsReqBT initialState.props.date
                  liftFlowBT $ push $ CoinTransactionResponseAction coinTransactionRes
                  pure unit
                ST.USE_COINS_VIEW -> do
                  coinUsageHistoryRes <- Remote.getCoinUsageHistoryReqBT ""
                  liftFlowBT $ push $ CoinUsageResponseAction coinUsageHistoryRes
                  pure unit
                _ -> do
                  liftFlowBT $ push $ FaqViewAction 
                  pure unit
            pure $ pure unit
        )
  ]
  , eval : (\action state -> do 
    let _ = spy "DriverEarningsScreenState action" action
    let _ = spy "DriverEarningsScreenState state" state 
    eval action state)
  }

getDatesList :: String -> ST.DriverEarningsScreenState -> Array String
getDatesList todaysDate state = do
  let storedRideSummaryData = fromMaybe [] (fetchWeekyEarningData RIDE_SUMMARY_DATA)
  if length storedRideSummaryData > 0 then map (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") case last storedRideSummaryData of 
                            Just x -> getPastDays (daysBetweenDates x.rideDate todaysDate)
                            Nothing -> getPastDays (22 + getDayOfWeek (getDayName todaysDate))
  else map (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") (getPastDays (22 + getDayOfWeek (getDayName todaysDate)))

view :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
view push state =
   relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    , background Color.white900
    ][ frameLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.blue600
        ][Anim.screenAnimationFadeInOut
          $ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , visibility if state.props.showShimmer then GONE else VISIBLE
            ][ tabView push state
              , case state.props.subView of
                  ST.USE_COINS_VIEW -> do
                     if state.data.config.coinsConfig.enableYatriCoins then 
                     linearLayout 
                      [ width MATCH_PARENT
                      , gravity CENTER_VERTICAL
                      , padding $ PaddingRight 16
                      , background Color.white900
                      ]
                      [ linearLayout 
                          [ width WRAP_CONTENT
                          ][  GenericHeader.view (push <<< GenericHeaderAC )  (genericHeaderConfig state ) 
                          ]
                        , helpButton push
                        ]
                      else linearLayout[][]
                  _ | any (_ == state.props.subView)[ST.FAQ_VIEW, ST.FAQ_QUESTON_VIEW] -> GenericHeader.view (push <<< GenericHeaderAC )  (genericHeaderConfig state ) 
                  _ -> linearLayout[][]
              , separatorView (if any (_ == state.props.subView) [ST.FAQ_VIEW, ST.FAQ_QUESTON_VIEW] then true else false) state.props.subView
              , if not state.data.anyRidesAssignedEver
                  then  noRideHistoryView push state
                  else  scrollView 
                        [ height MATCH_PARENT
                        , width MATCH_PARENT
                        , orientation VERTICAL
                        ][ 
                          case state.props.subView of
                            ST.EARNINGS_VIEW -> earningsView push state
                            ST.YATRI_COINS_VIEW -> yatriCoinsView push state
                            ST.USE_COINS_VIEW -> useCoinsView push state
                            ST.FAQ_VIEW -> faqView push state
                            ST.FAQ_QUESTON_VIEW -> faqQuestionView push state
                        ]
            ]
          , if state.props.showShimmer then shimmerView push state else dummyView
          ]
        , if DS.null state.props.showCoinsRedeemedAnim then dummyView else lottieView state push
        , if state.props.popupType /= ST.NO_POPUP then PopUpModal.view (push <<< PopUpModalAC) (earningsPopupConfig state) else dummyView
        , if state.props.calendarState.calendarPopup then Calendar.view (push <<< CalendarAC) (calendarConfig state) else dummyView
        , if state.props.showCoinsUsagePopup then coinsUsagePopup push state else dummyView
        , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.DRIVER_EARNINGS_SCREEN state.data.config.bottomNavConfig)
    ]
    where showGenericHeader = state.props.subView == ST.USE_COINS_VIEW || not state.data.config.coinsConfig.enableYatriCoins

lottieView :: forall w . ST.DriverEarningsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
lottieView state push =
  let id' = (getNewIDWithTag "lottieView")
  in relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  , visibility $ if DS.null state.props.showCoinsRedeemedAnim then GONE else VISIBLE
  , gravity CENTER
  ][
    Anim.screenAnimationFadeInOut
        $ lottieAnimationView
    [ id id'
    , onAnimationEnd (\action-> do
                  liftEffect $ countDown state.data.timer "lottieTimer" push CountDown
                  void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = state.props.showCoinsRedeemedAnim, lottieId = id', scaleType = "FIT_CENTER"}
                  )(const NoAction)
    , height MATCH_PARENT
    , width WRAP_CONTENT
    ]
  ]

tabView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
tabView push state = 
 let setVisibility = if ((state.props.subView == ST.EARNINGS_VIEW || state.props.subView == ST.YATRI_COINS_VIEW) && state.data.config.coinsConfig.enableYatriCoins) then VISIBLE else GONE
 in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 24.0 
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , padding $ Padding 4 4 4 4
  , margin $ Margin 16 24 16 24
  , gravity CENTER
  , visibility $ setVisibility
  ][  tabItem push (state.props.subView == ST.EARNINGS_VIEW) EARNINGS ST.EARNINGS_VIEW "ny_ic_tab_earnings"
    , tabItem push (state.props.subView == ST.YATRI_COINS_VIEW) YATRI_COINS ST.YATRI_COINS_VIEW "ny_ic_tab_coins"
  ]

tabItem :: forall w . (Action -> Effect Unit) -> Boolean -> STR -> ST.DriverEarningsSubView -> String -> PrestoDOM (Effect Unit) w
tabItem push isActive text' subView img = 
  let 
    imageHeight = if img == "ny_ic_tab_coins" then V 16 else V 12
    imageWidth = if img == "ny_ic_tab_coins" then V 16 else V 20
  in
    linearLayout 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , padding $ PaddingVertical 6 8
      , weight 1.0 
      , background if isActive then Color.black900 else Color.white900
      , gravity CENTER
      , cornerRadius 24.0
      , onClick push $ const $ ChangeTab subView
      ] 
      [ textView $
          [ height WRAP_CONTENT
          , text $ getString text'
          , color if isActive then Color.white900 else Color.black700
          , padding $ PaddingBottom 3
          , margin $ if getValueToLocalStore LANGUAGE_KEY == "KN_IN" then MarginTop 4 else MarginTop 0 
          ] <> FontStyle.tags TypoGraphy
      , imageView 
          [ imageWithFallback $ fetchImage FF_ASSET img
          , height imageHeight
          , width imageWidth
          , margin $ MarginLeft 3
          ]
      ]

earningsView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
earningsView push state = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    , padding $ Padding 16 0 16 70
    ][ totalEarningsView push state 
     , transactionViewForEarnings push state
    ]

shimmerView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
shimmerView push state = 
  shimmerFrameLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.blue600
  ][ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][ linearLayout
      [ height (V 350)
      , width MATCH_PARENT
      , background Color.greyDark
      , cornerRadius 12.0
      , orientation VERTICAL
      ][]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginTop 24
      ]( map(\item ->
          linearLayout 
            [ width MATCH_PARENT
            , height (V 60)
            , orientation VERTICAL
            , margin $ MarginVertical 10 10
            , background Color.greyDark
            , cornerRadius 12.0
            ][]
      ) (1 .. 3)  
      )  
    ]]

totalEarningsView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
totalEarningsView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , padding $ Padding 20 20 20 16
    , cornerRadius 12.0
    , stroke $ "1," <> Color.grey900
    , orientation VERTICAL
    ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , gravity CENTER
        ][ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , weight 1.0
        , gravity LEFT
        ][ imageView $
          [ width (V 32)
          , height (V 32)
          , imageWithFallback $ fetchImage FF_ASSET $ if state.props.weekIndex == 0 then "ny_ic_chevron_left_light_grey" else "ny_ic_chevron_left" 
          , onClick push $ const $ LeftChevronClicked state.props.weekIndex
          , clickable if state.props.weekIndex > 0 then true else false
          ]
        ]
        ,linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation VERTICAL
          , gravity CENTER
          ][ textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ convertUTCtoISC state.props.totalEarningsData.fromDate "DD MMM"
                    <> (if DS.null state.props.totalEarningsData.toDate
                            then ""
                            else " - " <> convertUTCtoISC state.props.totalEarningsData.toDate "DD MMM")
                    <> ", " <> getString EARNINGS
            ] <> FontStyle.paragraphText TypoGraphy
            , textView $ 
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ "₹" <> (formatCurrencyWithCommas (show state.props.totalEarningsData.totalEarnings))
            , color Color.black900
            ] <> FontStyle.priceFont TypoGraphy
          ]
          , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity RIGHT
          , weight 1.0
          ][ imageView $
            [ width $ V 32
            , height $ V 32
            , imageWithFallback $ fetchImage FF_ASSET $ if state.props.weekIndex == 3 then "ny_ic_chevron_right_light_grey" else "ny_ic_chevron_right"
            , gravity RIGHT
            , onClick push $ const $ RightChevronClicked state.props.weekIndex
            , clickable $ state.props.weekIndex < 3
            ]
          ]
        ]
      , barGraphView push state
      , separatorView true state.props.subView
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , padding $ PaddingVertical 15 15
        ][ linearLayout
          [ height MATCH_PARENT
          , gravity CENTER
          , width $ V (((screenWidth unit) - 75)/2)
          ][ textView $ 
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ getString RIDES
              , margin $ MarginRight 8
              ] <> FontStyle.paragraphText TypoGraphy
              ,  textView $ 
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ show $ state.props.totalEarningsData.totalRides
              ] <> FontStyle.h2 TypoGraphy
          ]
          ,linearLayout
            [ height $ V 30
            , width $ V 2
            , background Color.grey900
            ][]
          , linearLayout
            [ height WRAP_CONTENT
            , gravity CENTER
            , width $ V (((screenWidth unit) - 75)/2)
            ][ textView $ 
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ getString DISTANCE
              , margin $ MarginRight 8
              ] <> FontStyle.paragraphText TypoGraphy
              ,  textView $ 
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ show (state.props.totalEarningsData.totalDistanceTravelled / 1000) <> " km"
              ] <> FontStyle.h2 TypoGraphy
          ]
        ] 
      , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER
        , margin $ MarginTop 6
        ]( map (\index -> dotView push index state) [0,1,2,3])
    ]

dotView :: forall w . (Action -> Effect Unit) -> Int -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
dotView push index state = 
  linearLayout
  [ height $ V 6
  , width $ V 6
  , cornerRadius 12.0
  , background $ if index == state.props.weekIndex then Color.black800 else Color.grey900
  , margin $ MarginHorizontal 2 2
  ][]

yatriCoinsView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
yatriCoinsView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 16 0 16 70
  ] [  balanceView push state
      -- , insightView push state
      , transactionView push state
  ]

balanceView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
balanceView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , padding $ Padding 20 20 20 16
    , cornerRadius 12.0
    , stroke $ "1," <> Color.grey900
    ] [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        ][ linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , background Color.white900
          , orientation VERTICAL
          , weight 1.0
          ][ textView $ [
              text $ getString COIN_BALANCE
            ] <> FontStyle.paragraphText TypoGraphy
            , linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity BOTTOM
              ][ textView $ [
                text $ formatCurrencyWithCommas (show state.data.coinBalance)
              -- , color Color.yellow900
              , color Color.saffron
            ] <> FontStyle.priceFont TypoGraphy
              , linearLayout 
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , background if state.data.coinsEarnedPreviousDay >= 0 then Color.greenCoin else Color.redCoin
                , margin $ Margin 4 0 0 8
                , cornerRadius 100.0 
                , gravity CENTER_VERTICAL
                , padding $ Padding 8 3 8 3
                ][ textView $ [
                    text $ formatCurrencyWithCommas (show state.data.coinsEarnedPreviousDay)
                  , color if state.data.coinsEarnedPreviousDay >= 0 then Color.green900 else Color.red  
                  ] <> FontStyle.paragraphText TypoGraphy
                  , textView $ [
                      text if state.data.coinsEarnedPreviousDay >= 0 then "↑" else "↓"
                      , color if state.data.coinsEarnedPreviousDay >= 0 then Color.green900 else Color.red
                      , margin $ MarginBottom 2
                    ] <> FontStyle.subHeading1 TypoGraphy      
                ]
                ]
          ]
        , imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_coin_balance,"
          , height $ V 62
          , width $ V 94
          ]
        ]
      , separatorView true state.props.subView
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginTop 16
        ][
          linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , weight 1.0
          , gravity CENTER_VERTICAL
          ] [ textView $ [
                text $ getString TOTAL_EARNED
              , color Color.black700     
              , margin $ Margin 0 1 8 0
              ] <> FontStyle.body3 TypoGraphy      
            , textView $ [
                text $ formatCurrencyWithCommas (show state.data.coinsEarned)
              , color Color.black700     
              ] <> FontStyle.subHeading1 TypoGraphy    
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER_VERTICAL
          ] [ textView $ [
                text $ getString COINS_USED
              , color Color.black700     
              , margin $ Margin 0 1 8 0
              ] <> FontStyle.body3 TypoGraphy      
            , textView $ [
                text $ formatCurrencyWithCommas (show state.data.coinsUsed)
              , color Color.black700     
              ] <> FontStyle.subHeading1 TypoGraphy     
          ]
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , onClick push $ const $ ChangeTab ST.USE_COINS_VIEW
        , gravity CENTER
        , padding $ PaddingVertical 12 12
        , margin $ MarginTop 16
        , cornerRadius 6.0
        , background Color.blue600
        ] [ textView $ [
              text $ getString USE_COINS <> "  →"
            , color Color.blue800     
            , margin $ MarginRight 8
            ] <> FontStyle.body1 TypoGraphy       
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , padding $ Padding 12 16 12 16
        , margin $ MarginTop 12
        , cornerRadius 6.0
        , background Color.yellowOpacity40
        , visibility if state.data.expiringCoins > 0 then VISIBLE else GONE
        ] [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_coin_expire,"
            , height $ V 14
            , width $ V 14
            , margin $ MarginRight 10
            ]
          , textView $ [
              text $ getVarString COINS_EXPIRING_IN_THE_NEXT $ show <$> [ state.data.expiringCoins, state.data.expiringDays]
            , color Color.black700    
            ] <> FontStyle.tags TypoGraphy       
        ]
    ]

-- insightView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
-- insightView push state = 
--   linearLayout
--   [ height WRAP_CONTENT
--   , width MATCH_PARENT
--   , orientation VERTICAL
--   , margin $ MarginTop 20
--   ][  textView $ [
--         text $ getString INSIGHTS
--       , color Color.black900
--       ] <> FontStyle.h2 TypoGraphy
--     , horizontalScrollView
--       [ width MATCH_PARENT
--       , height WRAP_CONTENT
--       , orientation HORIZONTAL
--       , margin $ MarginVertical 12 12
--       ][ linearLayout
--           [ width MATCH_PARENT
--           , height MATCH_PARENT
--           , orientation HORIZONTAL
--           ](map(\item -> badgeView item) dummyBadges)
--       ]
--     ]

-- badgeView :: forall w. {badgeImage :: String, primaryText :: String, subText :: String} -> PrestoDOM (Effect Unit) w
-- badgeView state =
--   linearLayout
--     [ width WRAP_CONTENT
--     , height WRAP_CONTENT
--     , stroke $ "1," <> Color.grey900
--     , background Color.white900
--     , cornerRadius 15.0
--     , padding $ Padding 16 10 16 12
--     , margin $ MarginRight 16
--     ][ imageView
--       [ width $ V 65
--       , height $ V 65
--       , imageWithFallback $ fetchImage FF_ASSET state.badgeImage
--       ]
--     , linearLayout
--       [ width WRAP_CONTENT
--       , height MATCH_PARENT
--       , orientation VERTICAL
--       , gravity CENTER_VERTICAL
--       ][ textView $
--         [ text state.subText
--         , color Color.black800
--         ] <> FontStyle.captions TypoGraphy
--       , textView $
--         [ text state.primaryText
--         , color Color.black900
--         ] <> FontStyle.body1 TypoGraphy
--       ]
--     ]

-- dummyBadges :: Array ST.Badge
-- dummyBadges = [{badgeImage: "ny_ic_five_star_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_five_star_badge.png"
--                      , primaryText: "5-Star Rides"
--                      , subText: "235"
--                       },
--                       {badgeImage: "ny_ic_safe_ride,https://assets.juspay.in/nammayatri/images/driver/ny_ic_safe_ride.png"
--                      , primaryText: "Safe Rides"
--                      , subText: "235"
--                       },
--                       {badgeImage: "ny_ic_clean_auto_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_clean_auto_badge.png"
--                      , primaryText: "Clean Auto"
--                      , subText: "235"
--                       },
--                       {badgeImage: "ny_ic_expert_driver,https://assets.juspay.in/nammayatri/images/driver/ny_ic_expert_driver.png"
--                      , primaryText: "Expert Driving"
--                      , subText: "235"
--                       },
--                       {badgeImage: "ny_ic_navigator_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_navigator_badge.png"
--                      , primaryText: "Navigator"
--                      , subText: "235"
--                       },
--                       {badgeImage: "ny_ic_ontime_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_ontime_badge.png"
--                      , primaryText: "On Time"
--                      , subText: "235"
--                       },
--                       {badgeImage: "ny_ic_polite_driver_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_polite_driver_badge.png"
--                      , primaryText: "Professional"
--                      , subText: "235"
--                       }
--                       ]
  
faqView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
faqView push state = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.blue600
    ][  textView $ [
        text $ getString LEARN_ABOUT_YATRI_COINS
      , color Color.black700
      , margin $ Margin 16 12 0 12
      ] <> FontStyle.subHeading2 TypoGraphy
      , questionsListView push dummyQuestions state
    ]

questionsListView :: forall w . (Action -> Effect Unit) -> Array ST.FaqQuestions -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
questionsListView push questionsList state = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background $ Color.white900
    ] (map (\item -> individualQuestionListView push item state) questionsList)

individualQuestionListView :: forall w . (Action -> Effect Unit) -> ST.FaqQuestions -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
individualQuestionListView push faqQuestion state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (PaddingHorizontal 16 16)
    , onClick push $ const $ OpenFaqQuestion faqQuestion
    ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
          ][ linearLayout
              [ height MATCH_PARENT
              , weight 1.0
              , gravity CENTER_VERTICAL
                ][ textView (
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , text faqQuestion.question
                    , padding (PaddingVertical 20 20)
                    , color Color.black900
                    , margin $ MarginRight 16
                    ] <> FontStyle.subHeading2 TypoGraphy)
                ]
              , linearLayout
                [ height MATCH_PARENT
                , width $ V 20
                , gravity CENTER
                  ][ imageView
                      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_grey,"
                      , height $ V 18
                      , width $ V 18
                      , margin $ MarginHorizontal 10 8 
                      ]
                  ]
          ]
        , horizontalLine
      ]

faqQuestionView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
faqQuestionView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , padding (Padding 16 16 16 16)
    ][ textView (
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text state.props.individualQuestion.question
        , color Color.black900
        , margin $ MarginBottom 20
        ] <> FontStyle.h1 TypoGraphy)
        , answersListView push state.props.individualQuestion.answer state
    ]

faqVideoView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
faqVideoView push state = 
  if state.props.individualQuestion.videoLink /= Nothing
    then linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , background Color.red600 
          , gravity CENTER
          , margin $ MarginBottom 4
          , id (getNewIDWithTag "faqVideo")
          , afterRender
              ( \action ->  pure $ runFn3 setYoutubePlayer (youtubeData state "VIDEO") (getNewIDWithTag "faqVideo") (show ST.PLAY))
              (const NoAction)
          ][]
      else linearLayout [][]

youtubeData :: ST.DriverEarningsScreenState -> String -> YoutubeData
youtubeData state mediaType =
  { videoTitle: "title"
  , setVideoTitle: false
  , showMenuButton: false
  , showDuration: true
  , showSeekBar: true
  -- , videoId: getVideoID state.props.individualQuestion.videoLink
  , videoId : case state.props.individualQuestion.videoLink of
      Just x -> getVideoID x
      Nothing -> ""
  , videoType: "VIDEO"
  , videoHeight: 200
  }

answersListView :: forall w . (Action -> Effect Unit) -> Array String -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
answersListView push answersList state = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ] $ [faqVideoView push state] <> if length answersList > 1 
        then (map (\item -> singleAnswerView push item state) answersList)
        else (map (\item -> multipleAnswerView push item state) answersList)

singleAnswerView :: forall w . (Action -> Effect Unit) -> String -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
singleAnswerView push answer state =
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding (PaddingVertical 10 10)
    ][ textView (
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text "•"
        , margin $ MarginRight 8
        , color Color.black900
        ] <> FontStyle.subHeading2 TypoGraphy),
        textView (
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text answer
        , color Color.black900
        ] <> FontStyle.subHeading2 TypoGraphy)
    ]

multipleAnswerView :: forall w . (Action -> Effect Unit) -> String -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
multipleAnswerView push answer state =
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding (PaddingVertical 12 12)
    ][ textView (
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text answer
        , color Color.black900
        ] <> FontStyle.subHeading2 TypoGraphy)
    ]


horizontalLine :: forall w. PrestoDOM (Effect Unit) w 
horizontalLine = 
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ,gravity CENTER
    ][] 

helpButton :: forall w . (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
helpButton push = 
  linearLayout
    [ gravity RIGHT
    , weight 1.0
    , width WRAP_CONTENT
    , onClick push $ const $ ChangeTab ST.FAQ_VIEW
    ][  imageView $
        [ width (V 22)
        , height (V 22)
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_help_circle_blue,"
        , padding $ Padding 0 2 4 0 
        , gravity CENTER_VERTICAL
        ]
      , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ getString HELP_STR
        , color $ Color.blue900
        ] <> FontStyle.subHeading2 TypoGraphy
    ]

transactionView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
transactionView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 20
    ] [ linearLayout 
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , margin $ Margin 0 0 8 13
          ] [ textView $ [
              text $ getString TRANSACTION_HISTORY
            , weight 1.0
            , color Color.black800     
            ] <> FontStyle.h2 TypoGraphy      
            , calendarView push state    
          ]
          , historyView push state
    ]

calendarView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
calendarView push state = 
  let selectedDate = state.props.date
      dateToShow = if DS.null selectedDate || convertUTCtoISC selectedDate "YYYY-MM-DD" == getcurrentdate "" 
                   then getString TODAY 
                   else convertUTCtoISC selectedDate "Do MMM"
  in
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , cornerRadius 100.0
    , background Color.white900
    , gravity CENTER_VERTICAL
    , stroke $ "1," <> Color.grey900
    , padding $ Padding 12 4 12 4
    , onClick push $ const $ ShowCalendarPopup
    ][ imageView [
        imageWithFallback $ fetchImage FF_ASSET "ny_ic_calendar_unfilled_blue,"
      , height $ V 16
      , width $ V 16
      , margin $ MarginRight 4
      ]
      , textView $ [
        text dateToShow
      , color Color.black700
      ] <> FontStyle.tags TypoGraphy
      , imageView [
        imageWithFallback $ fetchImage FF_ASSET if state.props.calendarState.calendarPopup 
                          then "ny_ic_chevron_up,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_up.png"
                          else "ny_ic_chevron_down,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_down.png"
      , height $ V 12
      , width $ V 12
      , margin $ MarginLeft 6
      ]
    ] 

historyView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
historyView push state = 
  let historyItems = if state.props.subView == ST.USE_COINS_VIEW then state.data.usageHistoryItems else state.data.coinHistoryItems
  in
  if null historyItems
    then noItemsView state.props.subView 
    else
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    , cornerRadius 12.0
    ] [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding $ Padding 16 12 16 12
        , background Color.atlantisGreen
        , cornerRadii $ Corners 12.0 true true false false
        , gravity CENTER_VERTICAL
        ][ textView $ [
            text $ "₹" <> formatCurrencyWithCommas (getFixedTwoDecimals state.data.totalCoinConvertedToCash)
          , color Color.black800     
          , visibility if state.props.subView == ST.USE_COINS_VIEW then VISIBLE else GONE
          , margin $ MarginRight 8
          ] <> FontStyle.h2 TypoGraphy
        , textView $ [
            text case state.props.subView of
                    ST.YATRI_COINS_VIEW -> getString COINS_EARNED
                    ST.USE_COINS_VIEW -> getString CASH_CONVERTED
                    _ -> ""
          , weight 1.0
          , color Color.black700     
          , margin $ MarginRight 8
          ] <> FontStyle.paragraphText TypoGraphy      
        , textView $ [
            text $ formatCurrencyWithCommas case state.props.subView of
                                                ST.YATRI_COINS_VIEW -> show state.data.coinsEarnedToday
                                                ST.USE_COINS_VIEW -> show state.data.coinsUsed
                                                _ -> ""
          , color Color.black800     
          ] <> FontStyle.h2 TypoGraphy  
        , imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_yatri_coin,"
          , height $ V 16
          , width $ V 16
          , margin $ MarginLeft 4
          ]
        ]     
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          ] (mapWithIndex (\index item -> historyViewItem item (index == length historyItems - 1) state.props.subView) historyItems)
    ]


historyViewItem :: forall w . ST.CoinHistoryItem -> Boolean -> ST.DriverEarningsSubView -> PrestoDOM (Effect Unit) w
historyViewItem item isLast subView = 
  let color' = case subView, item.coins > 0 of
                  ST.YATRI_COINS_VIEW, true  ->  Color.green900
                  ST.YATRI_COINS_VIEW, false ->  Color.red
                  _, _       ->  Color.black800
      coinValue = case subView, item.coins > 0 of
                  ST.YATRI_COINS_VIEW, true  -> "+" <> formatCurrencyWithCommas (show item.coins)
                  ST.YATRI_COINS_VIEW, false -> formatCurrencyWithCommas (show item.coins)
                  _, _       ->  "-" <> formatCurrencyWithCommas (show item.coins)
  in
  linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding $ Padding 16 12 16 12
    ][
      linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , weight 1.0
      , orientation VERTICAL
      ][ linearLayout
         [ height WRAP_CONTENT
         , width WRAP_CONTENT
         ][ textView $ [
              text $ "₹" <> formatCurrencyWithCommas (show item.cash) <> " " <> getString CONVERTED_FROM_COINS
            , color Color.black900
            , visibility if subView == ST.USE_COINS_VIEW then VISIBLE else GONE
            ] <> FontStyle.body3 TypoGraphy
          , textView $ [
              text item.event
            , visibility if subView == ST.YATRI_COINS_VIEW then VISIBLE else GONE
            , color Color.black900    
            ] <> FontStyle.body3 TypoGraphy
        ]
         
      , textView $ [
          text $ convertUTCtoISC item.timestamp "DD/MM/YYYY" <> "  " <>  "•" <> "  " <> convertUTCtoISC item.timestamp "h:mm A"
        , color Color.black700  
        , margin $ MarginTop 4
        ] <> FontStyle.captions TypoGraphy 
      ]
    , linearLayout
      [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
      ][ textView $ [
          text $ coinValue
        , color color'
        , padding $ PaddingBottom 2
          ] <> FontStyle.body6 TypoGraphy
        , imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_yatri_coin"
          , height $ V 15
          , width $ V 15
          , margin $ MarginHorizontal 6 6
          ]
      ]
    ]
    , separatorView (not isLast) subView
  ]


useCoinsView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
useCoinsView push state = 
  scrollView 
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ PaddingBottom 50
  ] [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][ convertCoinsView push state
       , usageHistoryView push state
      ] 
  ]

convertCoinsView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w  
convertCoinsView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ] [ coinsUsageAlertView push state
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , padding $ Padding 16 18 16 18
        , margin $ Margin 16 20 16 0
        , cornerRadius 8.0    
        , background Color.white900
        , stroke $ "1," <> Color.grey900
        ] [ textView $ [
              text $ getString COIN_BALANCE
            , color Color.black700  
            , margin $ MarginRight 8
            , weight 1.0
            ] <> FontStyle.paragraphText TypoGraphy       
          , textView $ [
              text $ formatCurrencyWithCommas (show state.data.coinBalance)
            , color "#FCC32C"
            , margin $ MarginRight 8
            ] <> FontStyle.h2 TypoGraphy  
          , imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_yatri_coin,"
            , height $ V 16
            , width $ V 16
            , margin (Margin 4 0 0 0)
            ]     
          ]
      , if state.data.coinBalance < state.data.config.coinsConfig.minCoinSliderValue
          then alertView push "ny_ic_info_yellow" (getString MINIMUM <> " " <> show state.data.config.coinsConfig.minCoinSliderValue <> " " <> getString COINS_IS_REQUIRED_FOR_CONVERSION) false Color.yellowOpacity40 8.0 (Margin 16 20 16 0)
        else if not state.data.hasActivePlan
          then alertView push "ny_ic_info_yellow" (getString USING_COINS_REQUIRES_AN_ACTIVE_PLAN <> " " <> getString CHOOSE_A_PLAN_TO_GET_STARTED) false Color.yellowOpacity40 8.0 (Margin 16 20 16 0)
        else dummyView
      , convertView push state
      ]  

coinsUsageAlertView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
coinsUsageAlertView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , cornerRadii $ Corners 24.0 false false true true
  , background Color.yellowOpacity40
  ][ case state.data.coinConvertedToCashUsedForLatestDues of
        Just val -> alertView push "ny_ic_check_green" ("₹" <> show val <> " " <> getString HAS_BEEN_ADJUSTED_IN_YOUR_SUBSCRIPTION_DUES) true Color.transparent 0.0 (Margin 0 0 0 0) 
        Nothing -> dummyView
   , if state.data.coinConvertedTocashLeft /= 0.0 
        then alertView push "ny_ic_info_yellow" ("₹" <> show state.data.coinConvertedTocashLeft <> " " <> getString WILL_BE_ADJUSTED_IN_YOUR_FUTURE_SUBSCRIPTION_DUES) false Color.transparent 0.0 (Margin 0 0 0 0)
     else dummyView
    ]

alertView :: forall w . (Action -> Effect Unit) -> String -> String -> Boolean -> String -> Number -> Margin -> PrestoDOM (Effect Unit) w
alertView push image message showButton backgroundColor cornerRadius' margin'= 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER
  , background backgroundColor
  , cornerRadius cornerRadius'
  , margin margin'
  ] [ linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , weight 1.0
      , gravity CENTER
      , padding $ Padding 16 8 16 8
      ][ imageView
         [ imageWithFallback $ fetchImage FF_ASSET $ image 
         , height $ V 14
         , width $ V 14
         , margin $ MarginRight 8
         ]  
       , textView $ [
          text message
         , color Color.black700
         , weight 1.0
         ] <> FontStyle.tags TypoGraphy
      ]
    , textView $ [
        text $ getString VIEW_DETAILS
      , padding $ Padding 16 16 16 16
      , color Color.blue800
      , visibility if showButton then VISIBLE else GONE
      ] <> FontStyle.tags TypoGraphy
  ]

convertView ::  forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w  
convertView push state = 
  let bounds = runFn1 getLayoutBounds $ getNewIDWithTag "ConvertCoinsSliderView"
      marginLeft' = (state.data.coinsToUse * bounds.width) / 100
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 16 20 16 20
  , orientation VERTICAL
  ][ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , margin $ MarginBottom 12
    ] [ textView $ [
          text $ getString CONVERT
        , color Color.black800
        ] <> FontStyle.h2 TypoGraphy
      , imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_grey,"
        , height $ V 14
        , width $ V 14
        , margin $ Margin 4 1 0 0
        , onClick push $ const $ ShowCoinsUsagePopup
        ]
      ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , cornerRadius 12.0
    , background Color.white900
    , stroke $ "1," <> Color.grey900
    , alpha if state.data.coinBalance < state.data.config.coinsConfig.minCoinSliderValue || not state.data.hasActivePlan then 0.6 else 1.0
    , padding $ Padding 16 16 16 16
    ][  frameLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          ][  linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER
              , clickable $ state.data.coinBalance >= state.data.config.coinsConfig.minCoinSliderValue && state.data.hasActivePlan
              ] [ textView $ [
                    text $ show state.data.config.coinsConfig.minCoinSliderValue
                  , color Color.black700
                  ] <> FontStyle.body3 TypoGraphy
                , Anim.screenAnimationFadeInOut $ linearLayout
                  [ height WRAP_CONTENT
                  , width $ V 260
                  , id $ getNewIDWithTag "ConvertCoinsSliderView"
                  , onAnimationEnd (\action -> runEffectFn7 renderSlider (getNewIDWithTag "ConvertCoinsSliderView") push SliderCallback state.data.coinConversionRate state.data.config.coinsConfig.minCoinSliderValue state.data.config.coinsConfig.maxCoinSliderValue state.data.config.coinsConfig.minCoinSliderValue 
                      )(const AfterRender)
                  ][]
                , textView $ [
                    text $ show state.data.config.coinsConfig.maxCoinSliderValue
                  , color Color.black700
                  ] <> FontStyle.body3 TypoGraphy
              ]
            , linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , visibility if state.data.coinBalance < state.data.config.coinsConfig.minCoinSliderValue || not state.data.hasActivePlan then VISIBLE else GONE
              , clickable true
              ][]
          ]
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig (state.data.coinBalance >= state.data.config.coinsConfig.minCoinSliderValue && state.data.hasActivePlan))
      ]
  ]

usageHistoryView ::  forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w  
usageHistoryView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ Margin 16 20 16 20
  ][  textView $ [
        text $ getString USAGE_HISTORY
      , color Color.black800
      , margin $ MarginBottom 12
      ] <> FontStyle.h2 TypoGraphy
    , historyView push state
    ]

separatorView :: forall w. Boolean -> ST.DriverEarningsSubView -> PrestoDOM (Effect Unit) w 
separatorView isVisible subView = 
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background $ if any (_ == subView)[ST.FAQ_VIEW, ST.FAQ_QUESTON_VIEW] then Color.grey900 else Color.grey700
  , visibility if isVisible then VISIBLE else GONE
  ][]

coinsUsagePopup :: forall w. (Action -> Effect Unit) -> DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
coinsUsagePopup push state =
   PrestoAnim.animationSet [ Anim.fadeIn true ]
     $ linearLayout
         [ height MATCH_PARENT
         , width MATCH_PARENT
         ]
         [ RequestInfoCard.view (push <<< RequestInfoCardAction) (coinsInfoCardConfig FunctionCall) ]

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = linearLayout[visibility GONE][]

noItemsView :: forall w. ST.DriverEarningsSubView -> PrestoDOM (Effect Unit) w
noItemsView subView = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , padding $ Padding 16 32 16 32
  , cornerRadius 12.0
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  ] [ textView $ [
        width WRAP_CONTENT
      , text $ case subView of
            ST.YATRI_COINS_VIEW -> getString NO_COINS_EARNED
            ST.EARNINGS_VIEW -> getString NO_RIDES
            _ -> getString NO_COINS_USED
      , color Color.black900
      ] <> FontStyle.subHeading1 TypoGraphy
    , textView $ [
        width WRAP_CONTENT
      , gravity CENTER
      , text $ case subView of
              ST.YATRI_COINS_VIEW -> getString EARN_COINS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS
              ST.EARNINGS_VIEW -> getString YOU_DID_NOT_TAKE_ANY_RIDES_ON_THE_SELECTED_DATE
              _ -> getString USE_THEM_BEFORE_THEY_EXPIRE
      , color Color.black700
      ] <> FontStyle.paragraphText TypoGraphy
  ]

transactionViewForEarnings :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
transactionViewForEarnings push state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 24
    ] [ linearLayout 
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , margin $ Margin 8 0 8 8
          ] [ textView $ [
              text $ getString RIDE_HISTORY
            , weight 1.0
            , color Color.black800     
            ] <> FontStyle.h2 TypoGraphy      
          , calendarView push state
            ]    
          , historyViewForEarnings push state
    ]


historyViewForEarnings :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
historyViewForEarnings push state = 
  let historyItems = state.data.earningHistoryItems 
  in
  if null historyItems then noItemsView state.props.subView 
  else
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    , cornerRadius 12.0
    ] [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding $ Padding 16 12 16 12
        , background Color.atlantisGreen
        , cornerRadii $ Corners 12.0 true true false false
        ][ textView $ [
            text $ getString RIDES
          , weight 0.0
          , color Color.black700     
          , margin $ MarginRight 8
          ] <> FontStyle.paragraphText TypoGraphy    
          , textView $ [
            text $ show (length (filter (\item -> item.status == Just "COMPLETED") state.data.earningHistoryItems))
          , color Color.black800 
          , weight 1.0
          ] <> FontStyle.h2 TypoGraphy    
          , textView $ [
          text $ getString EARNINGS
          , color Color.black700 
          , margin $ MarginRight 8
          ] <> FontStyle.paragraphText TypoGraphy  
          , textView $ [
            text $ "₹" <> formatCurrencyWithCommas (show (getDailyEarnings state.data.earningHistoryItems))
          , color Color.black800     
          ] <> FontStyle.h2 TypoGraphy  
        ]  
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          ] (map(\item ->  historyViewItemForEarnings item state)  state.data.earningHistoryItems)
    ]

dottedLineView :: forall w. (Action -> Effect Unit) -> Int -> Int -> PrestoDOM (Effect Unit) w 
dottedLineView push margintop earnings = 
  linearLayout [
    height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ MarginTop margintop
  , gravity CENTER
  ][
  imageView
  [ height $ V 2
  , width MATCH_PARENT
  , imageWithFallback $ fetchImage FF_ASSET "ny_ic_dotted_line,"
  , weight 1.0
  ]
  , textView $
      [ height $ WRAP_CONTENT
      , width $ WRAP_CONTENT
      , gravity RIGHT
      , text $ "₹" <> formatCurrencyWithCommas (show earnings)
      ] <> FontStyle.paragraphText TypoGraphy
  ]

barGraphView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w 
barGraphView push state = 
  let currWeekMaxEarning = if state.props.currentWeekMaxEarning > 0 then state.props.currentWeekMaxEarning else 1500
  in
  relativeLayout
  [ height $ V 150
  , width MATCH_PARENT
  ][dottedLineView push 4 currWeekMaxEarning
  , dottedLineView push 37 ((currWeekMaxEarning * 2)/3)
  , dottedLineView push 70 (currWeekMaxEarning/3)
  , linearLayout
  [ height $ V 2
  , width MATCH_PARENT
  , background Color.grey900
  , margin $ MarginTop 111
  ][]
  , linearLayout
  [ height $ V 150
  , width MATCH_PARENT
  , background Color.transparent
  , orientation HORIZONTAL
  , gravity BOTTOM
  ]  (mapWithIndex(\ index item  -> (barView push index item state)) state.props.currWeekData)
  ]

barView :: forall w. (Action -> Effect Unit) -> Int -> ST.WeeklyEarning -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w 
barView push index item state = 
  let selectedIndex = state.props.selectedBarIndex 
  in
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ WRAP_CONTENT
  , margin $ case index of
               0 -> MarginHorizontal 4 8
               6 -> MarginHorizontal 8 (screenWidth unit / 8)
               _ -> MarginHorizontal 8 8
  , weight 1.0
  , orientation VERTICAL
  ][ linearLayout
  [ height $ WRAP_CONTENT
  , width $ WRAP_CONTENT
  , onClick push $ const $ BarViewSelected index
  ][PrestoAnim.animationSet[Anim.translateInYAnim $ animConfig {duration = 1000 + (ceil item.percentLength) , fromY = 200 + (ceil item.percentLength)}] $
    linearLayout
    [ height $ WRAP_CONTENT
    , width $ WRAP_CONTENT
    ][  linearLayout
      [ height $ V if item.percentLength > 0.0 then (ceil item.percentLength) else 1
      , width $ V (screenWidth unit / 16)
      , background if selectedIndex < 0 || selectedIndex == index then Color.green900 else Color.green200
      , cornerRadius 4.0
      ][]
    ]
    ]
    , textView $
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , text $ DS.drop 8 item.rideDate
      , gravity CENTER
      ] <> FontStyle.paragraphText TypoGraphy
    , textView $
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , text $ (fromMaybe "" (state.props.weekDay !! index))
      , gravity CENTER
      , singleLine true
      , margin $ if getValueToLocalStore LANGUAGE_KEY == "KN_IN" then MarginBottom 0 else MarginBottom 2
      ] <> FontStyle.body9 TypoGraphy
  ]

getDailyEarnings :: Array ST.CoinHistoryItem -> Int
getDailyEarnings list = foldl (\acc record -> case record.earnings of
                                         Just x -> acc + x
                                         Nothing -> acc) 0 list


historyViewItemForEarnings :: forall w . ST.CoinHistoryItem -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
historyViewItemForEarnings item state = 
  let rideStatus = fromMaybe "" item.status
      color' = if rideStatus /= "CANCELLED" then Color.green900 else Color.red
      earnings = fromMaybe 0 item.earnings   
  in
  linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding $ Padding 16 12 16 10
    ][
      linearLayout
      [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , weight 1.0
        , orientation VERTICAL
      ][ textView $ [
          text $ getString DESTINATION <> ":" <> (fromMaybe "" item.destination)
        , color Color.black900    
        ] <> FontStyle.body3 TypoGraphy
      , textView $ [
          text $ convertUTCtoISC item.timestamp "DD/MM/YYYY" <> "  " <>  "•" <> "  " <> convertUTCtoISC item.timestamp "h:mm A"
        , color Color.black700  
        , margin $ MarginVertical 4 6
        ] <> FontStyle.captions TypoGraphy 
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , background $ Color.grey900
        , cornerRadius 26.0
        , padding $ PaddingHorizontal 5 5
        ] (map(\name  -> (tagview name)) item.tagImages) 
      ]
    , linearLayout
      [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER
        , background if rideStatus == "CANCELLED" then Color.red600 else Color.white900
        , cornerRadius 100.0
        , padding $ Padding 11 3 11 6
      ][ textView $ [
          text $ if rideStatus /= "CANCELLED" then "₹" <> formatCurrencyWithCommas (show earnings) else getString CANCELLED_
        , height WRAP_CONTENT
        , width WRAP_CONTENT
        , color color'
          ] <> FontStyle.body6 TypoGraphy
      ]
    ]
    , separatorView true state.props.subView
  ]

tagview :: String -> forall w. PrestoDOM (Effect Unit) w 
tagview name = imageView
          [ width (V 16)
          , height (V 16)
          , imageWithFallback $ fetchImage FF_ASSET name 
          , margin $ Margin 4 4 4 3
          ]

noRideHistoryView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
noRideHistoryView push state = 
    linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity CENTER
    , padding (PaddingBottom safeMarginBottom)
    ][  ErrorModal.view (push <<< ErrorModalActionController) (errorModalConfig state)]

