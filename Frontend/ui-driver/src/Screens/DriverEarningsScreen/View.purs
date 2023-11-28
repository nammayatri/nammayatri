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
import Data.Array (length, (..), foldl, filter, (!!), null, last)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn1)
import Data.Int (ceil, floor, fromNumber, toNumber, fromString)
import Data.Maybe
import Data.Tuple as DT
import Data.String as DS
import Data.String (take)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn7)
import Engineering.Helpers.Commons (getCurrentUTC, flowRunner, getFormattedDate, getNewIDWithTag, screenHeight, getDayName, daysBetweenDates, safeMarginBottom, screenWidth, convertUTCtoISC, countDown, liftFlow, formatCurrencyWithCommas)
import Engineering.Helpers.Utils (loaderText, toggleLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Foreign.Generic (decodeJSON)
import Helpers.Utils (convertUTCtoISC, getFixedTwoDecimals, renderSlider, getcurrentdate, getPastDays, getDayOfWeek, generateUniqueId, addMediaPlayer, getVideoID, setYoutubePlayer)
import JBridge (startLottieProcess, lottieAnimationConfig, getLayoutBounds)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (Unit, ($), (<$>), (||), const, (==), (<<<), void, bind, pure, unit, discard, show, not, map, (&&), ($), (<$>), (<>), (<<<), (==), (/), (>), (-), (/=), (<), (*), (>=), const, (||),  max, (+), class Eq)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (textFromHtml, scrollView, frameLayout, shimmerFrameLayout, layoutGravity, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, lottieAnimationView, alignParentBottom, background, calendar, clickable, color, cornerRadius, fontSize, fontStyle, gravity, height, horizontalScrollView, id, imageView, imageWithFallback, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, relativeLayout, scrollBarX, scrollBarY, stroke, swipeRefreshLayout, text, textSize, textView, visibility, weight, width, onAnimationEnd, alpha, singleLine)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Services.API (GetRidesHistoryResp(..), GetRidesSummaryListResp(..), DriverProfileSummaryRes(..))
import Screens as ScreenNames
import Screens.DriverEarningsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.DriverEarningsScreen.ScreenData (dummyDateItem)
import Screens.Types (DriverEarningsScreenState)
import Screens.Types as ST
import Services.API as API
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Helpers.Utils (toStringJSON)

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
              void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
              void $ lift $ lift $ toggleLoader true
              case initialState.props.subView of
                ST.EARNINGS_VIEW -> do
                  let date = case initialState.props.calendarState.startDate of
                                Just val -> val.utcDate
                                Nothing -> getcurrentdate ""
                  _ <- pure $ spy "hi there3" date
                  (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "100" "0" "false" "null" (convertUTCtoISC date "YYYY-MM-DD")
                  liftFlowBT $ push $ RideHistoryAPIResponseAction rideHistoryResponse.list
                  let currentDate = getcurrentdate ""
                  let datesList = getDatesList currentDate initialState
                  (GetRidesSummaryListResp rideSummaryResp) <- Remote.getRideSummaryListReqBT datesList
                  liftFlowBT $ push $ RideSummaryAPIResponseAction rideSummaryResp.list currentDate datesList
                ST.YATRI_COINS_VIEW -> do
                  setValueToLocalNativeStore VISITED_DRIVER_COINS_PAGE "true"
                  let date = case initialState.props.calendarState.startDate of
                                Just val -> val.utcDate
                                Nothing -> getCurrentUTC ""
                  coinTransactionRes <- Remote.getCoinTransactionsReqBT date
                  liftFlowBT $ push $ CoinTransactionResponseAction coinTransactionRes
                  pure unit
                ST.USE_COINS_VIEW -> do
                  setValueToLocalNativeStore VISITED_DRIVER_COINS_PAGE "true"
                  coinUsageHistoryRes <- Remote.getCoinUsageHistoryReqBT ""
                  liftFlowBT $ push $ CoinUsageResponseAction coinUsageHistoryRes
                  pure unit

              void $ lift $ lift $ toggleLoader false
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
  if length storedRideSummaryData > 0 then map (\x -> take 10 x.utcDate) case last storedRideSummaryData of 
                            Just x -> getPastDays (daysBetweenDates x.rideDate todaysDate)
                            Nothing -> getPastDays (22 + getDayOfWeek (getDayName todaysDate))
  else map (\x -> take 10 x.utcDate) (getPastDays (22 + getDayOfWeek (getDayName todaysDate)))

fetchWeekyEarningData :: KeyStore -> Maybe (Array ST.WeeklyEarning)
fetchWeekyEarningData name = do
 let result = decodeJSON $ getValueToLocalNativeStore name
 case (runExcept $ result) of
  Left err -> Nothing
  Right weeklyEarnings -> Just weeklyEarnings

view :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
view push state =
   relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    , background Color.white900
    ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][Anim.screenAnimationFadeInOut
        $ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Color.blue600
          ][ tabView push state
            , case state.props.subView of
                  ST.USE_COINS_VIEW -> do
                    linearLayout 
                      [ width MATCH_PARENT
                      , gravity CENTER_VERTICAL
                      , padding $ PaddingRight 16
                      ] [ linearLayout 
                          [ width WRAP_CONTENT
                          ][  GenericHeader.view (push <<< GenericHeaderAC )  (genericHeaderConfig state "Use Coins") 
                          ]
                        , helpButton push
                        ]
                  ST.FAQ_VIEW -> GenericHeader.view (push <<< GenericHeaderAC )  (genericHeaderConfig state "Yatri Coins FAQs") 
                  ST.FAQ_QUESTON_VIEW -> GenericHeader.view (push <<< GenericHeaderAC )  (genericHeaderConfig state "") 
            , scrollView [      
                height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ][ 
                case state.props.subView of
                  ST.EARNINGS_VIEW -> earningsView push state
                  ST.YATRI_COINS_VIEW -> yatriCoinsView push state
                  ST.USE_COINS_VIEW -> useCoinsView push state
              ]
          ]
          ]
        , if state.props.showCoinsRedeemedAnim /= "" then lottieView state push else dummyView
        , if state.props.popupType /= ST.NO_POPUP then PopUpModal.view (push <<< PopUpModalAC) (earningsPopupConfig state) else dummyView
        , if state.props.calendarState.calendarPopup then Calendar.view (push <<< CalendarAC) (calendarConfig state) else dummyView
        , if state.props.showCoinsUsagePopup then coinsUsagePopup push state else dummyView
        , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.DRIVER_EARNINGS_SCREEN state.data.config.bottomNavConfig)
    ]

earningsTab :: forall w . ST.DriverEarningsSubView -> (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
earningsTab subView push state = scrollView [      -- linearlayout 
                  height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , if (subView == ST.FAQ_VIEW) || (subView == ST.FAQ_QUESTON_VIEW) then background Color.mint else background Color.blue600
                ][Keyed.relativeLayout [                  
                  height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL] $[
                  --    Tuple "Earnings" $ earningsView push state
                  -- ,  Tuple "YatriCoins" $ yatriCoinsView push state
                  -- ,  Tuple "UseCoins" $ useCoinsView push state
                  -- ,  Tuple "FAQView" $ faqView push state
                  -- , Tuple "faqQuestionView" $ faqQuestionView push state]]
                ] <> (if subView == ST.EARNINGS_VIEW then [Tuple "Earnings" $ earningsView push state] else [])
                  <>  (if subView ==  ST.YATRI_COINS_VIEW then [Tuple "YatriCoins" $ yatriCoinsView push state] else [])
                  <>  (if subView ==  ST.USE_COINS_VIEW then [Tuple "UseCoins" $ useCoinsView push state] else [])
                  <>  (if subView == ST.FAQ_VIEW then [Tuple "FAQView" $ faqView push state] else [])
                  <>  (if subView == ST.FAQ_QUESTON_VIEW then [Tuple "faqQuestionView" $ faqQuestionView push state] else [])]

lottieView :: forall w . ST.DriverEarningsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
lottieView state push =
  let id' = (getNewIDWithTag "lottieView")
  in relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  , visibility if state.props.showCoinsRedeemedAnim /= "" then VISIBLE else GONE
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
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 24.0 
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , padding $ Padding 4 4 4 4
  , margin $ Margin 16 24 16 24
  , gravity CENTER
  , visibility if (state.props.subView /= ST.USE_COINS_VIEW) && (state.props.subView /= ST.FAQ_VIEW) && (state.props.subView /= ST.FAQ_QUESTON_VIEW)
                then VISIBLE 
                else GONE
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
      ] 
      [ textView
          [ height WRAP_CONTENT
          , text $ getString text'
          , onClick push $ const $ ChangeTab subView
          , fontStyle $ FontStyle.medium LanguageStyle
          , color if isActive then Color.white900 else Color.black700
          , padding $ PaddingBottom 3
          ]
      , imageView 
          [ imageWithFallback $ img <> ","
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
    ](if state.data.anyRidesAssignedEver 
      then if state.props.showShimmer 
           then [shimmerView push state]
           else [ totalEarningsView push state 
                , transactionViewForEarnings push state]
      else [noRideHistoryView push state])

shimmerView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
shimmerView push state = 
  shimmerFrameLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.blue600
    ] [ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][ linearLayout
    [ height (V 350)
    , width MATCH_PARENT
    , background Color.greyDark
    -- , padding $ Padding 20 20 20 16
    , cornerRadius 12.0
    , orientation VERTICAL
    ][]
    ,  linearLayout
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
        , orientation HORIZONTAL
        ][ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , weight 1.0
        , gravity LEFT
        ][ imageView $
          [ width (V 32)
          , height (V 32)
          , imageWithFallback $ if state.props.weekIndex == 0 then "ny_ic_chevron_left_light_grey" else "ny_ic_chevron_left" 
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
            , text $ convertUTCtoISC state.props.totalEarningsData.fromDate "DD MMM" <> (if state.props.totalEarningsData.toDate /= "" then "- " <> convertUTCtoISC state.props.totalEarningsData.toDate "DD MMM" else "") <> ", " <> getString EARNINGS
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
            [ width (V 32)
            , height (V 32)
            , imageWithFallback $ if state.props.weekIndex == 3 then "ny_ic_chevron_right_light_grey" else "ny_ic_chevron_right"
            , gravity RIGHT
            , onClick push $ const $ RightChevronClicked state.props.weekIndex
            , clickable if state.props.weekIndex < 3 then true else false
            ]
          ]
        ]
      , barGraphView push state
      , separatorView true
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , padding $ Padding 0 15 0 15
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
              , text $ (show (state.props.totalEarningsData.totalDistanceTravelled / 1000) <> " km")
              ] <> FontStyle.h2 TypoGraphy
          ]
        ] 
      , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER
        , margin $ MarginTop 6
        ]( map (\index -> dotView push index state) [0,1,2])
    ]

dotView :: forall w . (Action -> Effect Unit) -> Int -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
dotView push index state = 
  linearLayout
  [ height $ V 6
  , width $ V 6
  , cornerRadius 12.0
  , background $ case index of 
                  0 -> if state.props.weekIndex == 0 then Color.black800 else Color.grey900
                  2 -> if state.props.weekIndex == 3 then Color.black800 else Color.grey900
                  _ -> if state.props.weekIndex > 0 && state.props.weekIndex < 3 then Color.black800 else Color.grey900
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
      , insightView push state
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
              , color "#FCC32C"
            ] <> FontStyle.priceFont TypoGraphy
              , linearLayout 
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , background if state.data.coinsEarnedPreviousDay >= 0 then "#1A53BB6F" else "#1AE55454"
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
        , imageView -- TO BE REPLACED BY IMAGE
          [ imageWithFallback $ "ny_ic_coin_balance,"
          , height $ V 62
          , width $ V 94
          ]
        ]
      , separatorView true
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
            [ imageWithFallback $ "ny_ic_coin_expire,"
            , height $ V 14
            , width $ V 14
            , margin $ MarginRight 10
            ]
          , textView $ [
              text $ show state.data.expiringCoins <> " coins expiring in the next " <> show state.data.expiringDays <> " days. \n Use them before they expire"
            , color Color.black700    
            ] <> FontStyle.tags TypoGraphy       
        ]
    ]

insightView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
insightView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginVertical 20 20
  ][  textView $ [
        text $ getString INSIGHTS
      , color Color.black900
      ] <> FontStyle.h2 TypoGraphy
    , horizontalScrollView
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin $ Margin 0 12 0 12
      ][ linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation HORIZONTAL
          ](map(\item -> badgeView item) dummyBages)
      ]
    ]

badgeView :: forall w. {badgeImage :: String, primaryText :: String, subText :: String} -> PrestoDOM (Effect Unit) w
badgeView state =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.grey900
    , background Color.white900
    , cornerRadius 15.0
    , padding $ Padding 16 10 16 12
    , margin $ MarginRight 16
    ][ imageView
      [ width $ V 65
      , height $ V 65
      , imageWithFallback state.badgeImage
      ]
    , linearLayout
      [ width WRAP_CONTENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      ][ textView $
        [ text state.subText
        , color Color.black800
        ] <> FontStyle.captions TypoGraphy
      , textView $
        [ text state.primaryText
        , color Color.black900
        ] <> FontStyle.body1 TypoGraphy
      ]
    ]

dummyBages :: Array ST.Badge
dummyBages = [{badgeImage: "ny_ic_five_star_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_five_star_badge.png"
                     , primaryText: "5-Star Rides"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_safe_ride,https://assets.juspay.in/nammayatri/images/driver/ny_ic_safe_ride.png"
                     , primaryText: "Safe Rides"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_clean_auto_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_clean_auto_badge.png"
                     , primaryText: "Clean Auto"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_expert_driver,https://assets.juspay.in/nammayatri/images/driver/ny_ic_expert_driver.png"
                     , primaryText: "Expert Driving"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_navigator_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_navigator_badge.png"
                     , primaryText: "Navigator"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_ontime_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_ontime_badge.png"
                     , primaryText: "On Time"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_polite_driver_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_polite_driver_badge.png"
                     , primaryText: "Professional"
                     , subText: "235"
                      }
                      ]
  

dummyQuestions :: Array ST.FaqQuestions
dummyQuestions = [
                  { question: "What are Yatri Coins?"
                  , videoLink: "https://www.youtube.com/watch?v=1340l2e0ZKY"
                  , answer: [ "Yatri Coins are the rewards you earn on day-to-day activities on Namma Yatri like rides, referrals, good ratings, etc."
                            , "The earned coins can be used to avail benefits like purchasing subscription plans with coins only."
                            , "New benefits will be added in the future."
                            ]
                  },
                  { question: "What is the validity of Yatri Coins?"
                  , videoLink: ""
                  , answer: [ "Yatri Coins are valid for a period of 90 days from the day they are earned. During this period, you can use the coins for availing benefits."
                            ]
                  },
                  { question: "How can I earn Yatri Coins?"
                  , videoLink: "https://www.youtube.com/watch?v=1340l2e0ZKY"
                  , answer: [ "Coins are earned through day-to-day activities on the app like taking rides, referring customers. You don’t need to do anything different."
                            , "You can earn more coins by exhibiting good behavior like low cancellations, getting good ratings from customers, etc."
                            ]
                  },
                  { question: "How can I use the Yatri Coins I earned?"
                  , videoLink: "https://www.youtube.com/watch?v=1340l2e0ZKY"
                  , answer: [ "You can use the earned coins to purchase subscription plans free-of-cost within the validity period."
                            , "In the future, more options will be added for using Yatri Coins."
                            ]
                  },
                  { question: "Is setting up Autopay mandatory for using Yatri Coins?"
                  , videoLink: ""
                  , answer: [ "Yes. Setting up Autopay is mandatory for using Yatri Coins."
                            , "However, you can keep earning them even without setting up Autopay."
                            , " The Autopay setup is mandatory only for using the coins."
                            ]
                  },
                  { question: "What is the eligibility to earn and use Yatri Coins?"
                  , videoLink: ""
                  , answer: [ "You must complete your first ride with Namma Yatri to start earning Yatri Coins."
                            , "The Yatri Coins section will be disabled until you complete your first ride."
                            ]
                  },
                  { question: "Some Yatri Coins were deducted from my account without using them. Why?"
                  , videoLink: ""
                  , answer: [ "Coins may be deducted when there is undesirable behavior. Examples of undesirable behavior can be bad ratings from the customer, booking cancellation."
                            ]
                  }
                  ]
  

faqView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
faqView push state = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.blue600
    ][  textView $ [
        text "Learn about Yatri Coins"
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
    -- , background $ Color.red600
    ] (map (\item -> individualQuestionListView push item state) questionsList)

individualQuestionListView :: forall w . (Action -> Effect Unit) -> ST.FaqQuestions -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
individualQuestionListView push faqQuestion state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (PaddingHorizontal 16 16)
    , onClick push $ const $ FaqQuestionView faqQuestion
    ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER
          ][ linearLayout
              [ height MATCH_PARENT
              , weight 1.0
              , orientation HORIZONTAL
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
                , orientation HORIZONTAL
                , gravity CENTER
                  ][ imageView
                      [ imageWithFallback $ "ny_ic_chevron_right_grey," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevron_right_grey.png"
                      , height $ V 18
                      , width $ V 18
                      , margin $ Margin 10 0 8 0 
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

faqVid :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
faqVid push state = 
  if state.props.individualQuestion.videoLink /= ""
    then linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , background Color.red600 
          , gravity CENTER
          , margin $ MarginBottom 4
          , id (getNewIDWithTag "faqVideo")
          , afterRender
              ( \action -> do
                  let
                    id = (getNewIDWithTag "faqVideo")

                    _ = spy "zxc " "setYoutubePlayer" --state.props.individualQuestion.videoLink
                  
                  pure $ setYoutubePlayer (youtubeData state "VIDEO") id (show ST.PLAY)
                    
              )
              (const NoAction)
          ][]
      else linearLayout [][]

youtubeData :: ST.DriverEarningsScreenState -> String -> ST.YoutubeData
youtubeData state mediaType =
  { videoTitle: "title"
  , setVideoTitle: false
  , showMenuButton: false
  , showDuration: true
  , showSeekBar: true
  -- , videoId: "dNi__BckudQ"
  , videoId: getVideoID state.props.individualQuestion.videoLink
  , videoType: "VIDEO"
  }

answersListView :: forall w . (Action -> Effect Unit) -> Array String -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
answersListView push answersList state = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ] $ [faqVid push state] <> if length answersList > 1 
        then (map (\item -> 
          linearLayout 
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
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
                , text item
                , color Color.black900
                ] <> FontStyle.subHeading2 TypoGraphy)
            ]) answersList)
        else (map (\item -> 
          linearLayout 
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , padding (PaddingVertical 12 12)
            ][ textView (
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text item
                , color Color.black900
                -- , margin (Margin 0 0 0 9)
                ] <> FontStyle.subHeading2 TypoGraphy)
            ]) answersList)

horizontalLine :: forall w. PrestoDOM (Effect Unit) w 
horizontalLine = 
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    -- , margin $ Margin 12 8 12 8 
    ,gravity CENTER
    ][] 

helpButton :: forall w . (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
helpButton push = 
  linearLayout
    [ gravity RIGHT
    , weight 1.0
    , width WRAP_CONTENT
    ][  imageView $
        [ width (V 22)
        , height (V 22)
        , imageWithFallback $ "ic_help_circle_blue," <> (getCommonAssetStoreLink FunctionCall) <> "ic_help_circle_blue.png"
        , padding $ Padding 0 2 4 0 
        , gravity CENTER_VERTICAL
        , onClick push $ const $ ChangeTab ST.FAQ_VIEW
        ]
      , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text "Help"
        , color $ Color.blue900
        , onClick push $ const $ ChangeTab ST.FAQ_VIEW
        ] <> FontStyle.subHeading2 TypoGraphy
    ]

transactionView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
transactionView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
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
  let selectedDate = fromMaybe dummyDateItem state.props.calendarState.startDate
      dateToShow = if selectedDate.utcDate == "" || convertUTCtoISC selectedDate.utcDate "YYYY-MM-DD" == getcurrentdate "" then "Today" else convertUTCtoISC selectedDate.utcDate "Do MMM"
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
        imageWithFallback $ "ny_ic_calendar_unfilled_blue,"
      , height $ V 16
      , width $ V 16
      , margin $ MarginRight 4
      ]
      , textView $ [
        text dateToShow
      , color Color.black700
      ] <> FontStyle.tags TypoGraphy
      , imageView [
        imageWithFallback if state.props.calendarState.calendarPopup 
                          then "ny_ic_chevron_up,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_up.png"
                          else "ny_ic_chevron_down,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_down.png" -- clear the use of both icons 
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
        , background "#E1E7F5"
        , cornerRadii $ Corners 12.0 true true false false
        , gravity CENTER_VERTICAL
        ][ textView $ [
            text $ "₹" <> formatCurrencyWithCommas (show (getFixedTwoDecimals state.data.totalCoinConvertedToCash))
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
            text $ formatCurrencyWithCommas (show state.data.coinsEarnedToday)
          , color Color.black800     
          ] <> FontStyle.h2 TypoGraphy  
        , imageView
          [ imageWithFallback $ "ny_ic_yatri_coin,"
          , height $ V 16
          , width $ V 16
          , margin $ MarginLeft 4
          ]
        ]     
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          ] (DA.mapWithIndex (\index item -> historyViewItem item (index == length historyItems - 1) state.props.subView) historyItems)
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
          [ imageWithFallback $ "ny_ic_yatri_coin,"
          , height $ V 15
          , width $ V 15
          , margin (Margin 6 0 6 0)
          ]
      ]
    ]
    , separatorView (not isLast)
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
    -- , margin $ MarginTop 24
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
            [ imageWithFallback $ "ny_ic_yatri_coin,"
            , height $ V 16
            , width $ V 16
            , margin (Margin 4 0 0 0)
            ]     
          ]
      , if state.data.coinBalance < 250
          then alertView push "ny_ic_info_yellow" "Minimum 250 coins is required for conversion" false Color.yellowOpacity40 8.0 (Margin 16 20 16 0)
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
         [ imageWithFallback $ image <> ","
         , height $ V 14
         , width $ V 14
         , margin $ MarginRight 8
         ]  
       , textView $ [
          text message --"₹25 has been adjusted in your\nsubscription dues"
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
      _ = spy "bounds" marginLeft'
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
          text $ getString CONVERT_COINS
        , color Color.black800
        ] <> FontStyle.h2 TypoGraphy
      , imageView
        [ imageWithFallback $ "ny_ic_info_grey,"
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
    , alpha if state.data.coinBalance < 250 || not state.data.hasActivePlan then 0.6 else 1.0
    , clickable $ state.data.coinBalance >= 250 || state.data.hasActivePlan
    , padding $ Padding 16 16 16 16
    ][  frameLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          ][  linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER
              ] [ textView $ [
                    text "250"
                  , color Color.black700
                  ] <> FontStyle.body3 TypoGraphy
                , Anim.screenAnimationFadeInOut $ linearLayout
                  [ height WRAP_CONTENT
                  , width $ V 260
                  , id $ getNewIDWithTag "ConvertCoinsSliderView"
                  , onAnimationEnd (\action -> runEffectFn7 renderSlider (getNewIDWithTag "ConvertCoinsSliderView") push SliderCallback state.data.coinConversionRate 250 2500 100 
                      )(const AfterRender)
                  ][]
                , textView $ [
                    text "2500"
                  , color Color.black700
                  ] <> FontStyle.body3 TypoGraphy
              ]
            , linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , visibility if state.data.coinBalance < 250 || not state.data.hasActivePlan then VISIBLE else GONE
              , clickable true
              ][]
          ]
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig (state.data.coinBalance >= 250 && state.data.hasActivePlan))
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


separatorView :: forall w. Boolean -> PrestoDOM (Effect Unit) w 
separatorView isVisible = 
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.grey700
  , visibility if isVisible then VISIBLE else GONE
  ][]

coinsUsagePopup :: forall w. (Action -> Effect Unit) -> DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
coinsUsagePopup push state =
   PrestoAnim.animationSet [ Anim.fadeIn true ]
     $ linearLayout
         [ height MATCH_PARENT
         , width MATCH_PARENT
         ]
         [ RequestInfoCard.view (push <<< RequestInfoCardAction) (waitTimeInfoCardConfig FunctionCall) ]

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
        , background "#E1E7F5"
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
  , imageWithFallback $ "ny_ic_dotted_line,"
  , weight 1.0
  ]
  , textView $
      [ height $ WRAP_CONTENT
      , width $ WRAP_CONTENT
      , gravity RIGHT
      , text $ "₹" <> (formatCurrencyWithCommas (show earnings))
      ] <> FontStyle.paragraphText TypoGraphy
  ]

barGraphView :: forall w. (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w 
barGraphView push state = 
  let currWeekMaxEarning = if state.props.currentWeekMaxEarning > 0 then state.props.currentWeekMaxEarning else 1500
  in
  relativeLayout
  [ height $ V (150)
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
  [ height $ V (150)
  , width MATCH_PARENT
  , background Color.transparent
  , orientation HORIZONTAL
  , gravity BOTTOM
  ]  (DA.mapWithIndex(\ index item  -> (barView push index item state)) state.props.currWeekData)
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
      , margin $ MarginBottom 2
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
    , separatorView true
  ]

tagview :: String -> forall w. PrestoDOM (Effect Unit) w 
tagview name = imageView
          [ width (V 16)
          , height (V 16)
          , imageWithFallback $ name <> ","
          , margin $ Margin 4 4 4 3
          ]

noRideHistoryView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
noRideHistoryView push state = 
    linearLayout
    [ 
      height $ V ((screenHeight unit / 10 ) * 7)
    , width MATCH_PARENT
    , gravity CENTER
    , padding (PaddingBottom safeMarginBottom)
    ][  ErrorModal.view (push <<< ErrorModalActionController) (errorModalConfig state)]