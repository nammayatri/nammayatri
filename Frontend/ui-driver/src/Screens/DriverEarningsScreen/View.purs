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

import Animation (fadeIn, fadeInWithDelay, fadeOut)
import Animation as Anim
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Components.BottomNavBar.Controller (navData)
import Components.Calendar.View as Calendar
import Components.DatePickerModel as DatePickerModel
import Components.ErrorModal as ErrorModal
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.GenericHeader.View as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.View as PrimaryButton
import Components.RequestInfoCard as RequestInfoCard
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (length, (..), null)
import Data.Array as DA
import Data.Function.Uncurried (runFn1)
import Data.Maybe as Mb
import Data.Tuple as DT
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn7)
import Engineering.Helpers.Commons (convertUTCtoISC, countDown, flowRunner, getCurrentUTC, getNewIDWithTag)
import Engineering.Helpers.Utils (loaderText, toggleLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (convertUTCtoISC, getFixedTwoDecimals, renderSlider, getcurrentdate)
import JBridge (horizontalScrollToPos, startLottieProcess, lottieAnimationConfig, getLayoutBounds)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<$>), (||), const, (==), (<<<), void, bind, pure, unit, discard, show, not, map, (&&), ($), (<$>), (<>), (<<<), (==), (/), (>), (-), (/=), (<), (*), (>=))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, calendar, clickable, color, cornerRadius, fontSize, fontStyle, frameLayout, gravity, height, horizontalScrollView, id, imageView, imageWithFallback, linearLayout, lottieAnimationView, margin, onAnimationEnd, onBackPressed, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, relativeLayout, scrollBarX, scrollBarY, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens as ScreenNames
import Screens.DriverEarningsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.DriverEarningsScreen.ScreenData (dummyDateItem)
import Screens.Types (DriverEarningsScreenState)
import Screens.Types as ST
import Services.API as API
import Services.Backend as Remote
import Storage (KeyStore(..), setValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)

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
              _ <- pure $ setValueToLocalStore VISITED_DRIVER_COINS_PAGE "true"
              case initialState.props.subView of
                ST.EARNINGS_VIEW -> pure unit
                ST.YATRI_COINS_VIEW -> do
                  let date = case initialState.props.calendarState.startDate of
                                Mb.Just val -> val.utcDate
                                Mb.Nothing -> getCurrentUTC ""
                  coinTransactionRes <- Remote.getCoinTransactionsReqBT date
                  lift $ lift $ doAff do liftEffect $ push $ CoinTransactionResponseAction coinTransactionRes
                  pure unit
                ST.USE_COINS_VIEW -> do
                  coinUsageHistoryRes <- Remote.getCoinUsageHistoryReqBT ""
                  lift $ lift $ doAff do liftEffect $ push $ CoinUsageResponseAction coinUsageHistoryRes
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
            , if state.props.subView == ST.USE_COINS_VIEW || not state.data.config.coinsConfig.enableYatriCoins then GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state) else linearLayout[][]
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

lottieView :: forall w . ST.DriverEarningsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
lottieView state push = 
  let _ = spy "lottieview" state
  in
  if state.props.showCoinsRedeemedAnim /= "" then 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  , visibility if state.props.showCoinsRedeemedAnim /= "" then VISIBLE else GONE
  , gravity CENTER
  ][
    Anim.screenAnimationFadeInOut
        $ lottieAnimationView
    [ id (getNewIDWithTag "DriverEarningsAnimation")
    , afterRender (\action-> do
                  liftEffect $ countDown state.data.timer "lottieTimer" push CountDown
                  void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = state.props.showCoinsRedeemedAnim, lottieId = (getNewIDWithTag "DriverEarningsAnimation"), scaleType = "FIT_CENTER"}
                  )(const NoAction)
    , height MATCH_PARENT
    , width MATCH_PARENT
    ]
  ]
  else dummyView

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
  , visibility if state.data.config.coinsConfig.enableYatriCoins && state.props.subView /= ST.USE_COINS_VIEW then VISIBLE else GONE
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
    -- , background Color.white900
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    ] [ textView [
         text "Earnings view"
        ]
    ]

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
              text "Coin Balance"
            ] <> FontStyle.paragraphText TypoGraphy
            , linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity BOTTOM
              ][ textView $ [
                text $ show state.data.coinBalance
                -- textFromHtml "<span style="background-image: linear-gradient(to right, red, yellow, green); background-clip: text; color: transparent; -webkit-background-clip: text; -webkit-text-fill-color: transparent;"> inline gradient text
        -- </span>"
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
                    text $ show state.data.coinsEarnedPreviousDay
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
                text "Total Earned"
              , color Color.black700     
              , margin $ Margin 0 1 8 0
              ] <> FontStyle.body3 TypoGraphy      
            , textView $ [
                text $ show state.data.coinsEarned
              , color Color.black700     
              ] <> FontStyle.subHeading1 TypoGraphy    
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER_VERTICAL
          ] [ textView $ [
                text "Coins Used"
              , color Color.black700     
              , margin $ Margin 0 1 8 0
              ] <> FontStyle.body3 TypoGraphy      
            , textView $ [
                text $ show state.data.coinsUsed
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
              text "Use Coins  →"
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
        text "Insights"
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
  
transactionView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
transactionView push state = 
  let selectedDate = Mb.fromMaybe dummyDateItem state.props.calendarState.startDate
      dateToShow = if selectedDate.utcDate == "" then "Today" else convertUTCtoISC selectedDate.utcDate "Do MMM"
  in
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
              text "Transaction History"
            , weight 1.0
            , color Color.black800     
            ] <> FontStyle.h2 TypoGraphy      
            , linearLayout
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
            ]    
          , historyView push state
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
            text $ "₹" <> getFixedTwoDecimals state.data.totalCoinConvertedToCash
          , color Color.black800     
          , visibility if state.props.subView == ST.USE_COINS_VIEW then VISIBLE else GONE
          , margin $ MarginRight 8
          ] <> FontStyle.h2 TypoGraphy
        , textView $ [
            text case state.props.subView of
                    ST.YATRI_COINS_VIEW -> "Coins Earned"
                    ST.USE_COINS_VIEW -> "cash converted"
                    _ -> ""
          , weight 1.0
          , color Color.black700     
          , margin $ MarginRight 8
          ] <> FontStyle.paragraphText TypoGraphy      
        , textView $ [
            text $ show state.data.coinsEarnedToday
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
                  ST.YATRI_COINS_VIEW, true  -> "+" <> show item.coins
                  ST.YATRI_COINS_VIEW, false -> show item.coins
                  _, _       ->  "-" <> show item.coins
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
              text $ "₹" <>  show item.cash <> " converted from coins."
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
              text "Coin Balance"
            , color Color.black700  
            , margin $ MarginRight 8
            , weight 1.0
            ] <> FontStyle.paragraphText TypoGraphy       
          , textView $ [
              text $ show state.data.coinBalance
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
          then alertView push "ny_ic_info_yellow" "Using Coins requires an active plan. Choose a plan to get started!" false Color.yellowOpacity40 8.0 (Margin 16 20 16 0)
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
        Mb.Just val -> alertView push "ny_ic_check_green" ("₹" <> show val <> " has been adjusted in your\nsubscription dues") true Color.transparent 0.0 (Margin 0 0 0 0) 
        Mb.Nothing -> dummyView
   , if state.data.coinConvertedTocashLeft /= 0.0 
        then alertView push "ny_ic_info_yellow" ("₹" <> show state.data.coinConvertedTocashLeft <> " will be adjusted in your future subscription dues") false Color.transparent 0.0 (Margin 0 0 0 0)
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
          text "Convert Coins"
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
        text "Usage History"
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
        text case subView of
               ST.YATRI_COINS_VIEW -> "No Coins Earned"
               _ -> "No Coins Used"
      , color Color.black900
      ] <> FontStyle.subHeading1 TypoGraphy
    , textView $ [
        text case subView of
               ST.YATRI_COINS_VIEW -> "Earn coins by taking rides and referring the app to others"
               _ -> "Use them before they expire"
      , color Color.black700
      ] <> FontStyle.paragraphText TypoGraphy
    ]

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