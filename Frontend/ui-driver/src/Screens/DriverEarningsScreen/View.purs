{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverEarningsScreen.View
  where

import Common.Types.App
import Debug
import Screens.DriverEarningsScreen.ComponentConfig
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Animation (fadeIn, fadeInWithDelay, fadeOut)
import Animation as Anim
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Components.DatePickerModel as DatePickerModel
import Components.ErrorModal as ErrorModal
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.GenericHeader.View as GenericHeader
import Components.PaymentHistoryModel as PaymentHistoryModel
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (length, (..))
import Data.Array as DA
import Data.Function.Uncurried (runFn1)
import Data.Tuple as DT
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn2, runEffectFn3)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (flowRunner, getDateFromObj, getFormattedDate, getNewIDWithTag)
import Engineering.Helpers.Commons (safeMarginBottom, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getcurrentdate, getPastDays, convertUTCtoISC)
import JBridge (horizontalScrollToPos)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<$>), const, (==), (<<<), bind, pure, unit, discard, show, not, map, (&&), ($), (<$>), (<>), (<<<), (==), (/), (>), (-), (/=))
import Presto.Core.Types.Language.Flow (doAff)
import Services.API (GetRidesHistoryResp(..), Status(..))
import PrestoDOM (scrollView, frameLayout, layoutGravity, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, background, calendar, clickable, color, cornerRadius, fontSize, fontStyle, gravity, height, horizontalScrollView, id, imageView, imageWithFallback, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, relativeLayout, scrollBarX, scrollBarY, stroke, swipeRefreshLayout, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.List as PrestoList
import PrestoDOM.Types.Core (toPropValue)
import Resource.Constants (tripDatesCount)
import Screens as ScreenNames
import Screens.DriverEarningsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.Backend as Remote
import Services.API as API
import Storage (getValueToLocalStore)
import Styles.Colors as Color
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Types.App (defaultGlobalState)
import Helpers.Utils (getCommonAssetStoreLink)
import Components.PrimaryButton.View as PrimaryButton

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
              pure unit
              -- let date = if initialState.datePickerState.selectedItem.date == 0 then getcurrentdate "" else (convertUTCtoISC initialState.datePickerState.selectedItem.utcDate "YYYY-MM-DD" )
              -- if initialState.currentTab == "COMPLETED" then do
              --   (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "8" (show initialState.offsetValue) "false" "COMPLETED" date
              --   lift $ lift $ doAff do liftEffect $ push $ RideHistoryAPIResponseAction rideHistoryResponse.list
              --   else do
              --     (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "8" (show initialState.offsetValue) "false" "CANCELLED" date
              --     lift $ lift $ doAff do liftEffect $ push $ RideHistoryAPIResponseAction rideHistoryResponse.list
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
            ][ tabView push state
              , if state.props.subView == ST.USE_COINS_VIEW then GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state) else linearLayout[][]
              , scrollView [      -- linearlayout 
                  height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , background Color.blue600
                ][ 
                  case state.props.subView of
                    ST.EARNINGS_VIEW -> earningsView push state
                    ST.YATRI_COINS_VIEW -> yatriCoinsView push state
                    ST.USE_COINS_VIEW -> useCoinsView push state
                ]
            ]
          ]
        , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.DRIVER_EARNINGS_SCREEN)
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
  , visibility if state.props.subView /= ST.USE_COINS_VIEW then VISIBLE else GONE
  ][  tabItem push (state.props.subView == ST.EARNINGS_VIEW) EARNINGS ST.EARNINGS_VIEW
    , tabItem push (state.props.subView == ST.YATRI_COINS_VIEW) YATRI_COINS ST.YATRI_COINS_VIEW
  ]

tabItem :: forall w . (Action -> Effect Unit) -> Boolean -> STR -> ST.DriverEarningsSubView -> PrestoDOM (Effect Unit) w
tabItem push isActive text' subView = 
  textView
  [ height WRAP_CONTENT
  , weight 1.0 
  , background if isActive then Color.black900 else Color.white900
  , text $ getString text'
  , cornerRadius 24.0 
  , padding $ PaddingVertical 6 8
  , onClick push $ const $ ChangeTab subView
  , fontStyle $ FontStyle.medium LanguageStyle
  , gravity CENTER
  , color if isActive then Color.white900 else Color.black700
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
         text "Earnigns view"
        ]
    ]

yatriCoinsView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
yatriCoinsView push state = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
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
          ][ textView $ [
              text "Coin Balance"
            ] <> FontStyle.paragraphText TypoGraphy
            , textView $ [
                text "1200"
                -- textFromHtml "<span style="background-image: linear-gradient(to right, red, yellow, green); background-clip: text; color: transparent; -webkit-background-clip: text; -webkit-text-fill-color: transparent;"> inline gradient text
        -- </span>"
              , color "#FCC32C"
            ] <> FontStyle.priceFont TypoGraphy
          ]
        , linearLayout
          [ height MATCH_PARENT
          , width WRAP_CONTENT
          , background Color.white900
          , gravity BOTTOM
          , weight 1.0
          , padding $ PaddingBottom 10
          ][ linearLayout 
              [
                height WRAP_CONTENT
              , width WRAP_CONTENT
              , background "#1A53BB6F" -- DISCUSS FOR DECREASE
              , margin $ MarginLeft 4
              , cornerRadius 100.0 
              , gravity CENTER_VERTICAL
              , padding $ Padding 8 3 8 3
              ][
                  textView $ [
                    text "120"
                  , color Color.green900     
                  ] <> FontStyle.paragraphText TypoGraphy
                , textView $ [
                    text "↑"
                    , color Color.green900     
                    , margin $ MarginBottom 2
                  ] <> FontStyle.subHeading1 TypoGraphy      
              ]
          ]
        , linearLayout -- TO BE REPLACED BY IMAGE
          [ height $ V 62
          , width $ V 94
          , background Color.grey700 
          , cornerRadius 8.0
          , orientation VERTICAL
          ][]
        ]
      , separatorView
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][
          linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , weight 1.0
          , gravity CENTER_VERTICAL
          ] [ textView $ [
                text "Total Earned"
              , color Color.black700     
              , margin $ MarginRight 8
              ] <> FontStyle.body3 TypoGraphy      
            , textView $ [
                text "4730"
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
              , margin $ MarginRight 8
              ] <> FontStyle.body3 TypoGraphy      
            , textView $ [
                text "2010"
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
          , textView $ [
              text "2010"
            , color Color.black700     
            ] <> FontStyle.subHeading1 TypoGraphy  
              ]    
          , historyView push state
    ]

historyView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
historyView push state = 
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
        , background "#D8EDE7"
        , cornerRadii $ Corners 12.0 true true false false
        , visibility if state.props.subView == ST.USE_COINS_VIEW then GONE else VISIBLE
        ][ textView $ [
            text "Coins Earned"
          , weight 1.0
          , color Color.black700     
          , margin $ MarginRight 8
          ] <> FontStyle.paragraphText TypoGraphy      
        , textView $ [
            text "507"
          , color Color.black800     
          ] <> FontStyle.h2 TypoGraphy  
        ]     
        , scrollView [
            width MATCH_PARENT ,
            height WRAP_CONTENT
            ] [ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                ] (map(\item ->  historyViewItem item) state.data.coinHistoryItems)
          ]
    ]


historyViewItem :: forall w . ST.CoinHistoryItem -> PrestoDOM (Effect Unit) w
historyViewItem item = 
  let color' = if item.coins > 0 then Color.green900 else Color.red
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
          text item.event
        , color Color.black900    
        ] <> FontStyle.body3 TypoGraphy
      , textView $ [
          text item.timestamp
        , color Color.black700  
        , margin $ MarginTop 4
        ] <> FontStyle.captions TypoGraphy 
      ]
    , linearLayout
      [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
      ][ textView $ [
          text $ show item.coins
        , color color'
          ] <> FontStyle.body6 TypoGraphy
        , imageView
          [ imageWithFallback $ "ny_ic_yatri_coin," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_yatri_coin.png"
          , height $ V 12
          , width $ V 12
          , cornerRadius 2.5
          , margin (Margin 6 0 6 0)
          ]
      ]
    ]
    , separatorView
  ]

useCoinsView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w
useCoinsView push state = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    ] [  somethingExcitingView push state
        , purchaseView push state
        , usageHistoryView push state
    ]
  
somethingExcitingView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w  
somethingExcitingView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , margin $ MarginTop 24
    ] [ linearLayout [      -- to be replaced
          height $ V 126
        , width $ V 328
        , background Color.grey700 
        , cornerRadius 8.0
        , orientation VERTICAL
        ] [
          textView [
         text "Something exciting"
        ]
        ]
      ]

purchaseView :: forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w  
purchaseView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginTop 24
    , orientation VERTICAL
    ] [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , padding $ Padding 16 18 16 18
        , margin $ MarginTop 16
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
              text "2720"
            , color "#FCC32C"
            , margin $ MarginRight 8
            ] <> FontStyle.h2 TypoGraphy       
          ]
      , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , stroke $ "1," <> Color.grey900
        , margin $ MarginTop 12
        , cornerRadius 8.0
        ](DA.mapWithIndex(\index item -> planTileView push item index state.props.selectedPlanIndex state.props.selectedPlanQuantity (index == length state.data.planItems -1)) state.data.planItems)
      ]  
      

usageHistoryView ::  forall w . (Action -> Effect Unit) -> ST.DriverEarningsScreenState -> PrestoDOM (Effect Unit) w  
usageHistoryView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginVertical 20 20
  ][  textView $ [
        text "Usage History"
      , color Color.black800
      , margin $ MarginBottom 12
      ] <> FontStyle.h2 TypoGraphy
    , historyView push state
    ]

separatorView :: forall w. PrestoDOM (Effect Unit) w 
separatorView = 
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.grey700
  , margin $ MarginVertical 12 12
  ][]

planTileView :: forall w. (Action -> Effect Unit) -> ST.CoinPlanItem -> Int -> Int -> Int-> Boolean -> PrestoDOM (Effect Unit) w 
planTileView push plan index activeIndex count isLast = 
  linearLayout 
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 16 16 16 16
  , background Color.white900
  , cornerRadius 8.0
  , onClick push $ const $ SelectPlan index
  ] [ linearLayout 
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ linearLayout 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , weight 1.0
        ] [ 
            radioView (index == activeIndex)
          , linearLayout 
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            ] [ textView $ [
                text plan.name
              , color Color.black700 
              , margin $ MarginRight 8
              ] <> FontStyle.paragraphText TypoGraphy    
              , textView $ [
                text $ show plan.coins
              , color Color.black700 
              , margin $ MarginRight 8
              ] <> FontStyle.paragraphText TypoGraphy    
            ]
          ]
        , linearLayout 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , cornerRadius 6.0
          , visibility if (index == activeIndex) then VISIBLE else GONE
          , stroke $ "1," <> Color.grey900
          ] [ textView $ [
                text $ getString ADD
              , visibility if count == 0 then VISIBLE else GONE
              , color Color.blue800
              , width $ V 72
              , padding $ Padding 7 12 7 12
              , onClick push $ const $ PlanCount true
              , gravity CENTER
              ] <> FontStyle.h2 TypoGraphy
          , linearLayout[
              height WRAP_CONTENT
            , width WRAP_CONTENT
            , visibility if count == 0 then GONE else VISIBLE
            , gravity CENTER_VERTICAL
            ][ textView $ [
                text "-"
              , height $ V 48
              , width $ V 24
              , gravity CENTER          
              , padding $ PaddingLeft 7
              , onClick push $ const  $ PlanCount false
              ]<> FontStyle.h2 TypoGraphy
            , textView $ [
                text $ show count
              , height $ V 48
              , width $ V 24
              , gravity CENTER
              ]<> FontStyle.h2 TypoGraphy
            , textView $ [
                text "+"
              , height $ V 48
              , width $ V 24
              , padding $ PaddingRight 7
              , gravity CENTER
              , onClick push $ const $ PlanCount true
              ]<> FontStyle.h2 TypoGraphy
            ]
          ]
        ]
    , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig (count > 0) (index == activeIndex))
    , if not isLast then separatorView else linearLayout[][]
    ]

radioView :: forall w. Boolean -> PrestoDOM (Effect Unit) w 
radioView isActive = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity RIGHT
  , margin (MarginRight 20)
  ][frameLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      ][ imageView
          [ height (V 24)
          , width (V 24)
          , imageWithFallback $ "ny_ic_radio_selected," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_radio_selected.png"
          , visibility if isActive then VISIBLE else GONE
          ]
        , imageView
          [ width (V 24)
          , height (V 24)
          , imageWithFallback $ "ny_ic_radio_unselected," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_radio_unselected.png"
          , visibility if isActive then GONE else VISIBLE
          ]
      ]
  ]