{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverEarningsScreen.Controller where

import Debug
import Effect.Unsafe
import Engineering.Helpers.LogEvent
import Prelude

import Components.BottomNavBar.Controller (Action(..)) as BottomNavBar
import Components.Calendar.Controller as CalendarController
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryButton as PrimaryButtonController
import Components.RequestInfoCard as RequestInfoCard
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (fromString) as NUM
import Data.Show (show)
import Engineering.Helpers.Commons (clearTimer, getCurrentUTC, getNewIDWithTag)
import Engineering.Helpers.Utils (initializeCalendar)
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppScreenRender)
import PrestoDOM (Eval, continue, exit, ScrollState(..), updateAndExit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Screens (ScreenName(..), getScreen)
import Screens.DriverEarningsScreen.Transformer (getEventName)
import Screens.Types (DriverEarningsScreenState, DriverEarningsSubView(..), AnimationState(..), ItemState(..), IndividualRideCardState(..), DisabilityType(..))
import Screens.Types as ST
import Services.API (Status(..), CoinTransactionRes(..), CoinTransactionHistoryItem(..), CoinsUsageRes(..), CoinUsageHistoryItem(..))
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> trackAppScreenRender appId "screen" (getScreen DRIVER_EARNINGS_SCREEN)

data ScreenOutput = GoBack
                  | HomeScreen DriverEarningsScreenState
                  | Alerts DriverEarningsScreenState
                  | Contest DriverEarningsScreenState
                  | SubscriptionScreen DriverEarningsScreenState
                  | ChangeDriverEarningsTab DriverEarningsSubView
                  | PurchasePlan DriverEarningsScreenState
                    

data Action = Dummy
            | ChangeTab DriverEarningsSubView
            | BackPressed
            | PrimaryButtonActionController PrimaryButtonController.Action
            | PlanCount Boolean
            | BottomNavBarAction BottomNavBar.Action
            | CoinTransactionResponseAction CoinTransactionRes
            | CoinUsageResponseAction CoinsUsageRes
            | CountDown Int String String String
            | NoAction
            | AfterRender
            | SelectPlan Int
            | GenericHeaderAC GenericHeader.Action
            | PopUpModalAC PopUpModal.Action
            | CalendarAC CalendarController.Action
            | ShowCalendarPopup
            | RenderSlider
            | SliderCallback Int
            | RequestInfoCardAction RequestInfoCard.Action
            | ShowCoinsUsagePopup

eval :: Action -> DriverEarningsScreenState -> Eval Action ScreenOutput DriverEarningsScreenState

eval BackPressed state = if state.props.subView == USE_COINS_VIEW 
                            then exit $ ChangeDriverEarningsTab YATRI_COINS_VIEW
                            else exit GoBack

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = exit $ PurchasePlan state

eval (ChangeTab subView') state = exit $ ChangeDriverEarningsTab subView'

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  case screen of
    "Home" -> exit $ HomeScreen state
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      exit $ Alerts state
    "Rankings" -> do
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ Contest state
    "Join" -> do
      exit $ SubscriptionScreen state
    _ -> continue state

eval (CoinTransactionResponseAction (CoinTransactionRes resp)) state = do
  let events = map (\(CoinTransactionHistoryItem item) -> {event : getEventName item.fn, timestamp : item.createdAt, coins : item.coins, cash : 0.0}) resp.coinTransactionHistory
  continue state {data {expiringDays = resp.expiringDays, expiringCoins = resp.expiringCoins, coinsEarned = resp.coinEarned, coinsUsed = resp.coinUsed, coinBalance = resp.coinBalance, coinsEarnedPreviousDay = resp.coinsEarnedPreviousDay, coinsEarnedToday = resp.todayCoinSummary, coinHistoryItems = events}}

eval (CoinUsageResponseAction (CoinsUsageRes resp)) state = do
  let usageHistory = map (\(CoinUsageHistoryItem item) -> {event : item.title, timestamp : item.createdAt, coins : item.numCoins, cash : item.cash}) resp.coinUsageHistory
  continue state {data { coinBalance = resp.coinBalance,
                         usageHistoryItems = usageHistory,
                         totalCoinConvertedToCash = resp.totalCoinConvertedToCash, 
                         coinConvertedToCashUsedForLatestDues = resp.coinConvertedToCashUsedForLatestDues, 
                         coinConvertedTocashLeft = resp.coinConvertedTocashLeft, 
                         coinConversionRate = resp.coinConversionRate}, 
                  props {popupType = if resp.coinBalance == 0 && state.props.popupType == ST.NO_POPUP 
                                        then ST.NO_COINS_POPUP else state.props.popupType}}

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [pure BackPressed]

eval (PopUpModalAC PopUpModal.OnButton1Click) state = do
  case state.props.popupType of
    ST.PLAN_PAID_POPUP -> exit $ SubscriptionScreen state
    ST.SETUP_AUTOPAY_POPUP -> exit $ SubscriptionScreen state
    ST.NO_COINS_POPUP -> continueWithCmd state [pure $ PopUpModalAC PopUpModal.DismissPopup]
    ST.COINS_EXPIRING_POPUP -> continue state
    _ -> continue state

eval (PopUpModalAC PopUpModal.OnButton2Click) state = do
  case state.props.popupType of
    ST.PLAN_PAID_POPUP -> continue state
    ST.SETUP_AUTOPAY_POPUP -> continueWithCmd state [pure $ PopUpModalAC PopUpModal.DismissPopup]
    ST.COINS_EXPIRING_POPUP -> continue state
    _ -> continue state

eval (PopUpModalAC PopUpModal.DismissPopup) state = do
   continue state {props{ popupType = ST.NO_POPUP}}

eval (CountDown seconds id status timerID) state = do
        if status == "EXPIRED" then do
            _ <- pure $ clearTimer state.data.timerID
            continue state{data{timerID = ""}, props{showCoinsRedeemedAnim = "", popupType = ST.PLAN_PAID_POPUP}}
        else
            continue $ state{data{timer = seconds, timerID=timerID}}

eval ShowCalendarPopup state = do
  let res = initializeCalendar true
  continue state { props{ calendarState{ weeks = res.weeks, calendarPopup = true, selectedTimeSpan = res.selectedTimeSpan, startDate = res.startDate, endDate = Nothing }}}

eval (CalendarAC CalendarController.HideCalendarPopup) state = continue state { props { calendarState { calendarPopup = false, startDate = Nothing, endDate = Nothing}}}

eval (CalendarAC (CalendarController.DecrementMonth res)) state = do
  continue state {props {calendarState{ weeks = res.weeks, selectedTimeSpan = res.selectedTimeSpan}}}

eval (CalendarAC (CalendarController.IncrementMonth res)) state = do
  continue state {props {calendarState{ weeks = res.weeks, selectedTimeSpan = res.selectedTimeSpan}}}

eval (CalendarAC (CalendarController.SelectDate res)) state = do
  -- let errorResponse = getErrorResponse res.startDate res.endDate
  let _ = spy " zxc date" res.startDate 
  continue state {props {calendarState{ startDate = res.startDate, weeks = res.weeks}}}--, showError = errorResponse.showError, errorMessage = errorResponse.errorMessage }}

eval (CalendarAC (CalendarController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
  continue state { props {calendarState{ calendarPopup = false}}}

eval (CalendarAC (CalendarController.PrimaryButtonCancelActionController (PrimaryButtonController.OnClick))) state = do
  continue state { props {calendarState{ calendarPopup = false}}}

eval (SliderCallback value) state = do
  _ <- pure $ spy "slider value" value
  continue state {data {coinsToUse = value}}

eval ShowCoinsUsagePopup state = do
  continue state {props {showCoinsUsagePopup = true}}

eval (RequestInfoCardAction RequestInfoCard.Close) state = continue state {props {showCoinsUsagePopup = false}}

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = continue state {props {showCoinsUsagePopup = false}}

eval _ state = continue state
