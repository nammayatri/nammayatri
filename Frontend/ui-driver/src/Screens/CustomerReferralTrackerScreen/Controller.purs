{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CustomerReferralTrackerScreen.Controller where

import Prelude
import Components.Calendar.Controller as CalendarController
import Common.Types.App (LazyCheck(..), ModifiedCalendarObject)
import Components.PrimaryButton as PrimaryButtonController
import Components.ReferralStepsView as ReferralStepsView
import Components.PopUpModal as PopUpModal
import Data.Array as DA
import Data.Array (union, (!!), filter, length, (:), foldl, drop, take, replicate, updateAt, elemIndex, (..), last, find, catMaybes, sortBy, reverse)
import Data.Either
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import PrestoDOM.Types.Core (class Loggable)
import Data.Show (show)
import Data.String as DS
import Data.String (Pattern(..), split, take, drop)
import Engineering.Helpers.Commons (getCurrentUTC, getFutureDate, getDayName, convertUTCtoISC, getPastDays, getDayOfWeek)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils
import Foreign.Generic (decodeJSON)
import Helpers.Utils (incrementValueOfLocalStoreKey, getRideLabelData, parseFloat, getRequiredTag)
import JBridge (pauseYoutubeVideo, copyToClipboard, toast, toggleBtnLoader, showDialer, getCurrentDate)
import Language.Strings (getString)
import Language.Types
import Log
import PrestoDOM (Eval, update, continue, exit, ScrollState(..), updateAndExit, continueWithCmd, update)
import Screens (ScreenName(..), getScreen)
import Services.API as API
import Resource.Constants as Const
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Screens.CustomerReferralTrackerScreen.Types
import Screens.CustomerReferralTrackerScreen.Transformer (getDailyEarnings, getOrderStatus)
import Domain.Payments as PP

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> trackAppBackPress appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN)
    NoAction -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "no_action" "no_action"
    (RideSummaryAPIResponseAction _ _ _) -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "ride_summary_api_response_action" "ride_summary_api_response_action"
    AfterRender -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "after_render" "after_render"
    HeaderSuffixImgOnClick -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "generic_header_action" "suffix_image_click"
    (CalendarAC _) -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "calendar_action" "calendar_action"
    ShowCalendarPopup -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "show_calendar_popup" "show_calendar_popup"
    (ReferralStepsViewAction _) -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "referral_steps_view_action" "referral_steps_view_action"
    (BarViewSelected _) -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "bar_view_selected" "bar_view_selected"
    (LeftChevronClicked _) -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "left_chevron_clicked" "left_chevron_clicked"
    (RightChevronClicked _) -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "right_chevron_clicked" "right_chevron_clicked"
    MenuButtonClick -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "menu_button_click" "menu_button_click"
    ShowReferralSteps -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "show_referral_steps" "show_referral_steps"
    ShowUPIDetails -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "show_upi_details" "show_upi_details"
    ShowDeleteUPI -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "show_delete_upi" "show_delete_upi"
    EmptyUPIPrimaryAction act -> case act of 
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "add_upi_action" "on_click"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "add_upi_action" "no_action"
    Copy _ -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "transaction_details" "copy"
    SelectEarning _ -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "payout_history"  "select_earning"
    PopUpModalAction _ -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "request_info_card" "action"
    CheckOrderStatus -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "check_order_status" "action"
    RetryPayment -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "retry_payment" "action"
    AddUPIAction -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "add_upi" "action"
    DeleteUPIAction _ -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "delete_upi" "on_click"

data ScreenOutput
  = GoBack
  | AddUPI CustomerReferralTrackerScreenState
  | DeleteUPI CustomerReferralTrackerScreenState
  | RefreshOrderStatus CustomerReferralTrackerScreenState

data Action
  = NoAction
  | BackPressed
  | RideSummaryAPIResponseAction (API.ReferralEarningsResp) String (Array String)
  | AfterRender
  | HeaderSuffixImgOnClick
  | CalendarAC CalendarController.Action
  | ShowCalendarPopup
  | ReferralStepsViewAction ReferralStepsView.Action
  | BarViewSelected Int
  | LeftChevronClicked Int
  | RightChevronClicked Int
  | MenuButtonClick
  | ShowReferralSteps
  | ShowUPIDetails
  | ShowDeleteUPI
  | EmptyUPIPrimaryAction PrimaryButtonController.Action
  | Copy String
  | SelectEarning DailyEarning
  | PopUpModalAction PopUpModal.Action
  | CheckOrderStatus
  | RetryPayment
  | AddUPIAction
  | DeleteUPIAction PrimaryButtonController.Action

eval :: Action -> CustomerReferralTrackerScreenState -> Eval Action ScreenOutput CustomerReferralTrackerScreenState
eval BackPressed state = 
  case state.data.currentStage of 
    Tracker       -> if state.props.showMenu then 
                        continue state {props{showMenu = false}}
                      else exit $ GoBack
    ReferralSteps -> continue state{data{currentStage = Tracker}}
    UPIDetails    -> if state.props.showDeleteUPIView then 
                        continue state{props{showDeleteUPIView = false, showUPIOptions = false}}
                      else if state.props.showUPIOptions then 
                        continue state{props{showUPIOptions = false}}
                      else continue state{data{currentStage = Tracker}}
    TransactionHistory -> continue state{data{currentStage = Tracker}}

eval (Copy val) state = continueWithCmd state [ do 
    _ <- pure $ copyToClipboard val
    _ <- pure $ toast (getString COPIED)
    pure NoAction
  ]

eval CheckOrderStatus state = exit $ RefreshOrderStatus state

eval (ReferralStepsViewAction (ReferralStepsView.GoBack)) state = continueWithCmd state [ pure BackPressed ]

eval (EmptyUPIPrimaryAction (PrimaryButtonController.OnClick)) state = do 
  void $ pure $ toggleBtnLoader "AddUPIButton" true
  updateAndExit state $ AddUPI state

eval (DeleteUPIAction (PrimaryButtonController.OnClick)) state = do 
  void $ pure $ toggleBtnLoader "deleteUPIButton" true
  updateAndExit state $ DeleteUPI state

eval RetryPayment state = exit $ AddUPI state

eval AddUPIAction state = exit $ AddUPI state

eval ShowUPIDetails state = continue state {data{currentStage = UPIDetails}, props{showMenu = false}}

eval ShowDeleteUPI state = continue state {props{showDeleteUPIView = true}}

eval ShowReferralSteps state = continue state{data{currentStage = ReferralSteps, selectedItem = Nothing}, props{showMenu = false}}

eval (MenuButtonClick) state = continue state{props{showMenu = true}}

eval (SelectEarning earning) state = do
  let newState = state{data{selectedItem = Just earning}}
  case earning.status of 
    Success -> continue newState{data {currentStage = TransactionHistory}}
    Failed -> do 
      void $ pure $ showDialer state.data.config.subscriptionConfig.supportNumber false
      update state
    _ -> continue newState{data {currentStage = ReferralSteps}}

eval (HeaderSuffixImgOnClick) state = 
  case state.data.currentStage of 
    Tracker -> continue state {props{showMenu = not state.props.showMenu}}
    UPIDetails -> continue state {props{showUPIOptions = true}}
    _ -> continue state

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = do 
  let newState = state{props{showInfoPopUp = false}}
  if state.data.orderStatus == Just FAILED then
    continueWithCmd newState [ do 
      pure AddUPIAction
    ]
  else continue newState

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = continue state {props {showInfoPopUp = false}}

eval ShowCalendarPopup state = do
  let
    res = initializeCalendar true
  continue state { props { calendarState { weeks = res.weeks, calendarPopup = true, selectedTimeSpan = res.selectedTimeSpan, startDate = res.startDate, endDate = Nothing } } }

eval (CalendarAC CalendarController.HideCalendarPopup) state = continue state { props { calendarState { calendarPopup = false, startDate = Nothing, endDate = Nothing } } }

eval (CalendarAC (CalendarController.DecrementMonth res)) state = continue state { props { calendarState { weeks = res.weeks, selectedTimeSpan = res.selectedTimeSpan } } }

eval (CalendarAC (CalendarController.IncrementMonth res)) state = continue state { props { calendarState { weeks = res.weeks, selectedTimeSpan = res.selectedTimeSpan } } }

eval (CalendarAC (CalendarController.SelectDate res)) state = continue state { props { calendarState { startDate = res.startDate, weeks = res.weeks } } }

eval (CalendarAC (CalendarController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
  let startDate = case state.props.calendarState.startDate of
                    Nothing -> (getCurrentUTC "")
                    Just val -> val.utcDate
      newState = state { props { calendarState { calendarPopup = false }, date = startDate, showShimmer = true, callEarningsAPI = true } }
  continue newState

eval (CalendarAC (CalendarController.PrimaryButtonCancelActionController (PrimaryButtonController.OnClick))) state = do
  continue state { props { calendarState { calendarPopup = false } } }

eval (LeftChevronClicked currentIndex) state = do
  let
    updatedIndex = if currentIndex == 0 then currentIndex else currentIndex - 1

    currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek updatedIndex state.data.dailyEarningData)

    currPayoutHistory = filter(\item -> item.earnings > 0) currentWeekData

    currWeekMaxEarning = foldl getMax 0 currentWeekData
  continue state { data {currWeekData = currentWeekData, currPayoutHistory = currPayoutHistory, totalEarningsData = getTotalCurrentWeekData currentWeekData}, props { weekIndex = updatedIndex, selectedBarIndex = -1, currentWeekMaxEarning = currWeekMaxEarning, showShimmer = false } }

eval (RightChevronClicked currentIndex) state = do
  let
    updatedIndex = if currentIndex == 3 then currentIndex else currentIndex + 1

    currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek updatedIndex state.data.dailyEarningData)

    currPayoutHistory = filter(\item -> item.earnings > 0) currentWeekData

    currWeekMaxEarning = foldl getMax 0 currentWeekData
  continue state { data {currWeekData = currentWeekData, currPayoutHistory = currPayoutHistory, totalEarningsData = getTotalCurrentWeekData currentWeekData}, props { weekIndex = updatedIndex, selectedBarIndex = -1, currentWeekMaxEarning = currWeekMaxEarning } }

eval (BarViewSelected index) state = do
  let
    mbSelectedBarData = state.data.currWeekData !! index

    selectedBarData =
      { fromDate: maybe "" (\record -> record.earningDate) mbSelectedBarData
      , toDate: ""
      , totalEarnings: maybe 0 (\record -> record.earnings) mbSelectedBarData
      , totalActivations: maybe 0 (\record -> record.activatedItems) mbSelectedBarData
      , totalReferrals: maybe 0 (\record -> record.referrals) mbSelectedBarData
      }
  continue state { data {totalEarningsData = if state.props.selectedBarIndex == index then getTotalCurrentWeekData state.data.currWeekData else selectedBarData}, props { selectedBarIndex = if state.props.selectedBarIndex == index then -1 else index } }

eval (RideSummaryAPIResponseAction (API.ReferralEarningsResp referralEarningsResp) todaysDate datesList) state = do
  let
    earningList = getDailyEarnings referralEarningsResp.dailyEarnings

    earningListWithMissingDates = getEarningListWithMissingDates earningList datesList

    dayOfWeek = getDayOfWeek (getDayName todaysDate)

    noOfDaysToNearestSunday = 6 - dayOfWeek

    datesUptoMearestSunday = if noOfDaysToNearestSunday > 0 then map (\x -> getFutureDate todaysDate x) (1 .. noOfDaysToNearestSunday) else []

    allWeeksData = getAllWeeksData earningListWithMissingDates datesUptoMearestSunday

    currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek state.props.weekIndex allWeeksData)

    currPayoutHistory = filter(\item -> item.earnings > 0) currentWeekData

    currWeekMaxEarning = foldl getMax 0 currentWeekData

    orderStatus = getOrderStatus (fromMaybe PP.NEW referralEarningsResp.orderStatus) 
    
  continue state { 
                  data { dailyEarningData = allWeeksData, 
                         currPayoutHistory = currPayoutHistory, 
                         currWeekData = currentWeekData, 
                         totalEarningsData = getTotalCurrentWeekData currentWeekData,
                         orderId = referralEarningsResp.orderId,
                         orderStatus = if isJust referralEarningsResp.orderStatus then Just orderStatus else Nothing,
                         referralRewardAmountPerRide = referralEarningsResp.referralRewardAmountPerRide,
                         referralCount = referralEarningsResp.totalReferralCount,
                         upiID = referralEarningsResp.vpaId
                        }, 
                  props { currentWeekMaxEarning = currWeekMaxEarning, 
                          showShimmer = false, 
                          callEarningsAPI = false 
                        } 
                 }

eval NoAction state = update state

eval AfterRender state = update state

eval _ state = update state

getEarningListWithMissingDates :: Array DailyEarning -> Array String -> Array DailyEarning
getEarningListWithMissingDates earningList dates = do
  let
    x = DA.catMaybes $ map (\d -> (getEarningForDate earningList d)) dates
  sortBy (comparing _.earningDate) (earningList <> x)

getEarningForDate :: Array DailyEarning -> String -> Maybe DailyEarning
getEarningForDate earningList date = 
  let earningForDate = DA.find (\e -> e.earningDate == date) earningList
  in maybe (Just { activatedItems : 0,
                   earningDate : date,
                   earnings : 0,
                   payoutOrderId : Nothing,
                   payoutOrderStatus : Nothing,
                   referrals : 0,
                   status : Processing,
                   percentLength : 0.0
                 }
            ) (\_ -> Nothing) earningForDate

getAllWeeksData :: Array DailyEarning -> Array String -> Array DailyEarning
getAllWeeksData earningList dates = do
  let
    objList = map (\date -> {  activatedItems : 0,
                            earningDate : date,
                            earnings : 0,
                            payoutOrderId : Nothing,
                            payoutOrderStatus : Nothing,
                            referrals : 0,
                            status : Processing,
                            percentLength : 0.0
                          }) dates
  earningList <> objList

initializeCalendar :: Boolean -> ModifiedCalendarObject
initializeCalendar selectTodaysDate =
  let currentDay = getCurrentDay true
      weeks = getWeeksInMonth currentDay.year currentDay.intMonth
  in if selectTodaysDate 
       then selectSingleCalendarDate currentDay Nothing Nothing weeks
       else 
         { selectedTimeSpan : currentDay, 
           weeks : weeks, 
           startDate : Nothing,
           endDate : Nothing 
         }

getTotalCurrentWeekData :: Array DailyEarning -> TotalEarningsData
getTotalCurrentWeekData barGraphData = do
  let
    firstElement = barGraphData !! 0

    lastElement = barGraphData !! 6

    calculateTotals =
      foldl
        ( \acc record ->
            { totalEarnings: acc.totalEarnings + record.earnings
            , totalReferrals: acc.totalReferrals + record.referrals
            , totalActivations: acc.totalActivations + record.activatedItems
            }
        )
        { totalEarnings: 0, totalReferrals: 0, totalActivations: 0 }
        barGraphData
  { fromDate: maybe "" (\earning -> earning.earningDate) firstElement
  , toDate: maybe "" (\earning -> earning.earningDate) lastElement
  , totalEarnings: calculateTotals.totalEarnings
  , totalActivations: calculateTotals.totalActivations
  , totalReferrals: calculateTotals.totalReferrals
  }

getEarningDataOfWeek :: Int -> Array DailyEarning -> Array DailyEarning
getEarningDataOfWeek index dailyEarningData = case index of
  3 -> DA.drop 21 dailyEarningData
  2 -> DA.take 7 (DA.drop 14 dailyEarningData)
  1 -> DA.take 7 (DA.drop 7 dailyEarningData)
  0 -> DA.take 7 dailyEarningData
  _ -> []

getWeeklyEarningsPercentage :: Array DailyEarning -> Array DailyEarning
getWeeklyEarningsPercentage dailyEarningData = map (\earning -> earning { percentLength = ((toNumber earning.earnings) * 100.0) / (toNumber maxValue) }) dailyEarningData
  where
  maxValue = foldl getMax 0 dailyEarningData

getMax :: Int -> DailyEarning -> Int
getMax val earning = max earning.earnings val

getTransactionItems :: CustomerReferralTrackerScreenState -> Array {key :: String, value :: String}
getTransactionItems state = 
  case state.data.selectedItem of 
    Nothing -> []
    Just earning -> 
      [ {key: "Order ID", value: fromMaybe "" earning.payoutOrderId}
      , {key: "Paid by", value: "Namma Yatri"}
      ]

getDatesList :: String -> CustomerReferralTrackerScreenState -> Array String
getDatesList todaysDate state =
  map (\date -> convertUTCtoISC date.utcDate "YYYY-MM-DD") $ getListofPastDays
  where
    getListofPastDays = getPastDays getCurrentDate (22 + getDayOfWeek (getDayName todaysDate))
  
