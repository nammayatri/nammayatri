{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CustomerReferralTrackerScreen.Controller where

import Effect.Unsafe
import Engineering.Helpers.LogEvent
import Prelude
import Components.BottomNavBar.Controller (Action(..)) as BottomNavBar
import Components.Calendar.Controller as CalendarController
import Components.ErrorModal as ErrorModalController
import Components.GenericHeader as GenericHeader
import Common.Types.App (LazyCheck(..), ModifiedCalendarObject)
import Components.PrimaryButton as PrimaryButtonController
import Components.ReferralStepsView as ReferralStepsView
import Components.PopUpModal as PopUpModal
import Control.Monad.Except (runExcept)
import Data.Array as DA
import Data.Array (union, (!!), filter, length, (:), foldl, drop, take, replicate, updateAt, elemIndex, (..), last, find, catMaybes, sortBy, reverse)
import Data.Either
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (fromString) as NUM
import Data.Show (show)
import Data.String as DS
import Data.String (Pattern(..), split, take, drop)
import Engineering.Helpers.Commons (getCurrentUTC, getFutureDate, getDayName, convertUTCtoISC, getPastDays)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils
import Foreign.Generic (decodeJSON)
import Helpers.Utils (checkSpecialPickupZone, isYesterday, getcurrentdate, getDayOfWeek, incrementValueOfLocalStoreKey, getRideLabelData, parseFloat, getRequiredTag)
import JBridge (pauseYoutubeVideo, copyToClipboard, toast)
import Language.Strings (getString)
import Language.Types
import Log
import PrestoDOM (Eval, update, continue, exit, ScrollState(..), updateAndExit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Resource.Constants (decodeAddress, rideTypeConstructor)
import Screens (ScreenName(..), getScreen)
import Screens.Types as ST
import Services.API (RidesSummary(..))
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore, getValueToLocalStore)
import Timers (clearTimerWithId)
import Debug
import Foreign (unsafeToForeign)
import Resource.Constants as Const
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Screens.CustomerReferralTrackerScreen.ScreenData

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> trackAppBackPress appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN)
    NoAction -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "no_action" "no_action"
    (RideSummaryAPIResponseAction _ _ _) -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "ride_summary_api_response_action" "ride_summary_api_response_action"
    AfterRender -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "after_render" "after_render"
    (GenericHeaderAC _) -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "generic_header_action" "generic_header_action"
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
    AddUPIAction act -> case act of 
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "add_upi_action" "on_click"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "add_upi_action" "no_action"
    Copy _ -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "transaction_details" "copy"
    SelectEarning _ -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "payout_history"  "select_earning"
    PopUpModalAction _ -> trackAppActionClick appId (getScreen CUSTOMER_REFERRAL_TRACKER_SCREEN) "request_info_card" "action"

data ScreenOutput
  = GoBack
  | RefreshScreen CustomerReferralTrackerScreenState
  | AddUPI CustomerReferralTrackerScreenState

data Action
  = NoAction
  | BackPressed
  | RideSummaryAPIResponseAction (Array RidesSummary) String (Array String)
  | AfterRender
  | GenericHeaderAC GenericHeader.Action
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
  | AddUPIAction PrimaryButtonController.Action
  | Copy String
  | SelectEarning DailyEarning
  | PopUpModalAction PopUpModal.Action

eval :: Action -> CustomerReferralTrackerScreenState -> Eval Action ScreenOutput CustomerReferralTrackerScreenState
eval BackPressed state = 
  case state.data.currentStage of 
    Tracker       -> if state.props.showMenu then 
                        continue state {props{showMenu = false}}
                      else exit $ GoBack
    ReferralSteps -> continue state{data{currentStage = Tracker}}
    UPIDetails    -> if state.props.showDeleteUPIView then 
                        continue state{props{showDeleteUPIView = false}}
                      else if state.props.showUPIOptions then 
                        continue state{props{showUPIOptions = false}}
                      else continue state{data{currentStage = Tracker}}
    TransactionHistory -> continue state{data{currentStage = Tracker}}

eval (Copy val) state = continueWithCmd state [ do 
    _ <- pure $ copyToClipboard val
    _ <- pure $ toast (getString COPIED)
    pure NoAction
  ]

eval (ReferralStepsViewAction (ReferralStepsView.GoBack)) state = continueWithCmd state [ pure BackPressed ]

eval (AddUPIAction (PrimaryButtonController.OnClick)) state = exit $ AddUPI state

eval ShowUPIDetails state = continue state {data{currentStage = UPIDetails}, props{showMenu = false}}

eval ShowDeleteUPI state = continue state {props{showDeleteUPIView = true}}

eval ShowReferralSteps state = continue state{data{currentStage = ReferralSteps, selectedItem = Nothing}, props{showMenu = false}}

eval (MenuButtonClick) state = continue state{props{showMenu = true}}

eval (SelectEarning earning) state = do
  let newState = state{data{selectedItem = Just earning}}
  case earning.status of 
    Success -> continue newState{data {currentStage = TransactionHistory}}
    Failed -> continue newState
    _ -> continue newState{data {currentStage = ReferralSteps}}

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (GenericHeaderAC (GenericHeader.SuffixImgOnClick)) state = 
  case state.data.currentStage of 
    Tracker -> continue state {props{showMenu = not state.props.showMenu}}
    UPIDetails -> continue state {props{showUPIOptions = true}}
    _ -> continue state

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
      newState = state { props { calendarState { calendarPopup = false }, date = startDate, showShimmer = true} }
  updateAndExit newState $ RefreshScreen newState

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

eval (RideSummaryAPIResponseAction ridesSummaryList todaysDate datesList) state = do
  let
    earningList = mapSummaryListWithWeeklyEarnings ridesSummaryList

    earningListWithMissingDates = getEarningListWithMissingDates earningList datesList

    dayOfWeek = getDayOfWeek (getDayName todaysDate)

    noOfDaysToNearestSunday = 6 - dayOfWeek

    datesUptoMearestSunday = if noOfDaysToNearestSunday > 0 then map (\x -> getFutureDate todaysDate x) (1 .. noOfDaysToNearestSunday) else []

    allWeeksData = getAllWeeksData earningListWithMissingDates datesUptoMearestSunday

    currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek state.props.weekIndex allWeeksData)

    currPayoutHistory = filter(\item -> item.earnings > 0) currentWeekData

    currWeekMaxEarning = foldl getMax 0 currentWeekData
    
  continue state { data { dailyEarningData = allWeeksData, currPayoutHistory = currPayoutHistory, currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData }, props { currentWeekMaxEarning = currWeekMaxEarning, showShimmer = false, callEarningsAPI = false } }

eval NoAction state = continue state

eval AfterRender state = continue state

eval _ state = continue state

mapSummaryListWithWeeklyEarnings :: Array RidesSummary -> Array DailyEarning
mapSummaryListWithWeeklyEarnings ridesSummaryList =
  map
    ( \(RidesSummary rideSummary) ->
        { earnings: rideSummary.earnings
        , referrals: rideSummary.rideDistance
        , earningDate: rideSummary.rideDate
        , activatedItems: rideSummary.noOfRides
        , percentLength: 0.0
        , status : Processing
        , id : "Dummy ID"
        }
    )
    ridesSummaryList

getEarningListWithMissingDates :: Array DailyEarning -> Array String -> Array DailyEarning
getEarningListWithMissingDates earningList dates = do
  let
    x = DA.catMaybes $ map (\d -> (getEarningForDate earningList d)) dates
  sortBy (comparing _.earningDate) (earningList <> x)

getEarningForDate :: Array DailyEarning -> String -> Maybe DailyEarning
getEarningForDate earningList date =
  let
    foundDate = DA.find (\e -> e.earningDate == date) earningList
  in
    maybe (Just { id : "Dummy", earnings: 0, referrals: 0, earningDate: date, activatedItems: 0, percentLength: 0.0, status: Verifying }) (\_ -> Nothing) foundDate

getAllWeeksData :: Array DailyEarning -> Array String -> Array DailyEarning
getAllWeeksData earningList dates = do
  let
    objList = map (\x -> { id : "Dummy", earnings: 0, referrals: 0, earningDate: x, activatedItems: 0, percentLength: 0.0, status: Verifying }) dates
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
  { fromDate: maybe "" (\x -> x.earningDate) firstElement
  , toDate: maybe "" (\x -> x.earningDate) lastElement
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
getWeeklyEarningsPercentage dailyEarningData = map (\x -> x { percentLength = ((toNumber x.earnings) * 100.0) / (toNumber maxValue) }) dailyEarningData
  where
  maxValue = foldl getMax 0 dailyEarningData

getMax :: Int -> DailyEarning -> Int
getMax num1 obj1 = max obj1.earnings num1

updateToState :: CustomerReferralTrackerScreenState -> CustomerReferralTrackerScreenState
updateToState state = do
  let
    currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek 3 state.data.dailyEarningData)

    currWeekMaxEarning = foldl getMax 0 currentWeekData

    date = getcurrentdate ""

    dayName = getDayName date

    noOfDaysToNearestSunday =
      6
        - case DA.elemIndex (DS.take 3 dayName) state.props.weekDay of
            Just index -> index
            Nothing -> 0
  state { data{currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData}, props { weekIndex = 3, selectedBarIndex = -1, currentWeekMaxEarning = currWeekMaxEarning } }

getTransactionItems :: CustomerReferralTrackerScreenState -> Array {key :: String, value :: String}
getTransactionItems state = 
  case state.data.selectedItem of 
    Nothing -> []
    Just earning -> 
      [ {key: "Order ID", value: earning.id}
      , {key: "Paid by", value: "Namma Yatri"}
      ]

getDatesList :: String -> CustomerReferralTrackerScreenState -> Array String
getDatesList todaysDate state =
  map (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") $ getListofPastDays
  where
    getListofPastDays = getPastDays (22 + getDayOfWeek (getDayName todaysDate))
  
