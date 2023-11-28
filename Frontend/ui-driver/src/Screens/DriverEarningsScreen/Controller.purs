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
import Components.ErrorModal as ErrorModalController
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryButton as PrimaryButtonController
import Components.RequestInfoCard as RequestInfoCard
import Control.Monad.Except (runExcept)
import Data.Array as DA
import Data.Array (union, (!!), filter, length, (:), foldl, drop, take, replicate, updateAt, elemIndex, (..), last, find, catMaybes, sortBy)
import Data.Either
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (fromString) as NUM
import Data.Show (show)
import Data.String as DS
import Data.String (Pattern(..), split, take, drop)
import Engineering.Helpers.Commons (clearTimer, getCurrentUTC, getFutureDate, getDayName)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils (initializeCalendar, saveObject)
import Foreign.Generic (decodeJSON)
import Helpers.Utils (getcurrentdate, getDayOfWeek)
import Log (trackAppScreenRender)
import PrestoDOM (Eval, continue, exit, ScrollState(..), updateAndExit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Resource.Constants (decodeAddress)
import Screens (ScreenName(..), getScreen)
import Screens.DriverEarningsScreen.Transformer (getEventName)
import Screens.Types (DriverEarningsScreenState, DriverEarningsSubView(..), AnimationState(..), ItemState(..), IndividualRideCardState(..), DisabilityType(..))
import Screens.Types as ST
import Services.API (Status(..), CoinTransactionRes(..), CoinTransactionHistoryItem(..), CoinsUsageRes(..), CoinUsageHistoryItem(..), RidesInfo(..), DriverProfileSummaryRes(..), RidesSummary(..))
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
                  | RefreshScreen DriverEarningsScreenState
                    
data Action = Dummy
            | ChangeTab DriverEarningsSubView
            | BackPressed
            | PrimaryButtonActionController PrimaryButtonController.Action
            | PlanCount Boolean
            | BottomNavBarAction BottomNavBar.Action
            | RideHistoryAPIResponseAction (Array RidesInfo)
            | RideSummaryAPIResponseAction (Array RidesSummary) String (Array String)
            | DriverSummary DriverProfileSummaryRes
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
            | ErrorModalActionController ErrorModalController.Action
            | BarViewSelected Int 
            | LeftChevronClicked Int
            | RightChevronClicked Int
            | FaqQuestionView FaqQuestions

eval :: Action -> DriverEarningsScreenState -> Eval Action ScreenOutput DriverEarningsScreenState

eval BackPressed state = if state.props.subView == USE_COINS_VIEW 
                            then exit $ ChangeDriverEarningsTab YATRI_COINS_VIEW
                            else exit $ HomeScreen (updateToState state)

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = exit $ PurchasePlan state

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  case screen of
    "Home" -> exit $ HomeScreen (updateToState state)
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      exit $ Alerts (updateToState state)
    "Rankings" -> do
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ Contest (updateToState state)
    "Join" -> do
      exit $ SubscriptionScreen (updateToState state)
    _ -> continue (updateToState state)

eval (ChangeTab subView') state = exit $ ChangeDriverEarningsTab subView'

eval (CoinTransactionResponseAction (CoinTransactionRes resp)) state = do
  let events = map (\(CoinTransactionHistoryItem item) -> {event : getEventName item.eventFunction, timestamp : item.createdAt, coins : item.coins, cash : 0.0, earnings: Nothing, destination : Nothing, status : Nothing, tagImages : []}) resp.coinTransactionHistory
  continue state {data {expiringDays = resp.expiringDays, expiringCoins = resp.expiringCoins, coinsEarned = resp.coinEarned, coinsUsed = resp.coinUsed, coinBalance = resp.coinBalance, coinsEarnedPreviousDay = resp.coinsEarnedPreviousDay, coinsEarnedToday = resp.todayCoinSummary, coinHistoryItems = events}}

eval (CoinUsageResponseAction (CoinsUsageRes resp)) state = do
  let usageHistory = map (\(CoinUsageHistoryItem item) -> {event : item.title, timestamp : item.createdAt, coins : item.numCoins, cash : item.cash, earnings : Nothing, destination : Nothing, status : Nothing, tagImages : []}) resp.coinUsageHistory
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
    ST.COIN_TO_CASH_POPUP -> do
      _ <- pure $ clearTimer state.data.timerID
      let newState = state{props{subView = ST.YATRI_COINS_VIEW, popupType = ST.NO_POPUP, showCoinsRedeemedAnim = "", showShimmer = true}}
      updateAndExit newState $ RefreshScreen newState
    ST.COIN_TO_CASH_FAIL_POPUP -> do
      _ <- pure $ clearTimer state.data.timerID
      let newState = state{props{subView = ST.USE_COINS_VIEW, popupType = ST.NO_POPUP, showCoinsRedeemedAnim = "", showShimmer = true}}
      updateAndExit newState $ RefreshScreen newState
    ST.NO_COINS_POPUP -> do
      continue state {props {popupType = ST.NO_POPUP}} 
    ST.COINS_EXPIRING_POPUP -> continue state
    _ -> continue state

eval (PopUpModalAC PopUpModal.OnButton2Click) state = continueWithCmd state [pure $ PopUpModalAC PopUpModal.DismissPopup]

eval (PopUpModalAC PopUpModal.DismissPopup) state = do
  _ <- pure $ clearTimer state.data.timerID
  let newState = state {props{ popupType = ST.NO_POPUP, showCoinsRedeemedAnim = ""}}
  updateAndExit newState $ RefreshScreen newState

eval (CountDown seconds id status timerID) state = do
        if status == "EXPIRED" then do
            _ <- pure $ clearTimer state.data.timerID
            continue state{data{timerID = "", timer = 3}, props{showCoinsRedeemedAnim = "", popupType = if state.props.coinConvertedSuccess then ST.COIN_TO_CASH_POPUP else ST.COIN_TO_CASH_FAIL_POPUP, coinConvertedSuccess = false}}
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
  let newState = state { props {calendarState{ calendarPopup = false}}}
  updateAndExit newState $ RefreshScreen newState

eval (CalendarAC (CalendarController.PrimaryButtonCancelActionController (PrimaryButtonController.OnClick))) state = do
  continue state { props {calendarState{ calendarPopup = false}}}

eval (SliderCallback value) state = do
  _ <- pure $ spy "slider value" value
  continue state {data {coinsToUse = value}}

eval ShowCoinsUsagePopup state = do
  continue state {props {showCoinsUsagePopup = true}}

eval (RequestInfoCardAction RequestInfoCard.Close) state = continue state {props {showCoinsUsagePopup = false}}

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = continue state {props {showCoinsUsagePopup = false}}

eval (BarViewSelected index) state = do
  let mbSelectedBarData = state.props.currWeekData !! index
  let selectedBarData = { fromDate : case mbSelectedBarData of
                                  Just mbSelectedBarData -> mbSelectedBarData.rideDate
                                  Nothing -> "",
    toDate : "",
    totalEarnings : case mbSelectedBarData of
                      Just mbSelectedBarData -> mbSelectedBarData.earnings
                      Nothing -> 0,
    totalRides : case mbSelectedBarData of
                    Just mbSelectedBarData -> mbSelectedBarData.noOfRides
                    Nothing -> 0,
    totalDistanceTravelled : case mbSelectedBarData of
                    Just mbSelectedBarData -> mbSelectedBarData.rideDistance
                    Nothing -> 0
  } 
  continue state{props{selectedBarIndex = if state.props.selectedBarIndex == index then -1 else index, totalEarningsData = if state.props.selectedBarIndex == index then getTotalCurrentWeekData state.props.currWeekData else selectedBarData}}

eval (LeftChevronClicked currentIndex) state = do
  let updatedIndex = if currentIndex == 0 then currentIndex else currentIndex - 1
      currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek updatedIndex state.data.weeklyEarningData)
      currWeekMaxEarning = foldl getMax 0 currentWeekData
  continue state{props{weekIndex = updatedIndex, selectedBarIndex = -1, currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData, currentWeekMaxEarning = currWeekMaxEarning, showShimmer = false}}

eval (RightChevronClicked currentIndex) state = do
  let updatedIndex = if currentIndex == 3 then currentIndex else currentIndex + 1
      currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek updatedIndex state.data.weeklyEarningData)
      currWeekMaxEarning = foldl getMax 0 currentWeekData
  continue state{props{weekIndex = updatedIndex, selectedBarIndex = -1, currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData, currentWeekMaxEarning = currWeekMaxEarning}}

eval (RideSummaryAPIResponseAction ridesSummaryList todaysDate datesList) state = do
  let earningList = (map (\(RidesSummary rideSummary) -> {
  earnings : rideSummary.earnings,
  rideDistance : rideSummary.rideDistance,
  rideDate : rideSummary.rideDate,
  noOfRides : rideSummary.noOfRides,
  percentLength : 0.0
  }) ridesSummaryList)
  let earningListWithMissingDates = getEarningListWithMissingDates earningList datesList
  let dataToFetchAndUpdateFromCache = getEarningsToCache earningListWithMissingDates
      dayOfWeek = getDayOfWeek (getDayName todaysDate)
      earningsToCache = (DA.drop (length dataToFetchAndUpdateFromCache - 21 - dayOfWeek) dataToFetchAndUpdateFromCache)
  _ <- pure $ saveObject "RIDE_SUMMARY_DATA" earningsToCache
  let _ = fetchWeekyEarningData RIDE_SUMMARY_DATA
      noOfDaysToNearestSunday = 6 - dayOfWeek
      datesUptoMearestSunday = if noOfDaysToNearestSunday > 0 then map (\x -> getFutureDate todaysDate x) (1..noOfDaysToNearestSunday) else []
      allWeeksData = getAllWeeksData earningsToCache earningListWithMissingDates datesUptoMearestSunday
      currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek state.props.weekIndex allWeeksData)
      currWeekMaxEarning = foldl getMax 0 currentWeekData
  continue state{data{weeklyEarningData = allWeeksData}, props {currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData, currentWeekMaxEarning = currWeekMaxEarning}}

eval (RideHistoryAPIResponseAction rideList) state = do
  let earningHistoryItemList = earningHistoryItemsListTransformer rideList
  continue $ state {data{earningHistoryItems = earningHistoryItemList}, props{showShimmer = false}}

eval (FaqQuestionView faqQuestion) state = do
  continue state{props{subView = spy "FAQ_QUESTON_VIEW" FAQ_QUESTON_VIEW, individualQuestion = faqQuestion}}

eval _ state = continue state

getEarningListWithMissingDates :: Array ST.WeeklyEarning -> Array String -> Array ST.WeeklyEarning 
getEarningListWithMissingDates earningLst dates = do
  let x = DA.catMaybes $ map (\d -> (getEarningForDate earningLst d)) dates
  sortBy (comparing _.rideDate) (earningLst <> x)
  
getEarningForDate ::  Array ST.WeeklyEarning -> String -> Maybe ST.WeeklyEarning 
getEarningForDate earningLst date = 
  case DA.find (\e -> e.rideDate == date) earningLst of
    Nothing -> Just {earnings : 0, rideDistance : 0, rideDate : date, noOfRides : 0, percentLength : 0.0}
    _ -> Nothing

getEarningsToCache :: Array ST.WeeklyEarning -> Array ST.WeeklyEarning
getEarningsToCache earningList = do
  let cachedData = fetchWeekyEarningData RIDE_SUMMARY_DATA 
  fromMaybe [] cachedData <> DA.take (DA.length earningList - 1) earningList

getAllWeeksData :: Array ST.WeeklyEarning -> Array ST.WeeklyEarning -> Array String -> Array ST.WeeklyEarning
getAllWeeksData cachedEarnings earningList dates = do
  let objList = map (\x -> {earnings : 0, rideDistance : 0, rideDate : x, noOfRides : 0, percentLength : 0.0}) dates
      todaysData = case DA.last earningList of
                        Just x -> [x]
                        Nothing -> []
  cachedEarnings <> todaysData <> objList

earningHistoryItemsListTransformer :: Array RidesInfo -> Array ST.CoinHistoryItem
earningHistoryItemsListTransformer list = (map (\(RidesInfo ride) -> {
  destination : Just (decodeAddress (ride.toLocation) false),
  timestamp : ride.createdAt,
  earnings : case ride.status of
                    "CANCELLED" -> Just 0
                    _ -> ride.computedFare,
  status : Just ride.status, 
  coins :  0,
  event : "",
  tagImages : getTagImages (RidesInfo ride),
  cash : 0.0
}) list) 

getTagImages :: RidesInfo -> Array String
getTagImages (RidesInfo ride) = do
  let a = case ride.customerExtraFee of
            Just _ -> ["ny_ic_tip_ride_tag"]
            Nothing -> []
      b = case ride.disabilityTag of
          Just _ -> ["ny_ic_disability_tag"]
          Nothing -> []
      c = case ride.specialLocationTag of
          Just _ -> ["ny_ic_special_location_tag"]
          Nothing -> []
      d = case ride.driverGoHomeRequestId of
          Just _ -> ["ny_ic_goto_home_tag"]
          Nothing -> []
  (a <> b <> c <> d)
  

getTotalCurrentWeekData :: Array ST.WeeklyEarning -> ST.TotalEarningsData
getTotalCurrentWeekData barGraphData = do
  let firstElement = barGraphData !! 0
      lastElement = barGraphData !! 6
      totalEarnings = foldl (\acc record -> acc + record.earnings) 0 barGraphData
      totalDistance = foldl (\acc record ->  acc + record.rideDistance ) 0 barGraphData
      totalRides = foldl (\acc record -> acc + record.noOfRides) 0 barGraphData
  {fromDate : case firstElement of
              Just firstElement -> firstElement.rideDate
              Nothing -> "",
    toDate : case lastElement of
              Just lastElement -> lastElement.rideDate
              Nothing -> "",
    totalEarnings : totalEarnings,
    totalRides : totalRides,
    totalDistanceTravelled : totalDistance
  }  

fetchWeekyEarningData :: KeyStore -> Maybe (Array ST.WeeklyEarning)
fetchWeekyEarningData name = do
 let result = decodeJSON $ getValueToLocalNativeStore name
 case (runExcept $ result) of
  Left err -> Nothing
  Right weeklyEarnings -> Just weeklyEarnings

getEarningDataOfWeek :: Int -> Array ST.WeeklyEarning -> Array ST.WeeklyEarning
getEarningDataOfWeek index weeklyEarningData = 
  case index of
    3 -> DA.drop 21 weeklyEarningData
    2 -> DA.take 7 (DA.drop 14 weeklyEarningData)
    1 -> DA.take 7 (DA.drop 7 weeklyEarningData)
    0 -> DA.take 7 weeklyEarningData
    _ -> []

getWeeklyEarningsPercentage :: Array ST.WeeklyEarning -> Array ST.WeeklyEarning
getWeeklyEarningsPercentage weeklyEarningData = map (\x-> x{percentLength = ((toNumber x.earnings) * 100.0)/ (toNumber maxValue)}) weeklyEarningData
  where maxValue = foldl getMax 0 weeklyEarningData

getMax :: Int -> ST.WeeklyEarning -> Int
getMax num1 obj1 = if obj1.earnings > num1 then obj1.earnings else num1

findIndex :: forall a. Eq a => a -> Array a -> Maybe Int
findIndex element arr = elemIndex element arr

updateWeeklyEarningData :: Array ST.WeeklyEarning -> Array ST.WeeklyEarning -> Int -> Array ST.WeeklyEarning
updateWeeklyEarningData currWeekData allWeeksData weekIndex = do
  let obj = {
    earnings : 0,
        rideDistance : 0,
        rideDate : "",
        noOfRides : 0,
        percentLength : 0.0
  }
  let fillData = DA.replicate (weekIndex * 7) obj
  case weekIndex of 
    3 ->  fillData <> currWeekData <> DA.drop 28 allWeeksData
    2 ->  fillData <> currWeekData <> DA.drop 21 allWeeksData
    1 ->  fillData <> currWeekData <> DA.drop 14 allWeeksData
    0 ->  fillData <> currWeekData <> DA.drop 7 allWeeksData 
    _ ->  allWeeksData


updateToState :: DriverEarningsScreenState -> DriverEarningsScreenState
updateToState state = do
  let currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek 3 state.data.weeklyEarningData)
      currWeekMaxEarning = foldl getMax 0 currentWeekData
      date = getcurrentdate ""
      dayName = getDayName date
      noOfDaysToNearestSunday = 6 - case findIndex (DS.take 3 dayName) state.props.weekDay of
                                      Just index -> index
                                      Nothing -> 0
  state{props{weekIndex = 3, selectedBarIndex = -1, currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData, currentWeekMaxEarning = currWeekMaxEarning}}