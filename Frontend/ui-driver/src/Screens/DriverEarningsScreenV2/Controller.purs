{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverEarningsScreenV2.Controller where

import Effect.Unsafe
import Engineering.Helpers.LogEvent
import Prelude
import Components.BottomNavBar.Controller (Action(..)) as BottomNavBar
import Components.Calendar.Controller as CalendarController
import Components.ErrorModal as ErrorModalController
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Common.Types.App (LazyCheck(..))
import Components.PrimaryButton as PrimaryButtonController
import Components.RequestInfoCard as RequestInfoCard
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
import Engineering.Helpers.Commons (getCurrentUTC, getFutureDate, getDayName, convertUTCtoISC, getPastDays, getPastMonths)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils (initializeCalendar, saveObject, getCurrentDay)
import Foreign.Generic (decodeJSON)
import Helpers.Utils (checkSpecialPickupZone, getcurrentdate, getDayOfWeek, incrementValueOfLocalStoreKey, getRideLabelData, parseFloat, getRequiredTag, transformBapName ,dummyLocationInfo )
import JBridge (pauseYoutubeVideo)
import Language.Strings (getString)
import RemoteConfig.Utils
import Language.Types
import Log
import PrestoDOM (Eval, update, continue, exit, ScrollState(..), updateAndExit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Resource.Constants (decodeAddress, rideTypeConstructor)
import Screens (ScreenName(..), getScreen)
import Screens.DriverEarningsScreenV2.Transformer (getEventName)
import Screens.Types (DriverEarningsScreenState, DriverEarningsSubView(..), AnimationState(..), IndividualRideCardState(..), IndividualRideCardState(..), DisabilityType(..))
import Screens.Types as ST
import Services.API (Status(..), CoinTransactionRes(..), CoinTransactionHistoryItem(..), CoinsUsageRes(..), CoinUsageHistoryItem(..), RidesInfo(..), LocationInfo(..), DriverProfileSummaryRes(..), RidesSummary(..))
import Services.API as API
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore, getValueToLocalStore)
import Timers (clearTimerWithId)
import Debug
import Foreign (unsafeToForeign)
import Resource.Constants as Const
import Helpers.Utils (fetchImage, FetchImageFrom(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> trackAppBackPress appId (getScreen DRIVER_EARNINGS_SCREEN)
    NoAction -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "no_action" "no_action"
    (ChangeTab subView) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "change_tab" (show subView)
    (BottomNavBarAction act) -> case act of
      BottomNavBar.OnNavigate screen -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "bottom_nav_bar_action" screen
    (RideSummaryAPIResponseAction _ _ _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "ride_summary_api_response_action" "ride_summary_api_response_action"
    (UpdateRidesEver _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "update_rides_ever" "update_rides_ever"
    AfterRender -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "after_render" "after_render"
    (GenericHeaderAC _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "generic_header_action" "generic_header_action"
    (ErrorModalActionController _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "error_modal_action" "error_modal_action"
    (BarViewSelected _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "bar_view_selected" "bar_view_selected"
    LeftChevronClicked -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "left_chevron_clicked" "left_chevron_clicked"
    RightChevronClicked -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "right_chevron_clicked" "right_chevron_clicked"
    (EarningPeriodStatsAPIResponseAction _ _ _) -> trackAppActionClick appId (getScreen DRIVER_EARNINGS_SCREEN) "earning_period_stats_api_response_action" "earning_period_stats_api_response_action"

data ScreenOutput
  = GoBack
  | HomeScreen DriverEarningsScreenState
  | Alerts DriverEarningsScreenState
  | Contest DriverEarningsScreenState
  | SubscriptionScreen DriverEarningsScreenState
  | ChangeDriverEarningsTab DriverEarningsSubView DriverEarningsScreenState
  | RefreshScreen DriverEarningsScreenState

data Action
  = NoAction
  | ChangeTab DriverEarningsSubView
  | BackPressed
  | BottomNavBarAction BottomNavBar.Action
  | RideSummaryAPIResponseAction (Array RidesSummary) String (Array String)
  | UpdateRidesEver Boolean
  | AfterRender
  | GenericHeaderAC GenericHeader.Action
  | ErrorModalActionController ErrorModalController.Action
  | BarViewSelected Int
  | LeftChevronClicked
  | RightChevronClicked
  | EarningPeriodStatsAPIResponseAction (Array API.EarningPeriodStats) String String

eval :: Action -> DriverEarningsScreenState -> Eval Action ScreenOutput DriverEarningsScreenState
eval BackPressed state = 
  if state.props.showCoinsUsagePopup then
    continue state { props { showCoinsUsagePopup = false } }
  else if state.props.popupType /= ST.NO_POPUP then
    continue state { props { popupType = ST.NO_POPUP } }
  else exit $ HomeScreen (updateToState state)

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  case screen of
    "Home" -> exit $ HomeScreen (updateToState state)
    "Alert" -> do
      void $ pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      exit $ Alerts (updateToState state)
    "Rankings" -> do
      void $ pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ Contest (updateToState state)
    "Join" -> do
      exit $ SubscriptionScreen (updateToState state)
    _ -> continue (updateToState state)

eval (ChangeTab subView') state = do
  let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_driver_earnings_scn_change_tab" $ [{key : "Tab", value : unsafeToForeign (show subView')}]
  if subView' == state.props.subView then continue state else exit $ ChangeDriverEarningsTab subView' state { props { graphIndex = 0, fromDate = "", toDate = "" } }

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (BarViewSelected index) state = do
  let
    mbSelectedBarData = state.props.currWeekData !! index

    selectedBarData =
      { fromDate: maybe "" (\record -> record.rideDate) mbSelectedBarData
      , toDate: ""
      , totalEarnings: maybe 0 (\record -> record.earnings) mbSelectedBarData
      , totalRides: maybe 0 (\record -> record.noOfRides) mbSelectedBarData
      , totalDistanceTravelled: maybe 0 (\record -> record.rideDistance) mbSelectedBarData
      , cancellationCharges: maybe 0 (\record -> record.cancellationCharges) mbSelectedBarData
      , tipAmount: maybe 0 (\record -> record.tipAmount) mbSelectedBarData
      }
  continue state { props { selectedBarIndex = if state.props.selectedBarIndex == index then -1 else index, totalEarningsData = if state.props.selectedBarIndex == index then getTotalCurrentWeekData state.props.currWeekData else selectedBarData } }

eval LeftChevronClicked state = do
  let dates = 
        case state.props.subView of
          ST.EARNINGS_VIEW -> {fromDate: EHC.getPastDateFromDate state.props.fromDate 7, toDate: EHC.getPastDateFromDate state.props.toDate 7}
          ST.WEEKLY_EARNINGS_VIEW -> {fromDate: (EHC.getPastDateFromDate state.props.fromDate 49), toDate: EHC.getPastDateFromDate state.props.fromDate 7}
          ST.MONTHLY_EARNINGS_VIEW -> do
            let pastMonths = EHC.getPastMonthsFromDate state.props.fromDate 7
                fromDate = maybe "2025-01-01" (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") (DA.head pastMonths)
                toDate = maybe "2025-01-01" (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") (DA.last pastMonths)
            {fromDate: fromDate, toDate: toDate}
          _ -> {fromDate: "2025-01-01", toDate: "2025-01-01"}
      newState = state { props { fromDate = dates.fromDate, toDate = dates.toDate, graphIndex = state.props.graphIndex - 1, selectedBarIndex = -1, showShimmer = true } }
  updateAndExit newState $ RefreshScreen newState


eval RightChevronClicked state = do
  let dates = 
        case state.props.subView of
          ST.EARNINGS_VIEW -> {fromDate: EHC.getFutureDate state.props.fromDate 7, toDate: EHC.getFutureDate state.props.toDate 7}
          ST.WEEKLY_EARNINGS_VIEW -> {fromDate: (EHC.getFutureDate state.props.toDate 7), toDate: EHC.getFutureDate state.props.toDate 49}
          ST.MONTHLY_EARNINGS_VIEW -> do
            let futureMonths = EHC.getFutureMonthsFromDate state.props.toDate 7
                fromDate = maybe "2025-01-01" (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") (DA.head futureMonths)
                toDate = maybe "2025-01-01" (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") (DA.last futureMonths)
            {fromDate: fromDate, toDate: toDate}
          _ -> {fromDate: "2025-01-01", toDate: "2025-01-01"}
      newState = state { props { fromDate = dates.fromDate, toDate = dates.toDate, graphIndex = state.props.graphIndex + 1, selectedBarIndex = -1, showShimmer = true } }
  updateAndExit newState $ RefreshScreen newState

eval (UpdateRidesEver anyRidesAssignedEver) state = continue state { data { anyRidesAssignedEver = anyRidesAssignedEver }, props { weekIndex = 3 } }

eval (EarningPeriodStatsAPIResponseAction earningPeriodStats fromDate toDate) state = do
  let datesList = getDatesList state fromDate toDate
      periodStatsToWeeklyEarning = map (\(API.EarningPeriodStats earningPeriodStats) -> { earnings: earningPeriodStats.totalEarnings, rideDistance: earningPeriodStats.totalDistance, rideDate: earningPeriodStats.periodStart, noOfRides: earningPeriodStats.totalRides, percentLength: 0.0, cancellationCharges: earningPeriodStats.cancellationCharges, tipAmount: earningPeriodStats.tipAmount }) earningPeriodStats
      earningPeriodStatsWithMissingDates = getEarningListWithMissingDates periodStatsToWeeklyEarning datesList
      earningPeriodStatsWithPercentage = getWeeklyEarningsPercentage earningPeriodStatsWithMissingDates
      currWeekMaxEarning = foldl getMax 0 earningPeriodStatsWithPercentage
  continue state { props { currWeekData = earningPeriodStatsWithPercentage, totalEarningsData = getTotalCurrentWeekData earningPeriodStatsWithPercentage, currentWeekMaxEarning = currWeekMaxEarning, fromDate = fromDate, toDate = toDate, showShimmer = false } }

eval _ state = update state

getEarningListWithMissingDates :: Array ST.WeeklyEarning -> Array String -> Array ST.WeeklyEarning
getEarningListWithMissingDates earningLst dates = do
  let
    x = DA.catMaybes $ map (\d -> (getEarningForDate earningLst d)) dates
  sortBy (comparing _.rideDate) (earningLst <> x)

getEarningForDate :: Array ST.WeeklyEarning -> String -> Maybe ST.WeeklyEarning
getEarningForDate earningLst date =
  let
    foundDate = DA.find (\e -> e.rideDate == date) earningLst
  in
    maybe (Just { earnings: 0, rideDistance: 0, rideDate: date, noOfRides: 0, percentLength: 0.0, cancellationCharges: 0, tipAmount: 0 }) (\_ -> Nothing) foundDate

getTagImages :: RidesInfo -> Array String
getTagImages (RidesInfo ride) =
  let
    tag = getRequiredTag ride.specialLocationTag
    conditionsAndTags = 
      [ {condition: isJust ride.customerExtraFee, tag: fetchImage FF_ASSET "ny_ic_tip_ride_tag"}
      , {condition: isJust ride.disabilityTag, tag: fetchImage FF_ASSET "ny_ic_disability_tag"}
      , {condition: isJust ride.specialLocationTag && isJust tag, tag: fetchImage FF_ASSET "ny_ic_star"}
      , {condition: isJust ride.driverGoHomeRequestId, tag: fetchImage FF_ASSET "ny_ic_goto_home_tag"}
      , {condition: checkSpecialPickupZone ride.specialLocationTag, tag: fetchImage COMMON_ASSET "ny_ic_sp_zone_green"}
      ]
  in
    DA.concatMap (\{condition, tag} -> if condition then [tag] else []) conditionsAndTags

getTotalCurrentWeekData :: Array ST.WeeklyEarning -> ST.TotalEarningsData
getTotalCurrentWeekData barGraphData = do
  let
    firstElement = barGraphData !! 0

    lastElement = barGraphData !! 6

    calculateTotals =
      foldl
        ( \acc record ->
            { totalEarnings: acc.totalEarnings + record.earnings
            , totalDistance: acc.totalDistance + record.rideDistance
            , totalRides: acc.totalRides + record.noOfRides
            , cancellationCharges: acc.cancellationCharges + record.cancellationCharges
            , tipAmount: acc.tipAmount + record.tipAmount
            }
        )
        { totalEarnings: 0, totalDistance: 0, totalRides: 0, cancellationCharges: 0, tipAmount: 0 }
        barGraphData
  { fromDate: maybe "" (\x -> x.rideDate) firstElement
  , toDate: maybe "" (\x -> x.rideDate) lastElement
  , totalEarnings: calculateTotals.totalEarnings
  , totalRides: calculateTotals.totalRides
  , totalDistanceTravelled: calculateTotals.totalDistance
  , cancellationCharges: calculateTotals.cancellationCharges
  , tipAmount: calculateTotals.tipAmount
  }

getWeeklyEarningsPercentage :: Array ST.WeeklyEarning -> Array ST.WeeklyEarning
getWeeklyEarningsPercentage weeklyEarningData = map (\x -> x { percentLength = ((toNumber x.earnings) * 100.0) / (toNumber maxValue) }) weeklyEarningData
  where
  maxValue = foldl getMax 0 weeklyEarningData

getMax :: Int -> ST.WeeklyEarning -> Int
getMax num1 obj1 = max obj1.earnings num1

updateToState :: DriverEarningsScreenState -> DriverEarningsScreenState
updateToState state = state { props { selectedBarIndex = -1, graphIndex = 0, showShimmer = true, fromDate = "", toDate = "" }}

getDatesList :: ST.DriverEarningsScreenState -> String -> String -> Array String
getDatesList state fromDate toDate =
  case state.props.subView of
    ST.EARNINGS_VIEW -> (map (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") (EHC.getPastDaysFromDate toDate 7))
    ST.WEEKLY_EARNINGS_VIEW -> do
      let dayOfWeek = getDayOfWeek (getDayName toDate)
          firstDateOfWeek = EHC.getPastDateFromDate toDate dayOfWeek
      DA.reverse (DA.mapWithIndex (\index item -> EHC.getPastDateFromDate firstDateOfWeek (index * 7)) [1,2,3,4,5,6,7])
    ST.MONTHLY_EARNINGS_VIEW -> (map (\x -> convertUTCtoISC x.utcDate "YYYY-MM-DD") (EHC.getPastMonthsFromDate toDate 6)) <> [toDate]
    _ -> []