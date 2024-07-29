module Screens.EarningsScreen.Weekly.Controller where

import Common.Types.App (Currency(..), Distance(..), DistanceUnit(..))
import Constants.Configs (dummyDistance)
import Data.Array as DA
import Data.Int (toNumber)
import Engineering.Helpers.Commons as EHC
import Data.Foldable (foldl)
import Helpers.Utils as HU
import Prelude
import PrestoDOM
import Effect
import Data.Maybe
import Screens.EarningsScreen.ScreenData
import Services.API (RidesSummary(..))
import Debug
import Storage
import JBridge as JB
import Helpers.Utils (incrementValueOfLocalStoreKey, dummyPriceForCity, getcurrentdate)
import Screens.EarningsScreen.Common.Utils
import Screens.EarningsScreen.Common.Types (RideData)
import Components.BottomNavBar.Controller (Action(..)) as BottomNavBar
import Effect.Unsafe (unsafePerformEffect)

data ScreenOutput
  = NextScreen State
  | Back State
  | GoToDaily State
  | UpdatedWeeklyEarnings State
  | GoToRideHistoryScreen State
  | GoToHomeScreen State
  | GoToRidesScreen State
  | GoToProfileScreen State
  | GoToNotifications State
  | GoToReferralScreen State
  | SubscriptionScreen State
  | GoToPayoutHistory State
  | GoToHelpAndSupportScreen

data Action
  = NextClick
  | BackClick
  | ChangeTab
  | ToggleInfoView
  | ShowTips
  | ShowAdjustments
  | UpdateRideSummary (Array WeeklyEarning) Int String Boolean TotalWeeklyEarningsData
  | GetWeeklyEarnings Boolean
  | OptionClickedFromList Int
  | BottomNavBarAction BottomNavBar.Action
  | GoToHelpAndSupport

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> State -> Eval Action ScreenOutput State

eval (UpdateRideSummary currentWeekData currWeekMaxEarning fromDate isCurrentWeek totalWeeklyEarningsdata) state = continue state { props { startBarAnim = true, currWeekData = currentWeekData, currentWeekMaxEarning = currWeekMaxEarning, fromDate = fromDate, toDate = EHC.getFutureDate fromDate 6, isCurrentWeek = isCurrentWeek, totalWeeklyEarningsdata = totalWeeklyEarningsdata}}

eval NextClick state = exit $ NextScreen state

eval ChangeTab state = exit $ GoToDaily state

eval ToggleInfoView state = continue state { props { rideDistanceInfoPopUp = not state.props.rideDistanceInfoPopUp } }

eval ShowAdjustments state = continue state { data { prevAdjustmentRotation = state.data.adjustmentRotation, adjustmentRotation = if state.data.adjustmentRotation == 0.0 then 180.0 else 0.0 } }

eval (GetWeeklyEarnings leftButtonClicked) state = do
  if leftButtonClicked then updateAndExit state{props{startBarAnim = false}} $ UpdatedWeeklyEarnings state {props {fromDate = EHC.getPastDate state.props.fromDate 7, toDate = EHC.getPastDate state.props.toDate 7, isCurrentWeek = false, startBarAnim = false}}
    else do
      let fromDate = EHC.getFutureDate state.props.fromDate 7
          toDate = EHC.getFutureDate state.props.toDate 7
          isCurrentWeek = EHC.withinDateRange fromDate toDate (HU.getcurrentdate "")
      updateAndExit state{props{startBarAnim = false}} $ UpdatedWeeklyEarnings state {props {fromDate = fromDate, toDate = toDate, isCurrentWeek = isCurrentWeek, startBarAnim = false}}

eval (OptionClickedFromList index) state = do
  case index of
    0 -> updateAndExit state $ GoToRideHistoryScreen state
    1 -> updateAndExit state $ GoToPayoutHistory state
    _ -> continue state

eval (BottomNavBarAction (BottomNavBar.OnNavigate item)) state = do
  pure $ JB.hideKeyboardOnNavigation true
  case item of
    "Home" -> exit $ GoToHomeScreen state
    "Rides" -> exit $ GoToRidesScreen state
    "Profile" -> exit $ GoToProfileScreen state
    "Rankings" -> do
      void $ pure $ incrementValueOfLocalStoreKey TIMES_OPENED_NEW_BENEFITS
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ GoToReferralScreen state
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      exit $ GoToNotifications state
    "Join" -> do
      let
        driverSubscribed = getValueToLocalNativeStore DRIVER_SUBSCRIBED == "true"
      void $ pure $ incrementValueOfLocalStoreKey TIMES_OPENED_NEW_SUBSCRIPTION
      _ <- pure $ JB.cleverTapCustomEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      _ <- pure $ JB.metaLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      let
        _ = unsafePerformEffect $ JB.firebaseLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      exit $ SubscriptionScreen state
    _ -> continue state

eval GoToHelpAndSupport state = exit GoToHelpAndSupportScreen

eval _ state = update state


mapSummaryListWithWeeklyEarnings :: Array RidesSummary -> Array WeeklyEarning
mapSummaryListWithWeeklyEarnings ridesSummaryList =
  map
    ( \(RidesSummary rideSummary) ->
        { earnings: rideSummary.earnings
        , earningsWithCurrency: rideSummary.earningsWithCurrency
        , rideDistance: rideSummary.rideDistance
        , rideDistanceWithUnit: rideSummary.rideDistanceWithUnit
        , rideDate: rideSummary.rideDate
        , noOfRides: rideSummary.noOfRides
        , percentLength: 0.0
        }
    )
    ridesSummaryList

getEarningListWithMissingDates :: Array WeeklyEarning -> Array String -> Array WeeklyEarning
getEarningListWithMissingDates earningLst dates = do
  let
    x = DA.catMaybes $ map (\d -> (getEarningForDate earningLst d)) dates
  DA.sortBy (comparing _.rideDate) (earningLst <> x)

getEarningForDate :: Array WeeklyEarning -> String -> Maybe WeeklyEarning
getEarningForDate earningLst date =
  let
    foundDate = DA.find (\e -> e.rideDate == date) earningLst
  in
    maybe (Just { earnings: 0, earningsWithCurrency: HU.dummyPriceForCity (getValueToLocalStore DRIVER_LOCATION), rideDistance: 0, rideDate: date, rideDistanceWithUnit: dummyDistance, noOfRides: 0, percentLength: 0.0 }) (\_ -> Nothing) foundDate

getWeeklyEarningsPercentage :: Array WeeklyEarning -> Array WeeklyEarning
getWeeklyEarningsPercentage weeklyEarningData = map (\x -> x { percentLength =((toNumber x.earnings) * 100.0) / (toNumber maxValue) }) weeklyEarningData
  where
  maxValue = DA.foldl getMax 0 weeklyEarningData

getMax :: Int -> WeeklyEarning -> Int
getMax num1 obj1 = max obj1.earnings num1

getTotalCurrentWeekData :: Array WeeklyEarning -> TotalWeeklyEarningsData
getTotalCurrentWeekData barGraphData = do
  let
    firstElement = barGraphData DA.!! 0

    lastElement = barGraphData DA.!! 6

    calculateTotals =
      foldl
        ( \acc record ->
            { totalEarnings: acc.totalEarnings + record.earnings
            , totalDistance: acc.totalDistance + record.rideDistance
            , totalRides: acc.totalRides + record.noOfRides
            }
        )
        { totalEarnings: 0, totalDistance: 0, totalRides: 0 }
        barGraphData

    currency = maybe INR (\x -> x.earningsWithCurrency.currency) firstElement

    (Distance distance) = maybe dummyDistance (\x -> x.rideDistanceWithUnit) firstElement
    
  { fromDate: maybe "" (\x -> x.rideDate) firstElement
  , toDate: maybe "" (\x -> x.rideDate) lastElement
  , totalRides: calculateTotals.totalRides
  , totalEarningsWithCurrency: {currency: currency, amount: toNumber calculateTotals.totalEarnings}
  , totalDistanceTravelledWithUnit: Distance {unit: distance.unit, value: toNumber calculateTotals.totalDistance}
  }

getMissingDatesFromList :: Array RidesSummary -> Array String -> Array String
getMissingDatesFromList rideSummaryList dates = DA.filter (\date -> maybe true (\_ -> false) (DA.find(\(RidesSummary obj) -> obj.rideDate == date) rideSummaryList)) dates

mapRideDataWithEarnings :: Array RideData -> Array WeeklyEarning
mapRideDataWithEarnings rideDataList = 
  map
    ( \rideData ->
        { earnings: rideData.earnings
        , earningsWithCurrency: rideData.earningsWithCurrency
        , rideDistance: rideData.rideDistance
        , rideDistanceWithUnit: rideData.rideDistanceWithUnit
        , rideDate: rideData.rideDate
        , noOfRides: rideData.noOfRides
        , percentLength: 0.0
        }
    )
    rideDataList