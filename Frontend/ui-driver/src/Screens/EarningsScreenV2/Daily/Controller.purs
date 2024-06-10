module Screens.EarningsScreen.Daily.Controller where

import Data.Maybe
import Effect
import Prelude
import PrestoDOM
import Screens.EarningsScreen.ScreenData
import Screens.EarningsScreen.Common.Utils
import Components.PrimaryButton as PrimaryButtonController
import Components.BottomNavBar.Controller (Action(..)) as BottomNavBar
import Services.API (RidesSummary(..), RidesInfo(..))
import JBridge (hideKeyboardOnNavigation, cleverTapCustomEvent, metaLogEvent, firebaseLogEvent, getDateFromDate, getCurrentDate)
import Helpers.Utils (incrementValueOfLocalStoreKey, dummyPriceForCity, getcurrentdate)
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore, getValueToLocalStore)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils (initializeCalendarWithDay, saveObject, getCurrentDayFromDate)
import Components.Calendar.Controller as CalendarController
import Data.Array (head, (!!))
import Data.String as DS
import Engineering.Helpers.Utils (priceToBeDisplayed, distanceTobeDisplayed)
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Styles.Colors as Color
import Data.Function.Uncurried (runFn2)
import Debug
import Screens.EarningsScreen.Common.Types
import Screens.DriverEarningsScreen.Controller (rideHistoryItemTransformer, dummyRideHistoryItem)

data ScreenOutput
  = NextScreen State
  | Back State
  | GoToWeekly State
  | GoToHomeScreen State
  | GoToRidesScreen State
  | GoToProfileScreen State
  | GoToNotifications State
  | GoToReferralScreen State
  | SubscriptionScreen State
  | DateUpdated State
  | SelectedTripDetails State
  | GoToHelpAndSupportScreen

data Action
  = NextClick
  | BackClick
  | ChangeTab
  | ToggleInfoView
  | RemovePopup
  | UpdateRideHistory (Array RidesInfo) String
  | UpdateRideData (Array RidesSummary)
  | CalendarAC CalendarController.Action
  | ShowCalendarPopup
  | BottomNavBarAction BottomNavBar.Action
  | ChangeDate String
  | AfterRender
  | UpdateDate String
  | NoAction
  | OpenSelectedTripDetails Int
  | GoToHelpAndSupport

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data INCREMENT_TYPE = INCREMENT | DECREMENT

eval :: Action -> State -> Eval Action ScreenOutput State
eval NextClick state = exit $ NextScreen state

eval ToggleInfoView state = continue state { props { showInfoView = not state.props.showInfoView } }

eval RemovePopup state = continue state { props { showInfoView = not state.props.showInfoView } }

eval ChangeTab state = updateAndExit state{props{isFromWeekly = false, isResetAnim = true}} $ GoToWeekly state

eval (UpdateRideData items) state = do
  let
    selectedDate = case head items of
      Nothing -> dummyRideSummaryType state.data.currentDate
      Just (RidesSummary val) ->
        { earningsWithCurrency: priceToBeDisplayed val.earningsWithCurrency false
        , rideDate: formateDate val.rideDate
        , noOfRides: show val.noOfRides
        , rideDistanceWithUnit: distanceTobeDisplayed val.rideDistanceWithUnit false true
        }
  continue state { data { selectedDate = Just selectedDate } }

eval (UpdateRideHistory item date) state = 
  if date == state.data.currentDate
    then do 
      let datas = rideHistoryTransformer item
      continue $ state{data{selectedDateRides = Just datas, selectedDatesRidesInfo = Just item}}
    else update state

eval AfterRender state = continue state{props{forwardBtnAlpha = if state.data.currentDate == formateDate getCurrentDate then 0.4 else 1.0}}

eval ShowCalendarPopup state = do
  let
    res = initializeCalendarWithDay (getCurrentDayFromDate (getDateFromCurrentDate state.data.currentDate) true) true
  continue state { data { calendarState { weeks = res.weeks, calendarPopup = true, selectedTimeSpan = res.selectedTimeSpan, startDate = res.startDate, endDate = Nothing } } }

eval (CalendarAC CalendarController.HideCalendarPopup) state = continue state { data { calendarState { calendarPopup = false, startDate = Nothing, endDate = Nothing } } }

eval (CalendarAC (CalendarController.DecrementMonth res)) state = continue state { data { calendarState { weeks = res.weeks, selectedTimeSpan = res.selectedTimeSpan } } }

eval (CalendarAC (CalendarController.IncrementMonth res)) state = continue state { data { calendarState { weeks = res.weeks, selectedTimeSpan = res.selectedTimeSpan } } }

eval (CalendarAC (CalendarController.SelectDate res)) state = continue state { data { calendarState { startDate = res.startDate, weeks = res.weeks } } }

eval (CalendarAC (CalendarController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
  let
    newState =
      state
        { data
          { calendarState { calendarPopup = false }
          , currentDate =
            case state.data.calendarState.startDate of
              Nothing -> getCurrentDate
              Just val -> convertUTCtoISC val.utcDate "YYYY-MM-DD"
          }
        }
  exit $ DateUpdated newState{data{selectedDateRides = Nothing, selectedDate = Nothing}, props {forwardBtnAlpha = if newState.data.currentDate == getCurrentDate then 0.4 else 1.0}}

eval (CalendarAC (CalendarController.PrimaryButtonCancelActionController (PrimaryButtonController.OnClick))) state = do
  continue state { data { calendarState { calendarPopup = false } } }

eval (ChangeDate updatedDate) state = exit $ DateUpdated state{data{selectedDateRides = Nothing, selectedDate = Nothing, selectedDatesRidesInfo = Nothing}, props {forwardBtnAlpha = if updatedDate == getCurrentDate then 0.4 else 1.0}}
eval (UpdateDate updatedDate) state = continue state{data{currentDate = formateDate updatedDate, selectedDateRides = Nothing, selectedDate = Nothing, selectedDatesRidesInfo = Nothing}, props {forwardBtnAlpha = if updatedDate == getCurrentDate then 0.4 else 1.0}}

eval (BottomNavBarAction (BottomNavBar.OnNavigate item)) state = do
  pure $ hideKeyboardOnNavigation true
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
      _ <- pure $ cleverTapCustomEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      _ <- pure $ metaLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      let
        _ = unsafePerformEffect $ firebaseLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      exit $ SubscriptionScreen state
    _ -> continue state

eval (OpenSelectedTripDetails index) state = do
  let 
      selectedDatesRideInfoArray = fromMaybe [] state.data.selectedDatesRidesInfo
      tripDetails = rideHistoryItemTransformer $ fromMaybe dummyRideHistoryItem (selectedDatesRideInfoArray !! index)
      updateedState = state { data{selectedRideItem = tripDetails } }
  updateAndExit updateedState $ SelectedTripDetails updateedState

eval GoToHelpAndSupport state = exit GoToHelpAndSupportScreen

eval _ state = continue state

dummyRideSummaryType :: String -> RidesSummaryType
dummyRideSummaryType date =
  { earningsWithCurrency: priceToBeDisplayed (dummyPriceForCity $ getValueToLocalStore DRIVER_LOCATION) false
  , rideDate: formateDate date
  , noOfRides: "0"
  , rideDistanceWithUnit: "0"
  }


rideHistoryTransformer :: Array RidesInfo -> Array RideComponent
rideHistoryTransformer items =
  let
    currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY/MM/DD"
  in
    map
      ( \(RidesInfo ride) ->
          let
            date = convertUTCtoISC ride.createdAt "YYYY/MM/DD"

            time = convertUTCtoISC ride.createdAt "h:mm a"

            isCancelledRide = ride.status == "CANCELLED"
          in
            { serviceTierType: ride.vehicleServiceTierName
            , date: if currentDate == date then "Today" else date
            , time: time
            , price: priceToBeDisplayed (fromMaybe (dummyPriceForCity (getValueToLocalStore DRIVER_LOCATION)) ride.computedFareWithCurrency) true
            , isCancelledRide: isCancelledRide
            , tags:
                ( if isCancelledRide then
                    [ { background: Color.backDanger
                      , color: Color.red900
                      , text: "Cancelled"
                      }
                    ]
                  else
                    []
                )
                  <> if isJust ride.customerCancellationDues && (fromMaybe 0.0 ride.customerCancellationDues) > 0.0 then
                      [ { background: "#F6F1FF"
                        , color: Color.purple700
                        , text: "Earned Penality"
                        }
                      ]
                    else
                      []
            }
      ) items 
