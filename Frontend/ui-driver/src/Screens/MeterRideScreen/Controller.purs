module Screens.MeterRideScreen.Controller where

import Screens.Types (MeterRideScreenState)
import Data.Maybe
import JBridge (updateSliderValue, minimizeApp, openNavigation, differenceBetweenTwoUTC)
import Prelude
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable, Eval)
import PrestoDOM
import Timers (clearTimerWithId, startTimer, waitingCountdownTimerV2)
import Components.TripStageTopBar.Controller as TripStageTopBar
import Services.API
import Engineering.Helpers.Commons (getCurrentUTC, parseFloat)
import Data.Function.Uncurried (runFn2)
import Debug
import Resource.Constants (decodeAddress)
import Helpers.Utils
import Common.Types.App
import Data.Int (fromNumber, round, toNumber, fromString)
import Data.Number (fromString) as Number
import Engineering.Helpers.Commons as EHC

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = pure unit

data Action
  = NoAction
  | BackPressed
  | ShowRateCard
  | CloseRateCard
  | HandleStartButton
  | MeterRideTimerCallback Int String String
  | MeterRideStartedTimerCB String String Int
  | ChangeSlider Boolean
  | SliderCallback Int
  | TripStageTopBarAC TripStageTopBar.Action
  | EnterDestination
  | GoToProfile
  | HideStopMeterRideConfirmCard
  | ConfirmStopMeter
  | OnNavigate
  | UpdateRidesInfo RidesInfo
  | EndRide
  | DebounceCallBack String Boolean
  | UpdateRateCard RateCardRespItem
  | UpdateFare GetMeterPriceResp
  | RefreshTime
  | StopRotation String

data ScreenOutput
  = GoBack MeterRideScreenState
  | GoToEnterDestination MeterRideScreenState
  | GoToDriverProfile MeterRideScreenState
  | GoToHelpAndSupportScreen MeterRideScreenState
  | StartMeterRide MeterRideScreenState
  | EndMeterRide MeterRideScreenState
  | UpdatePrice MeterRideScreenState Int
  | TriggerGlobalEvents MeterRideScreenState
  | RefreshTimeOut MeterRideScreenState

eval :: Action -> MeterRideScreenState -> Eval Action ScreenOutput MeterRideScreenState
eval BackPressed state = do
  if state.props.isMeterRideStarted then do
    void $ pure $ minimizeApp ""
    continue state
  else if state.props.startButtonCountDown <= 3 then do
    void $ pure $ clearTimerWithId "MeterRideStartTimer"
    exit $ GoBack state { props { startButtonCountDown = 5 } }
  else
    exit $ GoBack state

eval RefreshTime state = updateAndExit state { props { refreshAnimation = true } } $ RefreshTimeOut state

eval (StopRotation _) state = continue state { props { refreshAnimation = false } }

eval ShowRateCard state = continue state { props { showRateCard = true } }

eval CloseRateCard state = continue state { props { showRateCard = false } }

eval HandleStartButton state = do
  if state.props.startButtonCountDown <= 3 then do
    let
      _ = clearTimerWithId "MeterRideStartTimer"
    continue state { props { startButtonCountDown = 5 } }
  else
    continueWithCmd state { props { startButtonCountDown = 3 } }
      [ do
          push <- getPushFn Nothing "MeterRideScreen"
          void $ startTimer 4 "MeterRideStartTimer" "1" push MeterRideTimerCallback
          pure $ NoAction
      ]

eval (MeterRideTimerCallback seconds status timerId) state = do
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId "MeterRideStartTimer"
    updateAndExit state { props { startButtonCountDown = 0 , rideStartingLoader = true} } $ StartMeterRide state{props{rideStartingLoader = true}}
  else
    continue state { props { startButtonCountDown = seconds } }

eval (MeterRideStartedTimerCB timerId timeInString sec) state = do
  continue state { data { timeSec = sec } }

eval (SliderCallback val) state = continue state { props { rateCardConfig { sliderVal = val }, isRateCardLoading = true } }

eval (ChangeSlider action) state = do
  let
    finalVal = if action then min state.props.rateCardConfig.sliderMaxValue (state.props.rateCardConfig.sliderVal + state.props.rateCardConfig.incrementUnit) else max state.props.rateCardConfig.sliderMinValue (state.props.rateCardConfig.sliderVal - state.props.rateCardConfig.incrementUnit)
  let
    state' = state { props { rateCardConfig { sliderVal = finalVal }, isRateCardLoading = true } }
  updateWithCmdAndExit state'
    [ do
        void $ updateSliderValue { sliderValue: finalVal, id: EHC.getNewIDWithTag "RateSlider" }
        pure $ NoAction
    ]
    $ UpdatePrice state' finalVal

eval EnterDestination state = do
  exit $ GoToEnterDestination state

eval OnNavigate state = do
  void $ pure $ openNavigation state.data.destinationLat state.data.destinationLng "TWOWHEELER"
  continue state

eval GoToProfile state = do
  exit $ GoToDriverProfile state

eval (TripStageTopBarAC (TripStageTopBar.HelpAndSupportScreen)) state = exit $ GoToHelpAndSupportScreen state

eval HideStopMeterRideConfirmCard state = do
  continue state { props { confirmMeterRideStop = false } }

eval ConfirmStopMeter state = do
  continue state { props { confirmMeterRideStop = true } }

eval (UpdateRidesInfo (RidesInfo resp)) state = do
  let
    { destAddr, lat, long } = case resp.toLocation of
      Nothing -> { destAddr: "", lat: 0.0, long: 0.0 }
      Just (LocationInfo location) -> { destAddr: decodeAddress (LocationInfo location) true, lat: location.lat, long: location.lon }
  exit $ TriggerGlobalEvents state { data { destinationAddress = destAddr, destinationLat = lat, destinationLng = long, ridesInfo = Just (RidesInfo resp) }, props { isMeterRideStarted = true, startButtonCountDown = 5, isMeterClockRunning = true } }

eval EndRide state = exit $ EndMeterRide state { props { confirmMeterRideStop = false } }

eval (DebounceCallBack _ _) state = updateAndExit state { props { isRateCardLoading = true } } $ UpdatePrice state state.props.rateCardConfig.sliderVal

eval (UpdateRateCard (RateCardRespItem res)) state = continue state { props { rateCardConfig { sliderFare = (round res.totalFare.amount), ratePerKM = (toNumber (round (res.perKmRate.amount * 10.0))) / 10.0 } } }

eval (UpdateFare (GetMeterPriceResp resp)) state = continue state { data { distance = fromMaybe 0.0 $ Number.fromString $ parseFloat (resp.distance / 1000.0) 2, lastUpdatedTime = getCurrentUTC "" }, props { meterFare = fromMaybe 0 $ fromString $ toInt $ resp.fare, rideStartingLoader = false} }

eval _ state = continue state
