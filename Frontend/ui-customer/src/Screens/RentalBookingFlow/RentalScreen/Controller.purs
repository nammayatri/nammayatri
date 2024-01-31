{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RentalScreen.Controller
  ( Action(..)
  , FareBreakupRowType(..)
  , ScreenOutput(..)
  , eval
  )
  where

import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.GenericHeader.Controller as GenericHeaderController
import Components.IncrementDecrementModel.Controller as IncrementDecrementModelController
import Components.InputView.Controller as InputViewController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.RateCard.Controller as RateCardController
import Data.Array as DA
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Aff (launchAff)
import Effect.Uncurried (runEffectFn2, runEffectFn6)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons as EHC
import Helpers.Utils (getDateAfterNDaysv2, compareDate, getCurrentDatev2)
import JBridge (showDateTimePicker)
import Log (trackAppActionClick)
import Prelude (class Eq, class Show, bind, map, negate, pure, show, unit, ($), (&&), (*), (+), (<), (<>), (==), (>), (/=))
import PrestoDOM (class Loggable, Eval, continue, continueWithCmd, exit)
import PrestoDOM.Core (getPushFn)
import Screens (getScreen, ScreenName(..))
import Screens.Types (RentalScreenStage(..), RentalScreenState)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    GenericHeaderAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "BackClicked" "on_click"
    DurationIncrementDecrementAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "DurationIncrementDecrement" "on_click"
    DistanceIncrementDecrementAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "DistanceIncrementDecrement" "on_click"
    ChooseVehicleAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "ChooseVehicle" "on_click"
    DateTimePickerAction _ _ _ _ _ _ _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "DateTimePicker" "on_click"
    InputViewAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "InputView" "on_click"
    _ -> pure unit

data Action = 
    NoAction 
  | PrimaryButtonActionController PrimaryButtonController.Action
  | GenericHeaderAC GenericHeaderController.Action
  | DurationIncrementDecrementAC IncrementDecrementModelController.Action
  | DistanceIncrementDecrementAC IncrementDecrementModelController.Action
  | ChooseVehicleAC ChooseVehicleController.Action
  | DateTimePickerAction String Int Int Int String Int Int
  | InputViewAC InputViewController.Action
  | SliderCallback Int
  | RateCardAC RateCardController.Action

data ScreenOutput = NoScreen
                  | GoToHomeScreen
                  | SearchLocationForRentals RentalScreenState String
                  | GoToRideScheduledScreen RentalScreenState
                  
data FareBreakupRowType = BookingTime | BookingDistance | BaseFare | TollFee

derive instance genericFareBreakupRowType :: Generic FareBreakupRowType _
instance showFareBreakupRowType :: Show FareBreakupRowType where show = genericShow
instance eqFareBreakupRowType :: Eq FareBreakupRowType where eq = genericEq

eval :: Action -> RentalScreenState -> Eval Action ScreenOutput RentalScreenState

eval (SliderCallback value) state = 
  let minDistance = value * 10
      maxDistance = minDistance + 30
  in continue state{data{rentalBookingData{baseDuration = value, baseDistance = minDistance}}, props{minDistance = minDistance, maxDistance = maxDistance}}

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = 
  case state.data.currentStage of
    RENTAL_SELECT_PACKAGE -> continue state { data { currentStage = RENTAL_SELECT_VARIANT }}
    RENTAL_SELECT_VARIANT -> continue state { data { currentStage = RENTAL_CONFIRMATION }}
    RENTAL_CONFIRMATION -> exit $ GoToRideScheduledScreen state

eval (DurationIncrementDecrementAC (IncrementDecrementModelController.OnIncrement)) state = 
  continue $ incrementDecrementDuration true state

eval (DurationIncrementDecrementAC (IncrementDecrementModelController.OnDecrement)) state = 
  continue $ incrementDecrementDuration false state

eval (DistanceIncrementDecrementAC (IncrementDecrementModelController.OnIncrement)) state =
  continue $ incrementDecrementDistance true state 

eval (DistanceIncrementDecrementAC (IncrementDecrementModelController.OnDecrement)) state =
  continue $ incrementDecrementDistance false state

eval (GenericHeaderAC GenericHeaderController.PrefixImgOnClick) state = genericBackPressed state

eval (ChooseVehicleAC (ChooseVehicleController.OnSelect variant)) state =
  let updatedQuotes = map (\item -> item {activeIndex = variant.index}) state.data.quoteList
  in  continue state { data { quoteList = updatedQuotes }}

eval (ChooseVehicleAC (ChooseVehicleController.ShowRateCard _)) state = 
  continue state { props { showRateCard = true }}

eval (DateTimePickerAction dateResp year month day timeResp hour minute) state =
  if DA.all (_ /= "SELECTED") [dateResp, timeResp] then continue state 
  else
    let selectedDateString = (show year) <> "-" <> (if (month + 1 < 10) then "0" else "") <> (show (month+1)) <> "-" <> (if day < 10 then "0"  else "") <> (show day)
        validDate = (unsafePerformEffect $ runEffectFn2 compareDate (getDateAfterNDaysv2 (state.props.maxDateBooking)) selectedDateString)
                        && (unsafePerformEffect $ runEffectFn2 compareDate selectedDateString (getCurrentDatev2 "" ))
        updatedDateTime = state.data.selectedDateTimeConfig { year = year, month = month, day = day, hour = hour, minute = minute }
        selectedUTC = unsafePerformEffect $ EHC.convertDateTimeConfigToUTC year (month + 1) day hour minute 0
        newState = if validDate then state { data { selectedDateTimeConfig = updatedDateTime, startTimeUTC = selectedUTC}} else state
    in continue newState

eval (InputViewAC (InputViewController.BackPressed)) state = genericBackPressed state

eval (InputViewAC (InputViewController.DateTimePickerButtonClicked)) state = openDateTimePicker state

eval (InputViewAC (InputViewController.TextFieldFocusChanged id isFocused hasFocus)) state = do
  case state.data.currentStage of
    RENTAL_SELECT_PACKAGE -> exit $ SearchLocationForRentals state id
    RENTAL_SELECT_VARIANT -> 
      if (id == "DateAndTime") then openDateTimePicker state
      else genericBackPressed state
    _ -> continue state

eval (RateCardAC action) state =
  case action of
    RateCardController.NoAction -> continue state
    RateCardController.PrimaryButtonAC (PrimaryButtonController.NoAction) -> continue state
    _ -> continue state { props {showRateCard = false}}

eval _ state = continue state


genericBackPressed :: RentalScreenState -> Eval Action ScreenOutput RentalScreenState
genericBackPressed state = case state.data.currentStage of
  RENTAL_SELECT_PACKAGE -> exit GoToHomeScreen
  RENTAL_SELECT_VARIANT -> continue state { data { currentStage = RENTAL_SELECT_PACKAGE }}
  RENTAL_CONFIRMATION -> continue state { data { currentStage = RENTAL_SELECT_VARIANT }}

openDateTimePicker :: RentalScreenState -> Eval Action ScreenOutput RentalScreenState
openDateTimePicker state =
  continueWithCmd state
    [ do 
      push <- getPushFn Nothing "RentalScreen"
      _ <- launchAff $ showDateTimePicker push DateTimePickerAction
      pure NoAction
    ]

incrementDecrementDuration :: Boolean -> RentalScreenState -> RentalScreenState
incrementDecrementDuration isIncrement state = 
  let initialDuration = state.data.rentalBookingData.baseDuration
      toUpdate = if isIncrement then (initialDuration < state.props.maxDuration) else (initialDuration > state.props.minDuration)
      updatedDuration = if toUpdate then initialDuration + (if isIncrement then 1 else (negate 1)) else initialDuration
      updatedDistance = if toUpdate then updatedDuration * 10 else state.data.rentalBookingData.baseDistance
  in state { data { rentalBookingData { baseDuration = updatedDuration, baseDistance = updatedDistance }}}

incrementDecrementDistance :: Boolean -> RentalScreenState -> RentalScreenState
incrementDecrementDistance isIncrement state = 
  let initialDistance = state.data.rentalBookingData.baseDistance
      toUpdate = if isIncrement then (initialDistance < state.props.maxDistance) else (initialDistance > state.props.minDistance)
      updatedDistance = if toUpdate then initialDistance + (if isIncrement then 5 else (negate 5)) else initialDistance
  in state { data { rentalBookingData { baseDistance = updatedDistance }}}