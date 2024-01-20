{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RentalScreen.Controller where

import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.GenericHeader.Controller as GenericHeaderController
import Components.IncrementDecrementModel.Controller as IncrementDecrementModelController
import Components.InputView.Controller as InputViewController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array as DA
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Debug (spy)
import Effect.Aff (launchAff)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Helpers.Utils (getDateAfterNDaysv2, compareDate, getCurrentDatev2)
import JBridge (showDatePicker)
import Log (trackAppActionClick, trackAppEndScreen)
import ModifyScreenState (modifyScreenState)
import Prelude (class Eq, class Show, class Show, bind, map, pure, show, unit, (<), (>), (+), (-), ($), (<>), (&&), (*), (==))
import PrestoDOM (class Loggable, Eval, continue, continueWithCmd, exit)
import PrestoDOM.Core (getPushFn)
import Screens (getScreen, ScreenName(..))
import Screens.Types (RentalScreenStage(..), RentalScreenState)
import Types.App (ScreenType(..))

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

data ScreenOutput = NoScreen

data FareBreakupRowType = BookingTime | BookingDistance | BaseFare | TollFee

derive instance genericFareBreakupRowType :: Generic FareBreakupRowType _
instance showFareBreakupRowType :: Show FareBreakupRowType where show = genericShow
instance eqFareBreakupRowType :: Eq FareBreakupRowType where eq = genericEq

eval :: Action -> RentalScreenState -> Eval Action ScreenOutput RentalScreenState

eval (SliderCallback value) state = continue state{data{sliderVal = value}}

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = 
  case state.data.currentStage of
    RENTAL_SELECT_PACKAGE -> continue state { data { currentStage = RENTAL_SELECT_VARIANT }}
    RENTAL_SELECT_VARIANT -> continue state { data { currentStage = RENTAL_CONFIRMATION }}
    RENTAL_CONFIRMATION -> exit NoScreen

eval (DurationIncrementDecrementAC (IncrementDecrementModelController.OnIncrement)) state =
  let toUpdate = (state.data.rentalBookingData.baseDuration < state.props.maxDuration)
      updatedDuration = if toUpdate then state.data.rentalBookingData.baseDuration + 1 else state.data.rentalBookingData.baseDuration
      updatedDistance = if toUpdate then updatedDuration * 10 else state.data.rentalBookingData.baseDuration
  in continue state { data { rentalBookingData { baseDuration = updatedDuration , baseDistance = updatedDistance }}}

eval (DurationIncrementDecrementAC (IncrementDecrementModelController.OnDecrement)) state =
  let toUpdate = (state.data.rentalBookingData.baseDuration > state.props.minDuration)
      updatedDuration = if toUpdate then state.data.rentalBookingData.baseDuration - 1 else state.data.rentalBookingData.baseDuration
      updatedDistance = if toUpdate then updatedDuration * 10 else state.data.rentalBookingData.baseDuration
  in continue state { data { rentalBookingData { baseDuration = updatedDuration, baseDistance = updatedDistance }}}

eval (DistanceIncrementDecrementAC (IncrementDecrementModelController.OnIncrement)) state =
  let updatedDistance = if(state.data.rentalBookingData.baseDistance < state.props.maxDistance) then state.data.rentalBookingData.baseDistance + 10 else state.data.rentalBookingData.baseDistance
  in continue state { data { rentalBookingData { baseDistance = updatedDistance }}}

eval (DistanceIncrementDecrementAC (IncrementDecrementModelController.OnDecrement)) state =
  let updatedDistance = if(state.data.rentalBookingData.baseDistance > state.props.minDistance) then state.data.rentalBookingData.baseDistance - 10 else state.data.rentalBookingData.baseDistance
  in continue state { data { rentalBookingData { baseDistance = updatedDistance }}}

eval (GenericHeaderAC GenericHeaderController.PrefixImgOnClick) state = genericBackPressed state

eval (ChooseVehicleAC (ChooseVehicleController.OnSelect variant)) state =
  let updatedQuotes = map (\item -> item {activeIndex = variant.index}) state.data.quoteList
  in  continue state { data { quoteList = updatedQuotes }}

eval (DateTimePickerAction dateResp year month day timeResp hour minute) state =
  let selectedDateString = (show year) <> "-" <> (if (month + 1 < 10) then "0" else "") <> (show (month+1)) <> "-" <> (if day < 10 then "0"  else "") <> (show day)
      validDate = (unsafePerformEffect $ runEffectFn2 compareDate (getDateAfterNDaysv2 (state.props.maxDateBooking)) selectedDateString)
                      && (unsafePerformEffect $ runEffectFn2 compareDate selectedDateString (getCurrentDatev2 "" ))
      updatedDateTime = state.data.selectedDateTimeConfig { year = year, month = month, day = day, hour = hour, minute = minute }
      newState = if validDate then state { data { selectedDateTimeConfig = updatedDateTime }} else state
  in if DA.all (_ == "SELECTED") [dateResp, timeResp] then continue newState else continue state

eval (InputViewAC (InputViewController.BackPressed)) state = genericBackPressed state

eval (InputViewAC (InputViewController.DateTimePickerButtonClicked)) state = openDatePicker state

eval (InputViewAC (InputViewController.TextFieldFocusChanged id isFocused)) state = do
  case state.data.currentStage of
    RENTAL_SELECT_PACKAGE -> continue state
    RENTAL_SELECT_VARIANT -> 
      if (id == "\"DateAndTime\"") then openDatePicker state
      else genericBackPressed state
    _ -> continue state


eval _ state = continue state


genericBackPressed :: RentalScreenState -> Eval Action ScreenOutput RentalScreenState
genericBackPressed state = case state.data.currentStage of
  RENTAL_SELECT_PACKAGE -> exit NoScreen
  RENTAL_SELECT_VARIANT -> continue state { data { currentStage = RENTAL_SELECT_PACKAGE }}
  RENTAL_CONFIRMATION -> continue state { data { currentStage = RENTAL_SELECT_VARIANT }}

openDatePicker :: RentalScreenState -> Eval Action ScreenOutput RentalScreenState
openDatePicker state =
  continueWithCmd state
    [ do 
      push <- getPushFn Nothing "RentalScreen"
      _ <- launchAff $ showDatePicker push DateTimePickerAction
      pure NoAction
    ]