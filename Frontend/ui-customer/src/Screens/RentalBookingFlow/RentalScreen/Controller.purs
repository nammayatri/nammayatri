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
  , DescriptionType(..)
  , ScreenOutput(..)
  , eval
  , dummyRentalQuote
  )
  where

import Common.Types.App (LazyCheck(..))
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.GenericHeader.Controller as GenericHeaderController
import Components.IncrementDecrementModel.Controller as IncrementDecrementModelController
import Components.InputView.Controller as InputViewController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PopUpModal.Controller as PopUpModalController
import Components.RateCard.Controller as RateCardController
import Components.RequestInfoCard.Controller as RequestInfoCardController
import Screens.HomeScreen.ScreenData as HomeScreenData
import Data.Array as DA
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Show.Generic (genericShow)
import Effect.Aff (launchAff)
import Effect.Uncurried (runEffectFn2, runEffectFn6)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons as EHC
import Helpers.Utils (getDateAfterNDaysv2, getCurrentDatev2, decodeBookingTimeList, encodeBookingTimeList, invalidBookingTime, rideStartingInBetweenPrevRide,overlappingRides)
import Screens.HomeScreen.Transformer (getQuotesTransformer, getFilteredQuotes, transformQuote)
import JBridge (showDateTimePicker, updateSliderValue)
import Engineering.Helpers.Utils as EHU
import Language.Strings (getVarString)
import Language.Types (STR(..)) as STR
import Log (trackAppActionClick)
import Prelude (class Eq, class Show, bind, map, negate, pure, show, unit, ($), (&&), (*), (+), (<), (<>), (==), (>), (/=), (<=), (>=), discard, void, (||), not, min, (-), max, compare, Ordering(..), when, (/))
import PrestoDOM (class Loggable, Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Core (getPushFn)
import Screens (getScreen, ScreenName(..))
import Screens.Types (RentalScreenStage(..), RentalScreenState, BookingTime)
import Services.API (GetQuotesRes(..), RideBookingRes(..), OfferRes(..), QuoteAPIEntity(..), RentalQuoteAPIDetails(..))
import Data.Number (fromString)
import Data.Lens
import Accessor
import Data.Maybe (fromMaybe)
import Data.Int as INT
import Language.Strings (getString)
import Language.Types (STR(..)) as STR
import Data.String as DS
import Services.FlowCache as FlowCache
import Components.PopUpModal.Controller as PopUpModal
import Data.Function.Uncurried (runFn2)
import Helpers.Utils (isParentView, emitTerminateApp)
import Common.Types.App (LazyCheck(..))
import Engineering.Helpers.Utils as EHC

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    GenericHeaderAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "BackClicked" "on_click"
    DurationIncrementDecrementAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "DurationIncrementDecrement" "on_click"
    DistanceIncrementDecrementAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "DistanceIncrementDecrement" "on_click"
    DateTimePickerAction _ _ _ _ _ _ _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "DateTimePicker" "on_click"
    InputViewAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "InputView" "on_click"
    _ -> pure unit

data Action = 
    NoAction 
  | PrimaryButtonActionController PrimaryButtonController.Action
  | GenericHeaderAC GenericHeaderController.Action
  | DurationIncrementDecrementAC IncrementDecrementModelController.Action
  | DistanceIncrementDecrementAC IncrementDecrementModelController.Action
  | DateTimePickerAction String Int Int Int String Int Int
  | InputViewAC InputViewController.Action
  | SliderCallback Int
  | RateCardAC RateCardController.Action
  | BackpressAction
  | RentalPolicyInfo
  | GetRideConfirmation RideBookingRes
  | UpdateLocAndLatLong String String
  | PopUpModalAC PopUpModalController.Action
  | RequestInfoCardAction RequestInfoCardController.Action
  | UpdateSliderValue Int
  | ScheduledRideExistsAction PopUpModal.Action

data ScreenOutput = NoScreen
                  | GoToHomeScreen RentalScreenState (Maybe BookingTime)
                  | SearchLocationForRentals RentalScreenState String
                  | GoToRideScheduledScreen RentalScreenState
                  | OnRentalRideConfirm RentalScreenState
                  | DoRentalSearch RentalScreenState
                  | UpdateQuoteList RentalScreenState
                  | GoToSelectPackage RentalScreenState
                  | GoToSelectVariant RentalScreenState
                  
data FareBreakupRowType = BookingFrom | BookingTime | BookingDistance | BaseFare | TollFee | ParkingCharges | NightTimeFee

derive instance genericFareBreakupRowType :: Generic FareBreakupRowType _
instance showFareBreakupRowType :: Show FareBreakupRowType where show = genericShow
instance eqFareBreakupRowType :: Eq FareBreakupRowType where eq = genericEq

data DescriptionType = BookingTimeAndDist | EstimatedCharges | AdditionalCharges 

derive instance genericDescriptionType :: Generic DescriptionType _
instance showDescriptionType :: Show DescriptionType where show = genericShow
instance eqDescriptionType :: Eq DescriptionType where eq = genericEq

eval :: Action -> RentalScreenState -> Eval Action ScreenOutput RentalScreenState

eval (PopUpModalAC (PopUpModalController.OnButton2Click)) state = continue state {props{showPopUpModal = false}}

eval RentalPolicyInfo state = continue state { props { showRentalPolicy = true}}

eval (RequestInfoCardAction (RequestInfoCardController.Close)) state = continue state {props { showRentalPolicy = false}}

eval (RequestInfoCardAction (RequestInfoCardController.BackPressed)) state = continue state {props { showRentalPolicy = false}}

eval (UpdateLocAndLatLong lat lon) state =
  continue state{data{pickUpLoc{lat = fromString lat, lon = fromString lon}}}

eval BackpressAction state = genericBackPressed state 

eval (SliderCallback hours) state = 
  let minDistance = hours * 10
      maxDistance = minDistance + (min 50 $ min (hours * 10) $ 120 - (hours * 10))
  in continue state{data{rentalBookingData{baseDuration = hours, baseDistance = minDistance}}, props{minDistance = minDistance, maxDistance = maxDistance}}

eval (UpdateSliderValue value) state = do 
  continueWithCmd state [do 
    let isSliderValueValid = value <= state.props.maxDuration && value >= state.props.minDuration
    if isSliderValueValid then do 
      void $ updateSliderValue {sliderValue : value, id : (EHC.getNewIDWithTag "DurationSliderView")}
      pure $ SliderCallback value 
      else pure $ NoAction
    ]

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = 
  case state.data.currentStage of
    RENTAL_SELECT_PACKAGE -> do
      let maybeOverLappingBookingDetails = invalidBookingTime state.data.startTimeUTC $ Just $ state.data.rentalBookingData.baseDuration * 60
      if (isJust maybeOverLappingBookingDetails) then exit $ GoToHomeScreen state maybeOverLappingBookingDetails
      else updateAndExit state{props{showPrimaryButton = false}} $ DoRentalSearch state{props{showPrimaryButton = false}}
    RENTAL_CONFIRMATION -> exit $ OnRentalRideConfirm state
    _ -> continue state
  where
    rideScheduledStartingOrEndingTime :: BookingTime -> String
    rideScheduledStartingOrEndingTime bookingDetails = 
      let diffInMins = (EHC.compareUTCDate bookingDetails.rideStartTime state.data.startTimeUTC) / 60
      in
        if rideStartingInBetweenPrevRide diffInMins bookingDetails (state.data.rentalBookingData.baseDuration * 60)
          then EHC.getUTCAfterNSeconds bookingDetails.rideStartTime $ (bookingDetails.estimatedDuration - 30) * 60
        else bookingDetails.rideStartTime

    formatTimeInHHMM :: String -> String
    formatTimeInHHMM rideTime = EHC.convertUTCtoISC rideTime "hh" <> ":" <> EHC.convertUTCtoISC rideTime "mm"


eval (DurationIncrementDecrementAC (IncrementDecrementModelController.OnIncrement)) state = 
  continue $ incrementDecrementDuration true state

eval (DurationIncrementDecrementAC (IncrementDecrementModelController.OnDecrement)) state = 
  continue $ incrementDecrementDuration false state

eval (DistanceIncrementDecrementAC (IncrementDecrementModelController.OnIncrement)) state =
  continue $ incrementDecrementDistance true state 

eval (DistanceIncrementDecrementAC (IncrementDecrementModelController.OnDecrement)) state =
  continue $ incrementDecrementDistance false state

eval (GenericHeaderAC GenericHeaderController.PrefixImgOnClick) state = genericBackPressed state

eval (DateTimePickerAction dateResp year month day timeResp hour minute) state =
  if DA.any (_ /= "SELECTED") [dateResp, timeResp] then continue state 
  else
    let selectedDateString = (show year) <> "-" <> (if (month + 1 < 10) then "0" else "") <> (show (month+1)) <> "-" <> (if day < 10 then "0"  else "") <> (show day)
        selectedUTC = unsafePerformEffect $ EHC.convertDateTimeConfigToUTC year (month + 1) day hour minute 0
        isAfterThirtyMinutes = (EHC.compareUTCDate selectedUTC (EHC.getCurrentUTC "")) > (30 * 60)
        validDate = (unsafePerformEffect $ runEffectFn2 EHC.compareDate (getDateAfterNDaysv2 (state.props.maxDateBooking)) selectedDateString)
                        && (unsafePerformEffect $ runEffectFn2 EHC.compareDate selectedDateString (getCurrentDatev2 "" ))
        updatedDateTime = state.data.selectedDateTimeConfig { year = year, month = month, day = day, hour = hour, minute = minute }
        newState = if validDate && isAfterThirtyMinutes then state { data { selectedDateTimeConfig = updatedDateTime, startTimeUTC = selectedUTC}} else state
        returnTimeUTC = EHC.getUTCAfterNSeconds selectedUTC (state.data.rentalBookingData.baseDuration * 60 * 60)
    in if validDate && isAfterThirtyMinutes then do 
      let latestScheduledRides = state.data.latestScheduledRides
          {overLapping,overLappedBooking} = overlappingRides selectedUTC (Just returnTimeUTC) 1800 latestScheduledRides
      if overLapping then do
          continue state {data{overLappingBooking =overLappedBooking}, props{showScheduledRideExistsPopUp = true}}
      else continue newState {props{showPrimaryButton = true}}
       else 
        if validDate then do 
          void $ pure $ EHU.showToast $ getString STR.SCHEDULE_RIDE_AVAILABLE
          continue state {props{showPrimaryButton = true}}
        else do
          void $ pure $ EHU.showToast $ getVarString STR.DATE_INVALID_MESSAGE $ DA.singleton $ show state.props.maxDateBooking
          continue state {props{showPrimaryButton = true}}

eval (InputViewAC (InputViewController.BackPressed)) state = genericBackPressed state

eval (InputViewAC (InputViewController.DateTimePickerButtonClicked)) state = openDateTimePicker state

eval (InputViewAC (InputViewController.TextFieldFocusChanged id isFocused hasFocus)) state = do
  case state.data.currentStage of
    RENTAL_SELECT_PACKAGE -> exit $ SearchLocationForRentals state id
    RENTAL_SELECT_VARIANT -> 
      if (id == "DateAndTime") then continueWithCmd state{data{currentStage = RENTAL_SELECT_PACKAGE, rentalsQuoteList = []}, props{showPrimaryButton = true}} 
        [ do 
          push <- getPushFn Nothing "RentalScreen"
          _ <- launchAff $ showDateTimePicker push DateTimePickerAction (Just $ EHC.getCurrentUTC "") Nothing true true
          pure NoAction
        ]
      else genericBackPressed state {props{showPrimaryButton = true}}
    _ -> continue state

eval (RateCardAC action) state =
  case action of
    RateCardController.NoAction -> continue state
    RateCardController.PrimaryButtonAC (PrimaryButtonController.NoAction) -> continue state
    _ -> continue state { props {showRateCard = false}}

eval (ScheduledRideExistsAction (PopUpModal.OnButton2Click)) state = continue state{data{ startTimeUTC = "",overLappingBooking= Nothing}, props{showScheduledRideExistsPopUp = false}}

eval _ state = continue state


genericBackPressed :: RentalScreenState -> Eval Action ScreenOutput RentalScreenState
genericBackPressed state = case state.data.currentStage of
  RENTAL_SELECT_PACKAGE -> do 
    if state.props.showRentalPolicy then continue state { props {showRentalPolicy = false}}
    else if isParentView FunctionCall 
      then do 
        void $ pure $ emitTerminateApp Nothing true
        continue state
      else exit $ GoToHomeScreen state Nothing
  RENTAL_SELECT_VARIANT -> do 
    if state.props.showRateCard then continue state { props {showRateCard = false}}
    else exit $ GoToSelectPackage state { data { currentStage = RENTAL_SELECT_PACKAGE, rentalsQuoteList = []}, props { showPrimaryButton = true}}
  RENTAL_CONFIRMATION -> exit $ GoToSelectVariant state
  _ -> continue state

openDateTimePicker :: RentalScreenState -> Eval Action ScreenOutput RentalScreenState
openDateTimePicker state = do 
  let 
    scheduledUTC = runFn2 EHC.getUTCAfterNSecondsImpl (EHC.getCurrentUTC "") 1860
  continueWithCmd state
    [ do 
      push <- getPushFn Nothing "RentalScreen"
      let maxDate = Just $ EHC.getUTCAfterNHours (EHC.getCurrentUTC "") 120
      _ <- launchAff $ showDateTimePicker push DateTimePickerAction (Just scheduledUTC) maxDate true true
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

dummyFareQuoteDetails = {
  baseFare : 0 ,
  includedKmPerHr : 0 ,
  perExtraKmRate : 0 ,
  perExtraMinRate : 0 ,
  perHourCharge : 0 ,
  plannedPerKmRate : 0,
  nightShiftCharge : 0,
  tollCharges : Nothing,
  deadKmFare : Nothing
}

dummyRentalQuote = {
  quoteDetails : ChooseVehicleController.config ,
  index : 0 ,
  activeIndex : 0 ,
  fareDetails : dummyFareQuoteDetails
}