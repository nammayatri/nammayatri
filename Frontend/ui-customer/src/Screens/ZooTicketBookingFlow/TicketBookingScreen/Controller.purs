module Screens.TicketBookingScreen.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, unit, bind, ($), not, (+), (-), (==), (*), (<>), show, (+), (==), (-), show)
import PrestoDOM (Eval, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketBookingScreenState, TicketBookingScreenStage(..))
import Helpers.Utils (compareDate, getCurrentDate)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types (TicketBookingScreenState, TicketBookingItem(..), HomeScreenState)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (generateQR)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons(getNewIDWithTag)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen TICKET_BOOKING_SCREEN)
    _ -> pure unit
    
data Action = AfterRender
            | GenericHeaderAC GenericHeader.Action 
            | PrimaryButtonAC PrimaryButton.Action
            | ToggleTicketOption String 
            | IncrementTicket String String 
            | DecrementTicket String String
            | DatePicker String String Int Int Int
            | ToggleTermsAndConditions
            | OpenCalendar 
            | NoAction
            | BackPressed
            | GetBookingInfo String
            | TicketQRRendered String String
            | IncrementSliderIndex
            | DecrementSliderIndex

data ScreenOutput = GoToHomeScreen
                  | GoToGetBookingInfo TicketBookingScreenState

eval :: Action -> TicketBookingScreenState -> Eval Action ScreenOutput TicketBookingScreenState
eval (ToggleTicketOption ticketID) state = 
  case ticketID of 
    "ZOO_ENTRY" -> continue state{data{zooEntry {availed = not (state.data.zooEntry.availed)}}}
    "AQUARIUM_ENTRY" -> continue state{data{aquariumEntry {availed = not (state.data.aquariumEntry.availed)}}}
    "PHOTO_OR_VIDEOGRAPHY" -> continue state{data{photoOrVideoGraphy {availed = not (state.data.photoOrVideoGraphy.availed)}}}
    _ -> continue state

eval (IncrementTicket ticketID subcategory) state = 
  case ticketID of 
    "ZOO_ENTRY" -> case subcategory of 
                      "ADULT" -> do 
                        let newState = state{data{zooEntry{adult = state.data.zooEntry.adult + 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      "CHILD" -> do 
                        let newState = state{data{zooEntry{child = state.data.zooEntry.child + 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    "AQUARIUM_ENTRY" -> case subcategory of 
                      "ADULT" -> do 
                        let newState = state{data{aquariumEntry{adult = state.data.aquariumEntry.adult + 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      "CHILD" -> do 
                        let newState = state{data{aquariumEntry{child = state.data.aquariumEntry.child + 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    "PHOTO_OR_VIDEOGRAPHY" -> case subcategory of 
                      "DEVICES" -> do 
                        let newState = state{data{photoOrVideoGraphy{noOfDevices = state.data.photoOrVideoGraphy.noOfDevices + 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    _ -> continue state

eval (DecrementTicket ticketID subcategory) state = 
  case ticketID of 
    "ZOO_ENTRY" -> case subcategory of 
                      "ADULT" -> do 
                        let newState = state{data{zooEntry{adult = if state.data.zooEntry.adult == 0 then 0 else state.data.zooEntry.adult - 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      "CHILD" -> do 
                        let newState = state{data{zooEntry{child = if state.data.zooEntry.child == 0 then 0 else state.data.zooEntry.child - 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    "AQUARIUM_ENTRY" -> case subcategory of 
                      "ADULT" -> do 
                        let newState = state{data{aquariumEntry{adult = if state.data.aquariumEntry.adult == 0 then 0 else state.data.aquariumEntry.adult - 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      "CHILD" -> do 
                        let newState = state{data{aquariumEntry{child = if state.data.aquariumEntry.child == 0 then 0 else state.data.aquariumEntry.child - 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    "PHOTO_OR_VIDEOGRAPHY" -> case subcategory of 
                      "DEVICES" -> do 
                        let newState = state{data{photoOrVideoGraphy{noOfDevices = if state.data.photoOrVideoGraphy.noOfDevices == 0 then 0 else state.data.photoOrVideoGraphy.noOfDevices - 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    _ -> continue state

eval  (DatePicker _ resp year month date ) state = do
  case resp of 
    "SELECTED" -> do 
      let validDate = (unsafePerformEffect $ runEffectFn2 compareDate ((show date) <> "/" <> (show (month+1)) <> "/" <> (show year)) (getCurrentDate "" ))
      continue state {props{validDate = validDate },data { dateOfVisit = (show date) <> "/" <> (show (month+1)) <> "/" <> (show year)}}
    _ -> continue state

eval ToggleTermsAndConditions state = continue state{props{termsAndConditionsSelected = not state.props.termsAndConditionsSelected}}

eval (PrimaryButtonAC (PrimaryButton.OnClick)) state = do 
  case state.props.currentStage of 
    DescriptionStage -> continue state{props{currentStage = ChooseTicketStage}}
    ViewTicketStage -> continue state{props{currentStage = ChooseTicketStage}}
    _ -> continue state

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval BackPressed state = 
  case spy "ZoneStage" state.props.currentStage of 
    DescriptionStage -> exit $ GoToHomeScreen 
    ChooseTicketStage -> continue state{props{currentStage = DescriptionStage}}
    ViewTicketStage -> exit $ GoToHomeScreen 
    TicketInfoStage -> updateAndExit state{props{currentStage = ViewTicketStage}} $ GoToGetBookingInfo state{props{currentStage = ViewTicketStage}} --continueWithCmd state{props{currentStage = ViewTicketStage}} [ do
    --   pure $ NoAction
    -- ]
    _ -> continue state


eval (GetBookingInfo bookingShortId) state = do
  let newState = state { props { selectedBookingId = bookingShortId , currentStage = TicketInfoStage} }
  -- continue newState
  updateAndExit newState $ GoToGetBookingInfo newState

eval (TicketQRRendered id text) state  = 
  continueWithCmd state [ do
    runEffectFn4 generateQR text id 200 0
    pure $ NoAction
  ]

eval IncrementSliderIndex state = do
  let len = length state.props.selectedBookingInfo.services
      activeItem = state.props.selectedBookingInfo.services !! (state.props.activeIndex + 1)
  case activeItem of
    Just item -> continueWithCmd state {props{leftButtonDisable = false, rightButtonDisable = (state.props.activeIndex + 1) == (len-1), activeListItem = item, activeIndex = state.props.activeIndex + 1}} [ do
      pure $ NoAction
      -- pure $ (TicketQRRendered (getNewIDWithTag "ticketQRView") item.ticketServiceShortId )
    ]
    Nothing -> continue state

eval DecrementSliderIndex state = do
  let len = length state.props.selectedBookingInfo.services
      activeItem = state.props.selectedBookingInfo.services !! (state.props.activeIndex - 1)
  case activeItem of
    Just item -> continueWithCmd state{props{rightButtonDisable = false, leftButtonDisable = (state.props.activeIndex - 1) == 0, activeListItem = item, activeIndex = state.props.activeIndex - 1}} [ do
      pure $ NoAction
      -- pure $ (TicketQRRendered (getNewIDWithTag "ticketQRView") item.ticketServiceShortId )     
    ]
    Nothing -> continue state

eval _ state = continue state


calculateTicketAmount :: TicketBookingScreenState -> Int
calculateTicketAmount state = 
  let zooEntryAmount = (state.data.zooEntry.adult * state.data.zooEntry.ticketPerAdult) + (state.data.zooEntry.child * state.data.zooEntry.ticketPerChild)
      aquariumEntryAmount = (state.data.aquariumEntry.adult * state.data.aquariumEntry.ticketPerAdult) + (state.data.aquariumEntry.child * state.data.aquariumEntry.ticketPerChild)
      photoOrVideoGraphyAmount = state.data.photoOrVideoGraphy.noOfDevices * state.data.photoOrVideoGraphy.ticketPerDevice
  in zooEntryAmount + aquariumEntryAmount + photoOrVideoGraphyAmount