module Screens.TicketBookingScreen.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, unit, bind, ($), not, (+), (-), (==), (*), (<>), show, void, (+), (==), (-), show)
import PrestoDOM (Eval, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketBookingScreenState, TicketBookingScreenStage(..), TicketServiceI(..))
import Helpers.Utils (compareDate, getCurrentDate)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types (TicketBookingScreenState, TicketBookingItem(..), HomeScreenState)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (generateQR)
import Data.Array (length, head, (!!))
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons(getNewIDWithTag)
import Resources.Constants
import Services.API (TicketPlaceResp(..), TicketServicesResponse(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen TICKET_BOOKING_SCREEN)
    _ -> pure unit
    
data Action = AfterRender
            | UpdatePlacesData (Maybe TicketPlaceResp) (Maybe TicketServicesResponse)
            | GenericHeaderAC GenericHeader.Action 
            | PrimaryButtonAC PrimaryButton.Action
            | ShareTicketAC PrimaryButton.Action
            | ViewTicketAC PrimaryButton.Action
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
            | Copy

data ScreenOutput = GoToHomeScreen TicketBookingScreenState | GoToTicketPayment TicketBookingScreenState

-- updateTicketService :: String -> Int -> Array TicketServiceI -> Array TicketServiceI -- TODO:: Use similar helper function here
-- updateTicketService serviceId deltaUnits services =
--   let
--     updateFunc :: Maybe TicketServiceI -> Maybe TicketServiceI
--     updateFunc service =
--       case service of
--         Just s -> Just { id: s.id, attendeeType: s.attendeeType, numberOfUnits: s.numberOfUnits + deltaUnits }
--         Nothing -> Nothing
--   in
--     fromMaybe services $ updateAt updateFunc serviceId services
                  | GoToGetBookingInfo TicketBookingScreenState

eval :: Action -> TicketBookingScreenState -> Eval Action ScreenOutput TicketBookingScreenState
eval (ToggleTicketOption ticketID) state =
  case ticketID of 
    "b73378dc-427f-4efa-9b55-8efe7e3352c2" -> continue state{data{zooEntry {availed = not (state.data.zooEntry.availed)}}}
    "a7eba6ed-99f7-442f-a9d8-00c8b380657b" -> continue state{data{aquariumEntry {availed = not (state.data.aquariumEntry.availed)}}}
    "d8f47b42-50a5-4a97-8dda-e80a3633d7ab" -> continue state{data{photoOrVideoGraphy {availed = not (state.data.photoOrVideoGraphy.availed)}}}
    _ -> continue state

eval (IncrementTicket ticketID subcategory) state =  -- TODO:: Refactor this function with generic one
  case ticketID of 
    "b73378dc-427f-4efa-9b55-8efe7e3352c2" -> case subcategory of 
                      "ADULT" -> do 
                        let newState = state{data{zooEntry{adult = state.data.zooEntry.adult + 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      "CHILD" -> do 
                        let newState = state{data{zooEntry{child = state.data.zooEntry.child + 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    "a7eba6ed-99f7-442f-a9d8-00c8b380657b" -> case subcategory of 
                      "ADULT" -> do 
                        let newState = state{data{aquariumEntry{adult = state.data.aquariumEntry.adult + 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      "CHILD" -> do 
                        let newState = state{data{aquariumEntry{child = state.data.aquariumEntry.child + 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    "d8f47b42-50a5-4a97-8dda-e80a3633d7ab" -> case subcategory of 
                      "DEVICES" -> do 
                        let newState = state{data{photoOrVideoGraphy{noOfDevices = state.data.photoOrVideoGraphy.noOfDevices + 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    _ -> continue state

eval (DecrementTicket ticketID subcategory) state = 
  case ticketID of 
    "b73378dc-427f-4efa-9b55-8efe7e3352c2" -> case subcategory of 
                      "ADULT" -> do 
                        let newState = state{data{zooEntry{adult = if state.data.zooEntry.adult == 0 then 0 else state.data.zooEntry.adult - 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      "CHILD" -> do 
                        let newState = state{data{zooEntry{child = if state.data.zooEntry.child == 0 then 0 else state.data.zooEntry.child - 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    "a7eba6ed-99f7-442f-a9d8-00c8b380657b" -> case subcategory of 
                      "ADULT" -> do 
                        let newState = state{data{aquariumEntry{adult = if state.data.aquariumEntry.adult == 0 then 0 else state.data.aquariumEntry.adult - 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      "CHILD" -> do 
                        let newState = state{data{aquariumEntry{child = if state.data.aquariumEntry.child == 0 then 0 else state.data.aquariumEntry.child - 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    "d8f47b42-50a5-4a97-8dda-e80a3633d7ab" -> case subcategory of 
                      "DEVICES" -> do 
                        let newState = state{data{photoOrVideoGraphy{noOfDevices = if state.data.photoOrVideoGraphy.noOfDevices == 0 then 0 else state.data.photoOrVideoGraphy.noOfDevices - 1}}}
                        continue newState{data{totalAmount = calculateTicketAmount newState}}
                      _ -> continue state 
    _ -> continue state

eval  (DatePicker _ resp year month date ) state = do
  case resp of 
    "SELECTED" -> do 
      let validDate = (unsafePerformEffect $ runEffectFn2 compareDate ((show date) <> "/" <> (show (month+1)) <> "/" <> (show year)) (getCurrentDate "" ))
      continue state {props{validDate = validDate },data { dateOfVisit = (show year) <> "-" <> (show (month+1)) <> "-" <> (show date) }}
    _ -> continue state

eval ToggleTermsAndConditions state = continue state{props{termsAndConditionsSelected = not state.props.termsAndConditionsSelected}}

eval (PrimaryButtonAC (PrimaryButton.OnClick)) state = do 
  case state.props.currentStage of 
    DescriptionStage -> continue state{props{currentStage = ChooseTicketStage}}
    ChooseTicketStage -> exit $ GoToTicketPayment state
    ViewTicketStage -> continue state{props{currentStage = ChooseTicketStage}}
    _ -> continue state

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (UpdatePlacesData placeData serviceData) state = do
  let newState = state { data { placeInfo = placeData,  servicesInfo = serviceData}, props { showShimmer = false } }
  continue newState


eval BackPressed state = 
  case state.props.currentStage of 
    DescriptionStage -> exit $ GoToHomeScreen state {props {currentStage = DescriptionStage}}
    ChooseTicketStage -> continue state{props{currentStage = DescriptionStage}}
    ViewTicketStage -> exit $ GoToHomeScreen state{props{currentStage = DescriptionStage}}
    TicketInfoStage -> continue state{props{currentStage = ViewTicketStage}}
    _ -> continue state


eval (GetBookingInfo bookingShortId) state = do
  let newState = state { props { selectedBookingId = bookingShortId } }
  updateAndExit newState $ GoToGetBookingInfo newState

eval _ state = continue state


calculateTicketAmount :: TicketBookingScreenState -> Int
calculateTicketAmount state = 
  let zooEntryAmount = (state.data.zooEntry.adult * state.data.zooEntry.ticketPerAdult) + (state.data.zooEntry.child * state.data.zooEntry.ticketPerChild)
      aquariumEntryAmount = (state.data.aquariumEntry.adult * state.data.aquariumEntry.ticketPerAdult) + (state.data.aquariumEntry.child * state.data.aquariumEntry.ticketPerChild)
      photoOrVideoGraphyAmount = state.data.photoOrVideoGraphy.noOfDevices * state.data.photoOrVideoGraphy.ticketPerDevice
  in zooEntryAmount + aquariumEntryAmount + photoOrVideoGraphyAmount