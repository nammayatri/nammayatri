module Screens.TicketBookingScreen.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, map, unit, min, max, bind, ($), not, (+), (-), (==), (*), (<>), show, void, (+), (==), (-), show)
import PrestoDOM (Eval, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketBookingScreenState, TicketBookingScreenStage(..), TicketServiceI(..))
import Helpers.Utils (compareDate, getCurrentDate)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types (TicketBookingScreenState, TicketBookingItem(..), HomeScreenState, TicketServiceData, TicketServicePriceData, TicketOption)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (generateQR)
import Data.Array (length, (:), foldl, mapWithIndex, head, (!!))
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons(getNewIDWithTag)
import Resources.Constants
import Services.API (TicketPlaceResp(..), TicketServicesResponse(..), TicketServiceResp(..), TicketServicePrice(..))
import Data.Int (ceil)
import Common.Types.App as Common

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
            | IncrementTicket TicketOption
            | DecrementTicket TicketOption
            | DatePicker String String Int Int Int
            | ToggleTermsAndConditions
            | OpenCalendar 
            | NoAction
            | BackPressed
            | GetBookingInfo String
            | TicketQRRendered String String
            | IncrementSliderIndex
            | DecrementSliderIndex
            | PaymentStatusAction String
            | Copy
            | GoHome

data ScreenOutput = GoToHomeScreen TicketBookingScreenState
                  | GoToTicketPayment TicketBookingScreenState
                  | GoToGetBookingInfo TicketBookingScreenState
                  | TryAgain TicketBookingScreenState

eval :: Action -> TicketBookingScreenState -> Eval Action ScreenOutput TicketBookingScreenState
eval (ToggleTicketOption ticketID) state = do
  let updatedServicesInfo = map (updateExpandService ticketID) state.data.servicesInfo
  continue state { data { servicesInfo = updatedServicesInfo } }

eval (IncrementTicket ticketOption) state = do
  let {finalAmount, updateServicesInfo} = calculateTicketAmount ticketOption true state.data.servicesInfo
  continue state { data { totalAmount = finalAmount, servicesInfo = updateServicesInfo } }

eval (DecrementTicket ticketOption) state = do
  let {finalAmount, updateServicesInfo} = calculateTicketAmount ticketOption false state.data.servicesInfo
  continue state { data { totalAmount = finalAmount, servicesInfo = updateServicesInfo } }

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
    ChooseTicketStage -> updateAndExit state $ GoToTicketPayment state
    ViewTicketStage -> continue state{props{currentStage = ChooseTicketStage, showShimmer = false}}
    _ -> continue state

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (UpdatePlacesData placeData Nothing) state = do
  let newState = state { data { placeInfo = placeData }, props { showShimmer = false } }
  continue newState
eval (UpdatePlacesData placeData (Just (TicketServicesResponse serviceData))) state = do
  let servicesInfo = mapWithIndex (\i it -> transformRespToStateData (i==0) it) serviceData
  let newState = state { data { placeInfo = placeData, servicesInfo = servicesInfo}, props { showShimmer = false } }
  continue newState

eval BackPressed state = 
  case state.props.currentStage of 
    DescriptionStage -> exit $ GoToHomeScreen state {props {currentStage = DescriptionStage}}
    ChooseTicketStage -> continue state{props{currentStage = state.props.previousStage}}
    ViewTicketStage -> exit $ GoToHomeScreen state{props{currentStage = DescriptionStage, showShimmer = true}}
    TicketInfoStage -> continue state{props{currentStage = ViewTicketStage}}
    _ -> continue state

eval GoHome state = exit $ GoToHomeScreen state

eval (GetBookingInfo bookingShortId) state = do
  let newState = state { props { selectedBookingId = bookingShortId } }
  updateAndExit newState $ GoToGetBookingInfo newState

eval (ViewTicketAC (PrimaryButton.OnClick)) state = 
  case state.props.paymentStatus of
   Common.Success -> continueWithCmd state [do pure (GetBookingInfo state.props.selectedBookingId)]
   Common.Failed -> exit $ TryAgain state
   _ -> continue state

eval (PaymentStatusAction status) state =
  case status of 
    "Booked" -> continue state{props{paymentStatus = Common.Success}}
    "Failed" -> continue state{props{paymentStatus = Common.Failed}}
    _ -> continue state

eval _ state = continue state

transformRespToStateData :: Boolean -> TicketServiceResp -> TicketServiceData
transformRespToStateData isFirstElement (TicketServiceResp service) = do
  { id : service.id,
    service : service.service,
    openTimings : service.openTimings,
    closeTimings : service.closeTimings,
    isExpanded : isFirstElement,
    prices : map convertServicePriceRespToStateData service.prices
  }
  where
  convertServicePriceRespToStateData :: TicketServicePrice -> TicketServicePriceData
  convertServicePriceRespToStateData (TicketServicePrice price) = do
    { attendeeType : price.attendeeType,
      pricePerUnit : ceil price.pricePerUnit,
      currentValue : 0
    }

updateExpandService ticketID service =
  if service.id == ticketID then do
    service {isExpanded = not service.isExpanded}
  else service

calculateTicketAmount :: TicketOption -> Boolean -> Array TicketServiceData -> {finalAmount :: Int, updateServicesInfo :: Array TicketServiceData }
calculateTicketAmount ticketOption isIncrement servicesInfo = do
  let updatedServices = map (updateService ticketOption) servicesInfo
      finalAmount = sum (map calculateAmountService updatedServices)
  
  {finalAmount: finalAmount, updateServicesInfo: updatedServices}
  where
  updateService :: TicketOption -> TicketServiceData -> TicketServiceData
  updateService ticketOption service =
    if service.id == ticketOption.ticketID then do
      service {prices = map (updatePricePerUnit ticketOption) service.prices}
    else service
  
  updatePricePerUnit :: TicketOption -> TicketServicePriceData -> TicketServicePriceData
  updatePricePerUnit ticketOption price =
    if price.attendeeType == ticketOption.subcategory then do
      price { currentValue = min 99 (max 0 (if isIncrement then price.currentValue + 1 else price.currentValue - 1)) }
    else price

  calculateAmountService :: TicketServiceData -> Int
  calculateAmountService service =
    sum (map calculateAmountPrice service.prices)
  
  sum :: Array Int -> Int
  sum = foldl (\acc x -> acc + x) 0
  
  calculateAmountPrice :: TicketServicePriceData -> Int
  calculateAmountPrice price = price.currentValue * price.pricePerUnit
