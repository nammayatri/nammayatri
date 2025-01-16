module Screens.TicketBookingFlow.TicketStatus.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, map, unit, min, max, bind, ($), not, (+), (-), (==), (*), (<>), show, void, (+), (==), (-), show, (&&), (>), (/=), (||), (<=), (>=), (<))
import PrestoDOM (Eval, update, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketBookingScreenState, TicketBookingScreenStage(..), TicketServiceI(..))
import Helpers.Utils (getDateAfterNDaysv2, getCurrentDatev2)
import Engineering.Helpers.Utils(compareDate)
import Effect.Uncurried (runEffectFn2)
import Common.Types.App (LazyCheck(..))
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types (TimeInterval, TicketBookingScreenState, TicketBookingItem(..), HomeScreenState, SlotInterval(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (emitTerminateApp, isParentView)
import Helpers.Utils (generateQR)
import Data.Array (length, (:), foldl, mapWithIndex, head, (!!), filter, elem, groupBy, find)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Engineering.Helpers.Commons(convertUTCTimeToISTTimeinHHMMSS, getCurrentUTC, convertUTCtoISC, getNewIDWithTag)
import Resources.Constants
import Services.API (TicketPlaceResp(..), TicketServicesResponse(..), BusinessHoursResp(..), TicketServiceResp(..), PeopleCategoriesResp(..), BookingStatus(..), PeopleCategoriesResp(..), TicketCategoriesResp(..), PlaceType(..))
import Data.Int (ceil)
import Domain.Payments as PP
import Screens.TicketBookingFlow.TicketStatus.ScreenData as TicketBookingScreenData
import Data.Function.Uncurried as Uncurried
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import JBridge as JB
import Services.API (ServiceExpiry(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Types.App

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen TICKET_BOOKING_SCREEN)
    _ -> pure unit

data Action = AfterRender
            | ViewTicketAC PrimaryButton.Action
            | NoAction
            | BackPressed
            | GetBookingInfo String BookingStatus
            | PaymentStatusAction String
            | GoHome
            | RefreshStatusAC PrimaryButton.Action
            | Copy String

data ScreenOutput = GoToHomeScreen TicketBookingScreenState
                  | GoToGetBookingInfo TicketBookingScreenState BookingStatus
                  | RefreshPaymentStatus TicketBookingScreenState
                  | GoBack TicketBookingScreenState

eval :: Action -> TicketBookingScreenState -> Eval Action ScreenOutput TicketBookingScreenState

eval BackPressed state = do
  case state.props.currentStage of 
    DescriptionStage -> exit $ GoToHomeScreen state {props {currentStage = DescriptionStage}}
    ChooseTicketStage -> continue state{props{currentStage = if state.props.previousStage == ChooseTicketStage then DescriptionStage else state.props.previousStage}}
    ViewTicketStage -> exit $ GoToHomeScreen state{props{currentStage = DescriptionStage, showShimmer = true}}
    TicketInfoStage -> continue state{props{currentStage = ViewTicketStage}}
    BookingConfirmationStage -> if state.props.previousStage == ViewTicketStage then exit $ GoBack state{props{currentStage = MyTicketsStage}}
                                else exit $ GoToHomeScreen state{props{currentStage = DescriptionStage, showShimmer = true}}
    _ -> continue state

eval GoHome state = if state.props.previousStage == ViewTicketStage then exit $ GoBack state{props{currentStage = MyTicketsStage}}
                    else exit $ GoToHomeScreen state{props{currentStage = DescriptionStage, showShimmer = true}}

eval (GetBookingInfo bookingShortId bookingStatus) state = do
  let newState = state { props { selectedBookingId = bookingShortId } }
  updateAndExit newState $ GoToGetBookingInfo newState bookingStatus

eval (ViewTicketAC (PrimaryButton.OnClick)) state = 
  case state.props.paymentStatus of
   PP.Success -> continueWithCmd state [do pure (GetBookingInfo state.props.selectedBookingId Booked)]
   PP.Failed -> if isParentView FunctionCall
                      then do
                          void $ pure $ emitTerminateApp Nothing true
                          continue state
                      else exit $ GoToHomeScreen state
   _ -> continue state

eval (PaymentStatusAction status) state =
  case status of 
    "Booked" -> continue state{props{paymentStatus = PP.Success}}
    "Failed" -> continue state{props{paymentStatus = PP.Failed}}
    _ -> continue state

eval (RefreshStatusAC (PrimaryButton.OnClick)) state = exit $ RefreshPaymentStatus state

eval (Copy text) state = continueWithCmd state [ do 
    void $ pure $ JB.copyToClipboard text
    void $ pure $ EHU.showToast (getString COPIED)
    pure NoAction
  ]

eval _ state = update state