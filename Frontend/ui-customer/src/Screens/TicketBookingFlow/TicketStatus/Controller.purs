module Screens.TicketBookingFlow.TicketStatus.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude 
import PrestoDOM (Eval, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketStatusScreenState, TicketBookingScreenStage(..), TicketServiceI(..))
import Helpers.Utils (getDateAfterNDaysv2, compareDate, getCurrentDatev2)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types (TimeInterval, TicketStatusScreenState, TicketBookingItem(..), HomeScreenState, TicketServiceData, PeopleCategoriesRespData, TicketPeopleCategoriesOptionData, PeopleCategoriesRespData, BusinessHoursData, TicketCategoriesData, TicketCategoriesData, TicketCategoriesOptionData, SlotsAndTimeIntervalData(..), SlotInterval(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
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
import JBridge as JB
import Services.API (ServiceExpiry(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types as ST

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

data ScreenOutput = GoToHomeScreen TicketStatusScreenState
                  | GoToGetBookingInfo TicketStatusScreenState BookingStatus
                  | RefreshPaymentStatus TicketStatusScreenState
                  | GoBack TicketStatusScreenState

eval :: Action -> TicketStatusScreenState -> Eval Action ScreenOutput TicketStatusScreenState

eval BackPressed state = 
  case state.props.actionType of
    ST.MetroTicketToPaymentStatusEntry -> 
      continue state -- Handle Metro Ticket Back Press Flow
    ST.ZooTicketToPaymentStatusEntry -> 
      case state.props.currentStage of 
        DescriptionStage -> 
          exit $ GoToHomeScreen 
            state {
              props {
                currentStage = DescriptionStage
              }
            }
        ChooseTicketStage -> 
          continue 
            state {
              props{
                currentStage = case state.props.previousStage of
                  ChooseTicketStage -> DescriptionStage
                  _ -> state.props.previousStage
              }
            }
        ViewTicketStage -> 
          exit $ GoToHomeScreen 
            state {
              props {
                currentStage = DescriptionStage
              , showShimmer = true
              }
            }
        TicketInfoStage -> 
          continue 
            state {
              props {
                currentStage = ViewTicketStage
              }
            }
        BookingConfirmationStage -> 
          case state.props.previousStage of
            ViewTicketStage ->  
              exit $ GoBack 
                state {
                  props {
                    currentStage = MyTicketsStage
                  }
                }
            _ -> 
              exit $ GoToHomeScreen state {
                props {
                  currentStage = DescriptionStage
                , showShimmer = true
                }
              }
        _ -> continue state

eval GoHome state = 
  case state.props.previousStage of 
    ViewTicketStage ->
      exit $ GoBack 
        state{
          props{
            currentStage = MyTicketsStage
          }
        }
    _ -> 
      exit $ GoToHomeScreen state{
        props{
          currentStage = DescriptionStage
        , showShimmer = true
        }
      }

eval (GetBookingInfo bookingShortId bookingStatus) state = do
  let newState = state { 
                   props { 
                     selectedBookingId = bookingShortId
                   }
                 }
  updateAndExit newState $ GoToGetBookingInfo newState bookingStatus

eval (ViewTicketAC (PrimaryButton.OnClick)) state = 
  case state.props.actionType of 
    ST.MetroTicketToPaymentStatusEntry -> 
      continue state -- Handle Metro Ticket View Flow
    ST.ZooTicketToPaymentStatusEntry ->    
      case state.props.paymentStatus of
        PP.Success -> continueWithCmd state [do pure (GetBookingInfo state.props.selectedBookingId Booked)]
        PP.Failed -> exit $ GoToHomeScreen state
        _ -> continue state

eval (PaymentStatusAction status) state =
  case status of 
    "Booked" -> 
      continue 
        state{
          props{
            paymentStatus = PP.Success
          }
        }
    "Failed" -> 
      continue 
        state{
          props{
            paymentStatus = PP.Failed
          }
        }
    _ -> continue state

eval (RefreshStatusAC (PrimaryButton.OnClick)) state = exit $ RefreshPaymentStatus state

eval (Copy text) state = 
  continueWithCmd state [ do 
    void $ pure $ JB.copyToClipboard text
    void $ pure $ JB.toast (getString COPIED)
    pure NoAction
  ]

eval _ state = continue state

