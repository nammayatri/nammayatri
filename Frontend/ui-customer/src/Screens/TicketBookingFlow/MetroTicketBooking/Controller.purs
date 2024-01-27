module Screens.TicketBookingFlow.MetroTicketBooking.Controller where

import Prelude
import Screens.Types as ST
import Components.GenericHeader as GenericHeader
-- import Components.SelectionTabModal as SelectionTabModal
import Components.PrimaryEditText as PrimaryEditText
-- import Components.IncrementDecrementModel as IncrementDecrementModel
-- import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
-- import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryButton as PrimaryButton
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==))
import PrestoDOM
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Services.API
import Data.Array
import Data.Maybe
import Debug (spy)
import JBridge (toast, toggleBtnLoader)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit

data Action = AfterRender
            | BackPressed
            | GenericHeaderAC GenericHeader.Action
            | SourceEditText PrimaryEditText.Action
            | DestinationEditText PrimaryEditText.Action
            | UpdateButtonAction PrimaryButton.Action
            | MyMetroTicketAction
            | ChangeTicketTab ST.TicketType
            | IncrementTicket
            | DecrementTicket
            | MetroRouteMapAction
            | ToggleTermsAndConditions
            | SelectSource String
            | SelectDestination String
            | GetMetroQuotesAction (Array MetroQuote)

data ScreenOutput = GoBack ST.MetroTicketBookingScreenState
                  | UpdateAction ST.MetroTicketBookingScreenState
                  | MyMetroTicketScreen ST.MetroTicketBookingScreenState
                  | GoToMetroRouteMap
                  | SelectSrcDest ST.MetroTicketBookingScreenState String
                  | Refresh ST.MetroTicketBookingScreenState
                  -- | SelectDest ST.MetroTicketBookingScreenState

eval :: Action -> ST.MetroTicketBookingScreenState -> Eval Action ScreenOutput ST.MetroTicketBookingScreenState

eval BackPressed state =  exit $ GoBack state
eval (UpdateButtonAction (PrimaryButton.OnClick)) state = do
    updateAndExit state $ UpdateAction state

eval MyMetroTicketAction state = exit $ MyMetroTicketScreen state

eval IncrementTicket state = do
  if state.data.ticketCount < 6
    then continue state { data {ticketCount = state.data.ticketCount + 1 }}
    else continue state

eval DecrementTicket state = do
  if state.data.ticketCount > 1
    then continue state { data {ticketCount = state.data.ticketCount - 1 }}
    else continue state

eval MetroRouteMapAction state = exit $ GoToMetroRouteMap

eval ToggleTermsAndConditions state = continue state{props{termsAndConditionsSelected = not state.props.termsAndConditionsSelected}}

eval (ChangeTicketTab ticketType) state = continue state { data {ticketType = ticketType }}

eval (SelectSource src) state = updateAndExit state $ SelectSrcDest state src

eval (SelectDestination dest ) state = updateAndExit state $ SelectSrcDest state dest

eval (GetMetroQuotesAction resp) state = do 
  _ <- pure $ spy "GetMetroQuotesAction" state
  _ <- pure $ spy "GetMetroQuotesActionquoteId"  $ (getquoteData state resp).quoteId
  _ <- pure $ spy "GetMetroQuotesActionprice"  $ (getquoteData state resp).price
  _ <- pure $ spy "GetMetroQuotesActionresp" resp
  _ <- pure $ toggleBtnLoader "" false
  updateAndExit state { data {ticketPrice = (getquoteData state resp).price, quoteId = (getquoteData state resp).quoteId }, props { currentStage = ST.ConfirmMetroQuote}} $ Refresh state { data {ticketPrice = (getquoteData state resp).price, quoteId = (getquoteData state resp).quoteId }, props { currentStage = ST.ConfirmMetroQuote}}

eval _ state = continue state


getquoteData :: ST.MetroTicketBookingScreenState -> Array MetroQuote -> {"price" :: Int, "quoteId" :: String}--Int
getquoteData state  metroQuote =
  let quote = filter (\(MetroQuote item) -> (getTicketType item._type) == (state.data.ticketType)) metroQuote
      quoteData = quote !! 0
  in
    {
      "price": maybe 0 (\(MetroQuote item) -> item.price) quoteData,
      "quoteId" : maybe "" (\(MetroQuote item) -> item.quoteId) quoteData
    }
  where
    getTicketType :: String -> ST.TicketType
    getTicketType str = case str of 
      "SingleJourney" -> ST.ONE_WAY
      "ReturnJourney" -> ST.ROUND_TRIP
      "Pass" -> ST.ONE_WAY
      _ -> ST.ONE_WAY