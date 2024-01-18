{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketBooking.Controller where

import Prelude
import Screens.Types as ST
import Components.GenericHeader as GenericHeader
import Components.PrimaryEditText as PrimaryEditText
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
            | UpdateButtonAction PrimaryButton.Action
            | MyMetroTicketAction
            | ChangeTicketTab ST.TicketType
            | IncrementTicket
            | DecrementTicket
            | MetroRouteMapAction
            | ToggleTermsAndConditions
            | GetMetroQuotesAction (Array MetroQuote)
            | SelectLocation ST.LocationActionId

data ScreenOutput = GoBack ST.MetroTicketBookingScreenState
                  | UpdateAction ST.MetroTicketBookingScreenState
                  | MyMetroTicketScreen
                  | GoToMetroRouteMap
                  | SelectSrcDest ST.LocationActionId
                  | Refresh ST.MetroTicketBookingScreenState

eval :: Action -> ST.MetroTicketBookingScreenState -> Eval Action ScreenOutput ST.MetroTicketBookingScreenState

eval BackPressed state =  exit $ GoBack state
eval (UpdateButtonAction (PrimaryButton.OnClick)) state = do
    updateAndExit state $ UpdateAction state

eval MyMetroTicketAction state = exit $ MyMetroTicketScreen

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

eval (SelectLocation loc ) state = exit $ SelectSrcDest loc

eval (GetMetroQuotesAction resp) state = do 
  if null resp then do
    _ <- pure $ toast "No quotes available"
    _ <- pure $ toggleBtnLoader "" false
    continue state
  else do
    _ <- pure $ spy "GetMetroQuotesActionresp" resp
    _ <- pure $ toggleBtnLoader "" false
    updateAndExit state { data {ticketPrice = (getquoteData state resp).price, quoteId = (getquoteData state resp).quoteId }, props { currentStage = ST.ConfirmMetroQuote}} $ Refresh state { data {ticketPrice = (getquoteData state resp).price, quoteId = (getquoteData state resp).quoteId }, props { currentStage = ST.ConfirmMetroQuote}}

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

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
    getTicketType quoteType = case quoteType of 
      "SingleJourney" -> ST.ONE_WAY
      "ReturnJourney" -> ST.ROUND_TRIP
      _ -> ST.ONE_WAY
