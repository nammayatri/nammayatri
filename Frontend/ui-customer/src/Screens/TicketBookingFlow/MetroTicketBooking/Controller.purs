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
import Components.RequestInfoCard as InfoCard
import Language.Strings
import Language.Types
import MerchantConfig.Types (MetroConfig)
import Data.Int as DI
import Helpers.Utils (isParentView, emitTerminateApp)
import Common.Types.App (LazyCheck(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit

data Action = BackPressed
            | NoAction
            | GenericHeaderAC GenericHeader.Action
            | UpdateButtonAction PrimaryButton.Action
            | MyMetroTicketAction
            | ChangeTicketTab ST.TicketType MetroConfig 
            | IncrementTicket
            | DecrementTicket
            | MetroRouteMapAction
            | GetMetroQuotesAction (Array MetroQuote)
            | SelectLocation ST.LocationActionId
            | ShowMetroBookingTimeError Boolean
            | InfoCardAC InfoCard.Action 
            | GetSDKPollingAC CreateOrderRes
            | MetroBookingConfigAction MetroBookingConfigRes

data ScreenOutput = GoBack ST.MetroTicketBookingScreenState
                  | UpdateAction ST.MetroTicketBookingScreenState
                  | MyMetroTicketScreen
                  | GoToMetroRouteMap
                  | GoToHome
                  | SelectSrcDest ST.LocationActionId ST.MetroTicketBookingScreenState
                  | Refresh ST.MetroTicketBookingScreenState
                  | GotoPaymentPage CreateOrderRes String

eval :: Action -> ST.MetroTicketBookingScreenState -> Eval Action ScreenOutput ST.MetroTicketBookingScreenState

eval (MetroBookingConfigAction resp) state = do
  let updatedState = state { data {metroBookingConfigResp = resp}, props { showShimmer = false }}
  continue updatedState

eval BackPressed state = 
  if isParentView FunctionCall
      then do
        void $ pure $ emitTerminateApp Nothing true
        continue state
        else 
            exit $ GoToHome

eval (UpdateButtonAction (PrimaryButton.OnClick)) state = do
    updateAndExit state $ UpdateAction state

eval MyMetroTicketAction state = exit $ MyMetroTicketScreen

eval IncrementTicket state = do
  if state.data.ticketCount < 6
    then continue state { data {ticketCount = state.data.ticketCount + 1 }, props {currentStage  = ST.MetroTicketSelection}}
    else continue state

eval DecrementTicket state = do
  if state.data.ticketCount > 1
    then continue state { data {ticketCount = state.data.ticketCount - 1 }, props {currentStage  = ST.MetroTicketSelection}}
    else continue state

eval MetroRouteMapAction state = exit $ GoToMetroRouteMap

eval (ChangeTicketTab ticketType cityMetroConfig) state = do 
  let (MetroBookingConfigRes metroBookingConfigResp) = state.data.metroBookingConfigResp
  if state.props.currentStage == ST.ConfirmMetroQuote then do
    let ticketTypeUpdatedState = state {data {ticketType = ticketType}}
    updateQuotes state.data.quoteResp state
  else do
    let updatedTicketCount = case ticketType of
          ST.ONE_WAY_TICKET -> if state.data.ticketCount > metroBookingConfigResp.oneWayTicketLimit then metroBookingConfigResp.oneWayTicketLimit else state.data.ticketCount
          ST.ROUND_TRIP_TICKET -> if state.data.ticketCount > metroBookingConfigResp.roundTripTicketLimit then metroBookingConfigResp.roundTripTicketLimit else state.data.ticketCount
    continue state { data {ticketType = ticketType, ticketCount = updatedTicketCount}, props {currentStage  = ST.MetroTicketSelection}}

eval (SelectLocation loc ) state = updateAndExit state{props{currentStage  = ST.MetroTicketSelection}} $ SelectSrcDest loc state{props{currentStage  = ST.MetroTicketSelection}}

eval (GetMetroQuotesAction resp) state = do 
  void $ pure $ toggleBtnLoader "" false
  if null resp then do
    void $ pure $ toast $ getString NO_QOUTES_AVAILABLE
    continue state{ props{currentStage = ST.MetroTicketSelection}}
  else updateQuotes resp state

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (ShowMetroBookingTimeError withinTimeRange) state = 
  continue state {
    props{
      showMetroBookingTimeError = not withinTimeRange
    }
  }

eval (InfoCardAC (InfoCard.Close)) state = 
  exit $ GoBack state { 
    props { 
      showMetroBookingTimeError = false
    }
  }

eval (GetSDKPollingAC createOrderRes) state = exit $ GotoPaymentPage createOrderRes state.data.bookingId

eval _ state = update state

updateQuotes :: (Array MetroQuote) -> ST.MetroTicketBookingScreenState -> Eval Action ScreenOutput ST.MetroTicketBookingScreenState
updateQuotes quotes state = do
  let quoteData = find (\(MetroQuote item) -> (getTicketType item._type) == (state.data.ticketType)) quotes
  case quoteData of
    Nothing -> do
      void $ pure $ toast $ getString NO_QOUTES_AVAILABLE
      continue state { props {currentStage = ST.MetroTicketSelection}}
    Just (MetroQuote quoteData) -> do
      let updatedState = state { data {ticketPrice = quoteData.price, quoteId = quoteData.quoteId, quoteResp = quotes, eventDiscountAmount = DI.round <$> quoteData.eventDiscountAmount}, props { currentStage = ST.ConfirmMetroQuote}}
      updateAndExit updatedState $ Refresh updatedState
  where
    getTicketType :: String -> ST.TicketType
    getTicketType quoteType = case quoteType of 
      "SingleJourney" -> ST.ONE_WAY_TICKET
      "ReturnJourney" -> ST.ROUND_TRIP_TICKET
      _ -> ST.ONE_WAY_TICKET
