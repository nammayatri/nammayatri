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
import Services.API as API
import Data.Array
import Data.Maybe
import Debug (spy)
import JBridge (toggleBtnLoader,hideKeyboardOnNavigation)
import Engineering.Helpers.Utils as EHU
import Components.RequestInfoCard as InfoCard
import Language.Strings
import Language.Types
import MerchantConfig.Types (MetroConfig)
import Data.Int as DI
import Helpers.Utils (isParentView, emitTerminateApp)
import Common.Types.App (LazyCheck(..))
import Log(trackAppScreenEvent)


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    ListExpandAinmationEnd -> trackAppScreenEvent appId (getScreen TRIP_DETAILS_SCREEN) "in_screen" "list_expand_animation_end"
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
            | GetMetroQuotesAction (Array FrfsQuote)
            | SelectLocation ST.LocationActionId
            | ShowMetroBookingTimeError Boolean
            | InfoCardAC InfoCard.Action 
            | GetSDKPollingAC CreateOrderRes
            | MetroBookingConfigAction FRFSConfigAPIRes
            | ListExpandAinmationEnd
            | SelectRoutes String String
            | SelectRouteslistView
            | OfferInfoClicked
            | ApplyOffer String
            | UpdatePaymentOption

data ScreenOutput = GoBack ST.MetroTicketBookingScreenState
                  | UpdateAction ST.MetroTicketBookingScreenState
                  | MyMetroTicketScreen
                  | GoToMetroRouteMap ST.MetroTicketBookingScreenState
                  | GoToHome
                  | SelectSrcDest ST.LocationActionId ST.MetroTicketBookingScreenState
                  | Refresh ST.MetroTicketBookingScreenState
                  | GotoPaymentPage CreateOrderRes String ST.MetroTicketBookingScreenState
                  | GotoSearchScreen ST.MetroTicketBookingScreenState
                  -- | GET_ROUTES ST.MetroTicketBookingScreenState
                  | AadhaarVerificationSO ST.MetroTicketBookingScreenState String
                  | EditStops ST.MetroTicketBookingScreenState

eval :: Action -> ST.MetroTicketBookingScreenState -> Eval Action ScreenOutput ST.MetroTicketBookingScreenState

eval OfferInfoClicked state =
  continue state { props { currentStage = ST.OfferSelection }}

eval (ApplyOffer offerType) state =
  let updatedState = state 
        { data 
          { applyDiscounts = 
            Just $ [ API.FRFSDiscountReq
              { code: offerType
              , quantity: 1
              } ] 
          }
        }
  in exit $ AadhaarVerificationSO state offerType

eval (MetroBookingConfigAction resp) state = do
  let updatedState = state { data {metroBookingConfigResp = resp}, props { showShimmer = false }}
  if (state.props.currentStage == ST.BusTicketSelection )
    then continueWithCmd updatedState [ do pure UpdatePaymentOption]
    else continue updatedState

eval BackPressed state = 
  if isParentView FunctionCall && state.props.ticketServiceType == API.METRO
      then do
        void $ pure $ emitTerminateApp Nothing true
        continue state
        else if state.props.ticketServiceType == API.BUS 
                then 
                  case state.props.currentStage of
                    ST.OfferSelection -> continue state { props { currentStage = ST.ConfirmMetroQuote }}
                    _ -> exit $ GotoSearchScreen state 
                else exit $ GoToHome

eval UpdatePaymentOption state = do
  if state.props.ticketServiceType == API.BUS && state.props.routeName == "" then do
    continue state 
  else updateAndExit state $ UpdateAction state

eval (UpdateButtonAction (PrimaryButton.OnClick)) state = do
    if state.props.ticketServiceType == API.BUS && state.props.routeName == "" then do
     void $ pure $ EHU.showToast $ "Please Select Route"
     void $ pure $ toggleBtnLoader "" false
     continue state 
    else updateAndExit state $ UpdateAction state

eval MyMetroTicketAction state = exit $ MyMetroTicketScreen

eval IncrementTicket state = do
  let (FRFSConfigAPIRes metroBookingConfigResp) = state.data.metroBookingConfigResp
      ticketLimit = if state.data.ticketType == ST.ROUND_TRIP_TICKET then metroBookingConfigResp.roundTripTicketLimit else metroBookingConfigResp.oneWayTicketLimit
  if state.data.ticketCount < ticketLimit
    then continueWithCmd state { data {ticketCount = state.data.ticketCount + 1, applyDiscounts = Nothing, discounts = []}, props {currentStage  = if state.props.ticketServiceType == BUS then ST.BusTicketSelection else  ST.MetroTicketSelection}} [do pure UpdatePaymentOption]
    else continue state

eval DecrementTicket state = do
  if state.data.ticketCount > 1
    then continueWithCmd state { data {ticketCount = state.data.ticketCount - 1, applyDiscounts = Nothing, discounts = []}, props {currentStage  = if state.props.ticketServiceType == BUS then ST.BusTicketSelection else  ST.MetroTicketSelection}} [do pure UpdatePaymentOption]
    else continue state

eval MetroRouteMapAction state = exit $ GoToMetroRouteMap state

eval (ChangeTicketTab ticketType cityMetroConfig) state = do 
  let (FRFSConfigAPIRes metroBookingConfigResp) = state.data.metroBookingConfigResp
  if state.props.currentStage == ST.ConfirmMetroQuote then do
    let ticketTypeUpdatedState = state {data {ticketType = ticketType}}
    updateQuotes state.data.quoteResp state
  else do
    let updatedTicketCount = case ticketType of
          ST.ONE_WAY_TICKET -> if state.data.ticketCount > metroBookingConfigResp.oneWayTicketLimit then metroBookingConfigResp.oneWayTicketLimit else state.data.ticketCount
          ST.ROUND_TRIP_TICKET -> if state.data.ticketCount > metroBookingConfigResp.roundTripTicketLimit then metroBookingConfigResp.roundTripTicketLimit else state.data.ticketCount
    continue state { data {ticketType = ticketType, ticketCount = updatedTicketCount}, props {currentStage  = if state.props.ticketServiceType == BUS then ST.BusTicketSelection else  ST.MetroTicketSelection}}

eval (SelectLocation loc) state = 
  if state.props.isRepeatRide then continue state
  else if state.props.ticketServiceType == BUS
  then exit $ EditStops state
  else updateAndExit state { props { currentStage  = ST.MetroTicketSelection }} $ SelectSrcDest loc state { props { currentStage = ST.MetroTicketSelection }}

eval (GetMetroQuotesAction resp) state = do 
  void $ pure $ toggleBtnLoader "" false
  if null resp then do
    void $ pure $ EHU.showToast $ getString NO_QOUTES_AVAILABLE
    continue state{ props{currentStage  = if state.props.ticketServiceType == BUS then ST.BusTicketSelection else  ST.MetroTicketSelection}}
  else updateQuotes resp state

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (ShowMetroBookingTimeError withinTimeRange) state = 
  continue state {
    props{
      showMetroBookingTimeError = if state.props.ticketServiceType == BUS then false else not withinTimeRange
    }
  }

eval (InfoCardAC (InfoCard.Close)) state = 
  exit $ GoBack state { 
    props { 
      showMetroBookingTimeError = false
    }
  }
eval ListExpandAinmationEnd state = continue state {props {showRouteOptions = false }}
eval SelectRouteslistView state = do
--  if state.props.isEmptyRoute == "" then do exit $ GET_ROUTES state
--  else do 
    let old = state.props.routeList
    _ <- pure $ hideKeyboardOnNavigation true
    
    continue state{props{routeList = not old , showRouteOptions = true}}
  -- updateAndExit state{props{routeList = not old , showRouteOptions = true}} $ SearchRoute state
eval (SelectRoutes route routeName) state =
  continueWithCmd 
    state{props{isEmptyRoute = route ,routeName = routeName , routeList = not state.props.routeList , showRouteOptions = false,currentStage = ST.BusTicketSelection, isButtonActive = false}}
    [ do pure UpdatePaymentOption ]

eval (GetSDKPollingAC createOrderRes) state = exit $ GotoPaymentPage createOrderRes state.data.bookingId state

eval _ state = update state

updateQuotes :: (Array FrfsQuote) -> ST.MetroTicketBookingScreenState -> Eval Action ScreenOutput ST.MetroTicketBookingScreenState
updateQuotes quotes state = do
  let quoteData = find (\(FrfsQuote item) -> (getTicketType item._type) == (state.data.ticketType)) quotes
  case quoteData of
    Nothing -> do
      void $ pure $ EHU.showToast $ getString NO_QOUTES_AVAILABLE
      continue state { props {currentStage  = if state.props.ticketServiceType == API.BUS then ST.BusTicketSelection else  ST.MetroTicketSelection}}
    Just (FrfsQuote quoteData) -> do
      let updatedState = state { data {discounts = fromMaybe [] quoteData.discounts, ticketPrice = quoteData.price, quoteId = quoteData.quoteId, quoteResp = quotes, eventDiscountAmount = DI.round <$> quoteData.eventDiscountAmount}, props { currentStage = ST.ConfirmMetroQuote, isButtonActive = (quoteData.quantity == state.data.ticketCount) }}
      updateAndExit updatedState $ Refresh updatedState
  where
    getTicketType :: String -> ST.TicketType
    getTicketType quoteType = case quoteType of 
      "SingleJourney" -> ST.ONE_WAY_TICKET
      "ReturnJourney" -> ST.ROUND_TRIP_TICKET
      _ -> ST.ONE_WAY_TICKET
