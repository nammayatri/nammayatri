{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketingScreen.Controller where 

import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (TicketingScreenState)
import Storage (KeyStore(..), setValueToLocalStore)
import Components.SettingSideBar as SettingSideBar
import JBridge as JB
import MerchantConfig.Utils as MU
import Engineering.Helpers.Commons as EHC
import Services.API as API

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = HamburgerClick
            | BackPressed
            | MyTicketsAC
            | OnSelect API.TicketPlaceResp
            | NoAction
            | UpdatePlacesData API.TicketPlaceResponse

data ScreenOutput = GoBack
                  | ExitToHomeScreen TicketingScreenState
                  | ExitToMyTicketsScreen TicketingScreenState
                  | BookTickets TicketingScreenState API.TicketPlaceResp
                  | PastRides TicketingScreenState
                  | GoToHelp TicketingScreenState
                  | ChangeLanguage TicketingScreenState
                  | GoToAbout TicketingScreenState
                  | GoToEmergencyContacts TicketingScreenState
                  | GoToMyProfile TicketingScreenState
                  | GoToFavourites TicketingScreenState
                  | GoToMyTickets TicketingScreenState


eval :: Action -> TicketingScreenState -> Eval Action ScreenOutput TicketingScreenState

eval BackPressed state = exit $ ExitToHomeScreen state

eval MyTicketsAC state = exit $ ExitToMyTicketsScreen state

eval (OnSelect item) state = exit $ BookTickets state item

eval (UpdatePlacesData placesData) state = do
  let API.TicketPlaceResponse ticketPlaceResp = placesData
  continue state { data { placeInfoArray = ticketPlaceResp} }

eval _ state = continue state
