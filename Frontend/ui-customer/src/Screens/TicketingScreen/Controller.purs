{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketingScreen.Controller where 

import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (TicketingScreenState)
import Storage (KeyStore(..), setValueToLocalStore)
import Components.SettingSideBar as SettingSideBar
import Components.NavBar as NavBar
import JBridge as JB
import MerchantConfig.Utils as MU
import Engineering.Helpers.Commons as EHC
import Services.API as API

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = SettingSideBarActionController SettingSideBar.Action
            | HamburgerClick
            | BackPressed
            | NavBarAC NavBar.Action
            | MyTicketsAC
            | OnSelect Int
            | NoAction
            | UpdatePlacesData API.TicketPlaceResponse

data ScreenOutput = GoBack
                  | ExitToHomeScreen TicketingScreenState
                  | ExitToMyTicketsScreen TicketingScreenState
                  | BookTickets TicketingScreenState
                  | PastRides TicketingScreenState
                  | GoToHelp TicketingScreenState
                  | ChangeLanguage TicketingScreenState
                  | GoToAbout TicketingScreenState
                  | GoToEmergencyContacts TicketingScreenState
                  | GoToMyProfile TicketingScreenState
                  | GoToFavourites TicketingScreenState
                  | GoToMyTickets TicketingScreenState


eval :: Action -> TicketingScreenState -> Eval Action ScreenOutput TicketingScreenState

eval BackPressed state = do -- exit $ GoBack
  _ <- pure $ JB.minimizeApp ""
  continue state

eval (SettingSideBarActionController (SettingSideBar.OnClosed)) state = continue state{ data{sideBarStatus = SettingSideBar.CLOSED}}

eval HamburgerClick state = continue state {data{sideBarStatus = SettingSideBar.OPEN}}

eval MyTicketsAC state = exit $ ExitToMyTicketsScreen state

eval (SettingSideBarActionController (SettingSideBar.OnClose)) state =
 case state.data.sideBarStatus of
    SettingSideBar.CLOSED -> continue state
    _ -> continue state {data{sideBarStatus = SettingSideBar.CLOSING}}

eval (SettingSideBarActionController act) state = do
  let newState = state { data { sideBarStatus = SettingSideBar.OPEN } }
  case act of
    SettingSideBar.PastRides -> exit $ PastRides newState
    SettingSideBar.OnHelp -> exit $ GoToHelp newState
    SettingSideBar.ChangeLanguage -> exit $ ChangeLanguage newState
    SettingSideBar.GoToAbout -> exit $ GoToAbout newState
    SettingSideBar.GoToEmergencyContacts -> exit $ GoToEmergencyContacts newState
    SettingSideBar.ShareAppLink -> do
      _ <- pure $ JB.shareTextMessage (MU.getValueFromConfig "shareAppTitle") (MU.getValueFromConfig "shareAppContent")
      continue state
    SettingSideBar.EditProfile -> exit $ GoToMyProfile newState
    SettingSideBar.OnLogout ->  continue state
    SettingSideBar.GoToFavourites -> exit $ GoToFavourites newState
    SettingSideBar.GoToMyTickets -> exit $ GoToMyTickets newState
    SettingSideBar.GoToMyProfile -> exit $ GoToMyProfile newState
    SettingSideBar.LiveStatsDashboard -> continue state
    _ -> continue state

eval (NavBarAC (NavBar.NavigateTo index)) state = 
  case index of 
    0 -> exit $ ExitToHomeScreen state
    _ -> continue state

eval (OnSelect index) state = exit $ BookTickets state

eval (UpdatePlacesData placesData) state = do
  let API.TicketPlaceResponse ticketPlaceResp = placesData
  continue state { data { placeInfoArray = ticketPlaceResp} }

eval _ state = continue state
